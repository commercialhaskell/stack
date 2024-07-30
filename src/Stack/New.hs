{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

-- | Types and functions related to Stack's @new@ command.
module Stack.New
  ( NewOpts (..)
  , TemplateName
  , newCmd
  , new
  ) where

import           Control.Monad.Extra ( whenJust )
import           Control.Monad.Trans.Writer.Strict ( execWriterT )
import           Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Base64 as B64
import           Data.ByteString.Builder ( lazyByteString )
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit ( yield )
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Calendar ( toGregorian )
import           Data.Time.Clock ( getCurrentTime, utctDay )
import           Network.HTTP.Client ( applyBasicAuth )
import           Network.HTTP.StackClient
                   ( HttpException (..), HttpExceptionContent (..)
                   , Response (..), VerifiedDownloadException (..)
                   , mkDownloadRequest, notFound404, parseRequest
                   , setForceDownload, setRequestCheckStatus
                   , verifiedDownloadWithProgress
                   )
import           Path ( (</>), dirname, parent, parseRelDir, parseRelFile )
import           Path.IO
                   ( doesDirExist, doesFileExist, ensureDir, getCurrentDir )
import           RIO.Process ( proc, runProcess_, withWorkingDir )
import           Stack.Constants
                   ( altGitHubTokenEnvVar, backupUrlRelPath, gitHubBasicAuthType
                   , gitHubTokenEnvVar, stackDotYaml, wiredInPackages
                   )
import           Stack.Constants.Config ( templatesDir )
import           Stack.Init ( InitOpts (..), initProject )
import           Stack.Prelude
import           Stack.Runners
                   ( ShouldReexec (..), withConfig, withGlobalProject )
import           Stack.Types.Config ( Config (..), HasConfig (..) )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.Runner ( Runner, globalOptsL )
import           Stack.Types.SCM ( SCM (..) )
import           Stack.Types.TemplateName
                   ( RepoService (..), RepoTemplatePath (..), TemplateName
                   , TemplatePath (..), defaultTemplateName
                   , parseRepoPathWithService, templateName, templatePath
                   )
import           System.Environment ( lookupEnv )
import qualified Text.Mustache as Mustache
import qualified Text.Mustache.Render as Mustache
import           Text.ProjectTemplate
                   ( ProjectTemplateException, receiveMem, unpackTemplate )

--------------------------------------------------------------------------------
-- Exceptions

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.New" module.
data NewPrettyException
  = ProjectDirAlreadyExists !String !(Path Abs Dir)
  | DownloadTemplateFailed !Text !String !VerifiedDownloadException
  | forall b. LoadTemplateFailed !TemplateName !(Path b File)
  | forall b. ExtractTemplateFailed !TemplateName !(Path b File) !String
  | TemplateInvalid !TemplateName !StyleDoc
  | MagicPackageNameInvalid !String
  | AttemptedOverwrites !Text ![Path Abs File]
  deriving Typeable

deriving instance Show NewPrettyException

instance Pretty NewPrettyException where
  pretty (ProjectDirAlreadyExists name path) =
    "[S-2135]"
    <> line
    <> fillSep
         [ flow "Stack failed to create a new directory for project"
         , style Current (fromString name) <> ","
         , flow "as the directory"
         , pretty path
         , flow "already exists."
         ]
  pretty (DownloadTemplateFailed name url err) =
    "[S-1688]"
    <> line
    <> fillSep
         [ flow "Stack failed to download the template"
         , style Current (fromString . T.unpack $ name)
         , "from"
         , style Url (fromString url) <> "."
         ]
    <> blankLine
    <> ( if isNotFound
            then    flow "Please check that the template exists at that \
                         \location."
                 <> blankLine
            else mempty
       )
    <> fillSep
         [ flow "While downloading, Stack encountered"
         , msg
         ]
   where
    (msg, isNotFound) = case err of
      DownloadHttpError (HttpExceptionRequest req content) ->
        let msg' =    flow "an HTTP error. Stack made the request:"
                   <> blankLine
                   <> string (show req)
                   <> blankLine
                   <> flow "and the content of the error was:"
                   <> blankLine
                   <> string (show content)
            isNotFound404 = case content of
                              StatusCodeException res _ ->
                                responseStatus res == notFound404
                              _ -> False
        in  (msg', isNotFound404)
      DownloadHttpError (InvalidUrlException url' reason) ->
        let msg' = fillSep
                     [ flow "an HTTP error. The URL"
                     , style Url (fromString url')
                     , flow "was considered invalid because"
                     , fromString reason <> "."
                     ]
        in  (msg', False)
      _ -> let msg' =    flow "the following error:"
                      <> blankLine
                      <> fromString (displayException err)
           in  (msg', False)
  pretty (LoadTemplateFailed name path) =
    "[S-3650]"
    <> line
    <> fillSep
         [ flow "Stack failed to load the downloaded template"
         , style Current (fromString $ T.unpack $ templateName name)
         , "from"
         , pretty path <> "."
         ]
  pretty (ExtractTemplateFailed name path err) =
    "[S-9582]"
    <> line
    <> fillSep
         [ flow "Stack failed to extract the loaded template"
         , style Current (fromString $ T.unpack $ templateName name)
         , "at"
         , pretty path <> "."
         ]
    <> blankLine
    <> flow "While extracting, Stack encountered the following error:"
    <> blankLine
    <> string err
  pretty (TemplateInvalid name why) =
    "[S-9490]"
    <> line
    <> fillSep
         [ flow "Stack failed to use the template"
         , style Current (fromString $ T.unpack $ templateName name) <> ","
         , "as"
         , why
         ]
  pretty (MagicPackageNameInvalid name) =
    "[S-5682]"
    <> line
    <> fillSep
         [ flow "Stack declined to create a new directory for project"
         , style Current (fromString name) <> ","
         , flow "as package"
         , fromString name
         , flow "is 'wired-in' to a version of GHC. That can cause build \
                \errors."
         ]
    <> blankLine
    <> fillSep
         ( flow "The names blocked by Stack are:"
         : mkNarrativeList Nothing False
             (map fromPackageName sortedWiredInPackages :: [StyleDoc])
         )
   where
    sortedWiredInPackages = L.sort $ S.toList wiredInPackages
  pretty (AttemptedOverwrites name fps) =
    "[S-3113]"
    <> line
    <> fillSep
         [ flow "Stack declined to apply the template"
         , style Current (fromString . T.unpack $ name) <> ","
         , flow "as it would create files that already exist."
         ]
    <> blankLine
    <> flow "The template would create the following existing files:"
    <> line
    <> bulletedList (map (style File . pretty) fps)
    <> blankLine
    <> fillSep
         [ "Use the"
         , style Shell "--force"
         , "flag to ignore this and overwrite those files."
         ]

instance Exception NewPrettyException

--------------------------------------------------------------------------------
-- Main project creation

-- | Type representing command line options for the @stack new@ command (other
-- than those applicable also to the @stack init@ command).
data NewOpts = NewOpts
  { projectName  :: PackageName
    -- ^ Name of the project to create.
  , createBare   :: Bool
    -- ^ Whether to create the project without a directory.
  , init         :: Bool
    -- ^ Whether to initialise the project for use with Stack.
  , template     :: Maybe TemplateName
    -- ^ Name of the template to use.
  , nonceParams  :: Map Text Text
    -- ^ Nonce parameters specified just for this invocation.
  }

-- | Function underlying the @stack new@ command. Create a project directory
-- structure and initialize the Stack config.
newCmd :: (NewOpts, InitOpts) -> RIO Runner ()
newCmd (newOpts, initOpts) =
  withGlobalProject $ withConfig YesReexec $ do
    dir <- new newOpts initOpts.forceOverwrite
    exists <- doesFileExist $ dir </> stackDotYaml
    when (newOpts.init && (initOpts.forceOverwrite || not exists)) $ do
      go <- view globalOptsL
      initProject dir initOpts go.snapshot

-- | Create a new project with the given options.
new :: HasConfig env => NewOpts -> Bool -> RIO env (Path Abs Dir)
new opts forceOverwrite = do
  when (project `elem` wiredInPackages) $
      prettyThrowM $ MagicPackageNameInvalid projectName
  pwd <- getCurrentDir
  absDir <- if bare
              then pure pwd
              else do relDir <- parseRelDir (packageNameString project)
                      pure (pwd </> relDir)
  exists <- doesDirExist absDir
  configTemplate <- view $ configL . to (.defaultTemplate)
  let template = fromMaybe defaultTemplateName $ asum [ cliOptionTemplate
                                                      , configTemplate
                                                      ]
  if exists && not bare
    then prettyThrowM $ ProjectDirAlreadyExists projectName absDir
    else do
      templateText <- loadTemplate template (logUsing absDir template)
      files <-
        applyTemplate
          project
          template
          opts.nonceParams
          absDir
          templateText
      when (not forceOverwrite && bare) $
        checkForOverwrite (templateName template) (M.keys files)
      writeTemplateFiles files
      runTemplateInits absDir
      pure absDir
 where
  cliOptionTemplate = opts.template
  project = opts.projectName
  projectName = packageNameString project
  bare = opts.createBare
  logUsing absDir template templateFrom =
    let loading = case templateFrom of
                    LocalTemp -> flow "Loading local"
                    RemoteTemp -> "Downloading"
    in  prettyInfo
          ( fillSep
              [ loading
              , "template"
              , style
                  Current
                  (fromString $ T.unpack $ templateName template)
              , flow "to create project"
              , style Current (fromString projectName)
              , "in"
              ,    ( if bare
                       then flow "the current directory"
                       else fillSep
                              [ "directory"
                              , pretty $ dirname absDir
                              ]
                   )
                <> "..."
              ]
            )

data TemplateFrom = LocalTemp | RemoteTemp

-- | Download and read in a template's text content.
loadTemplate ::
     forall env. HasConfig env
  => TemplateName
  -> (TemplateFrom -> RIO env ())
  -> RIO env Text
loadTemplate name logIt = do
  templateDir <- view $ configL . to templatesDir
  case templatePath name of
    AbsPath absFile ->
      logIt LocalTemp >> loadLocalFile absFile eitherByteStringToText
    UrlPath s -> do
      let settings = asIsFromUrl s
      downloadFromUrl settings templateDir
    RelPath rawParam relFile ->
      catch
        (do f <- loadLocalFile relFile eitherByteStringToText
            logIt LocalTemp
            pure f)
        ( \(e :: PrettyException) -> do
            settings <- fromMaybe (throwM e) (relSettings rawParam)
            let url = settings.downloadUrl
                mBasicAuth = settings.basicAuth
                extract = settings.extract
            downloadTemplate url mBasicAuth extract (templateDir </> relFile)
        )
    RepoPath rtp -> do
      settings <- settingsFromRepoTemplatePath rtp
      downloadFromUrl settings templateDir

 where
  loadLocalFile :: Path b File
                -> (ByteString -> Either String Text)
                -> RIO env Text
  loadLocalFile path extract = do
    logDebug $
         "Opening local template: \""
      <> fromString (toFilePath path)
      <> "\""
    exists <- doesFileExist path
    if exists
      then do
        bs <- readFileBinary (toFilePath path) --readFileUtf8 (toFilePath path)
        case extract bs of
          Left err -> prettyThrowM $ ExtractTemplateFailed name path err
          Right template ->
              pure template
      else prettyThrowM $ LoadTemplateFailed name path

  relSettings :: String -> Maybe (RIO env TemplateDownloadSettings)
  relSettings req = do
    rtp <- parseRepoPathWithService defaultRepoService (T.pack req)
    pure (settingsFromRepoTemplatePath rtp)

  downloadFromUrl :: TemplateDownloadSettings -> Path Abs Dir -> RIO env Text
  downloadFromUrl settings templateDir = do
    let url =  settings.downloadUrl
        mBasicAuth = settings.basicAuth
        rel = fromMaybe backupUrlRelPath (parseRelFile url)
    downloadTemplate url mBasicAuth settings.extract (templateDir </> rel)

  downloadTemplate ::
       String
    -> Maybe (ByteString, ByteString)
       -- ^ Optional HTTP \'Basic\' authentication (type, credentials)
    -> (ByteString -> Either String Text)
    -> Path Abs File
    -> RIO env Text
  downloadTemplate url mBasicAuth extract path = do
    req <- parseRequest url
    let authReq = maybe id (uncurry applyBasicAuth) mBasicAuth req
        dReq = setForceDownload True $
                   mkDownloadRequest (setRequestCheckStatus authReq)
    logIt RemoteTemp
    catch
      ( do let label = T.pack $ toFilePath path
           res <- verifiedDownloadWithProgress dReq path label Nothing
           if res
             then logStickyDone ("Downloaded " <> display label <> ".")
             else logStickyDone "Already downloaded."
      )
      (useCachedVersionOrThrow url path)
    loadLocalFile path extract

  useCachedVersionOrThrow :: String
                          -> Path Abs File
                          -> VerifiedDownloadException
                          -> RIO env ()
  useCachedVersionOrThrow url path exception = do
    exists <- doesFileExist path

    if exists
      then
        prettyWarn
          ( flow "Tried to download the template but an error was \
                 \found. Using cached local version. It may not be the \
                 \most recent version though."
          )
      else
        prettyThrowM $ DownloadTemplateFailed (templateName name) url exception

-- | Type representing settings for the download of Stack project templates.
data TemplateDownloadSettings = TemplateDownloadSettings
  { downloadUrl :: String
  , basicAuth :: Maybe (ByteString, ByteString)
    -- ^ Optional HTTP 'Basic' authentication (type, credentials)
  , extract :: ByteString -> Either String Text
  }

eitherByteStringToText :: ByteString -> Either String Text
eitherByteStringToText = mapLeft show . decodeUtf8'

asIsFromUrl :: String -> TemplateDownloadSettings
asIsFromUrl url = TemplateDownloadSettings
  { downloadUrl = url
  , basicAuth = Nothing
  , extract = eitherByteStringToText
  }

-- | Construct settings for downloading a Stack project template from a
-- repository.
settingsFromRepoTemplatePath ::
    HasTerm env
 => RepoTemplatePath
 -> RIO env TemplateDownloadSettings
settingsFromRepoTemplatePath (RepoTemplatePath GitHub user name) = do
  let basicAuthMsg token = prettyInfoL
        [ flow "Using content of"
        , fromString token
        , flow " environment variable to authenticate GitHub REST API."
        ]
  mBasicAuth <- do
    wantGitHubToken <- liftIO $ fromMaybe "" <$> lookupEnv gitHubTokenEnvVar
    if not (L.null wantGitHubToken)
      then do
         basicAuthMsg gitHubTokenEnvVar
         pure $ Just (gitHubBasicAuthType, fromString wantGitHubToken)
      else do
        wantAltGitHubToken <-
          liftIO $ fromMaybe "" <$> lookupEnv altGitHubTokenEnvVar
        if not (L.null wantAltGitHubToken)
        then do
          basicAuthMsg altGitHubTokenEnvVar
          pure $ Just (gitHubBasicAuthType, fromString wantAltGitHubToken)
        else pure Nothing
  pure TemplateDownloadSettings
    { downloadUrl = concat
        [ "https://api.github.com/repos/"
        , T.unpack user
        , "/stack-templates/contents/"
        , T.unpack name
        ]
    , basicAuth = mBasicAuth
    , extract = \bs -> do
        decodedJson <- eitherDecode (LB.fromStrict bs)
        case decodedJson of
          Object o | Just (String content) <- KeyMap.lookup "content" o -> do
            let noNewlines = T.filter (/= '\n')
            bsContent <- B64.decode $ T.encodeUtf8 (noNewlines content)
            mapLeft show $ decodeUtf8' bsContent
          _ ->
            Left "Couldn't parse GitHub response as a JSON object with a \
                 \\"content\" field"
    }
settingsFromRepoTemplatePath (RepoTemplatePath GitLab user name) = pure $
  asIsFromUrl $ concat
    [ "https://gitlab.com"
    , "/"
    , T.unpack user
    , "/stack-templates/raw/master/"
    , T.unpack name
    ]
settingsFromRepoTemplatePath (RepoTemplatePath Bitbucket user name) = pure $
  asIsFromUrl $ concat
    [ "https://bitbucket.org"
    , "/"
    , T.unpack user
    , "/stack-templates/raw/master/"
    , T.unpack name
    ]

-- | Apply and unpack a template into a directory.
applyTemplate ::
     HasConfig env
  => PackageName
  -> TemplateName
  -> Map Text Text
  -> Path Abs Dir
  -> Text
  -> RIO env (Map (Path Abs File) LB.ByteString)
applyTemplate project template nonceParams dir templateText = do
  config <- view configL
  currentYear <- do
    now <- liftIO getCurrentTime
    let (year, _, _) = toGregorian (utctDay now)
    pure $ T.pack . show $ year
  let context = M.unions [nonceParams, nameParams, configParams, yearParam]
       where
        nameAsVarId = T.replace "-" "_" $ T.pack $ packageNameString project
        nameAsModule = T.filter (/= ' ') $ T.toTitle $ T.replace "-" " " $
                           T.pack $ packageNameString project
        nameParams = M.fromList [ ("name", T.pack $ packageNameString project)
                                , ("name-as-varid", nameAsVarId)
                                , ("name-as-module", nameAsModule) ]
        configParams = config.templateParams
        yearParam = M.singleton "year" currentYear
  files :: Map FilePath LB.ByteString <-
    catch
      ( execWriterT $ runConduit $
            yield (T.encodeUtf8 templateText) .|
            unpackTemplate receiveMem id
      )
      ( \(e :: ProjectTemplateException) ->
          prettyThrowM $ TemplateInvalid template (string $ displayException e)
      )
  when (M.null files) $
    prettyThrowM $ TemplateInvalid
      template
      (flow "the template does not contain any files.")

  let isPkgSpec f = ".cabal" `L.isSuffixOf` f || "package.yaml" `L.isSuffixOf` f
  unless (any isPkgSpec . M.keys $ files) $
    prettyThrowM $ TemplateInvalid
      template
      (flow "the template does not contain a Cabal or package.yaml file.")

    -- Apply Mustache templating to a single file within the project template.
  let applyMustache bytes
        -- Workaround for performance problems with mustache and
        -- large files, applies to Yesod templates with large
        -- bootstrap CSS files. See
        -- https://github.com/commercialhaskell/stack/issues/4133.
        | LB.length bytes < 50000
        , Right text <- TLE.decodeUtf8' bytes = do
            let etemplateCompiled =
                  Mustache.compileTemplate (T.unpack (templateName template)) $ TL.toStrict text
            templateCompiled <- case etemplateCompiled of
              Left e -> prettyThrowM $ TemplateInvalid
                template
                (  flow "Stack encountered the following error:"
                <> blankLine
                   -- Text.Parsec.Error.ParseError is not an instance
                   -- of Control.Exception.
                <> string (show e)
                )
              Right t -> pure t
            let (substitutionErrors, applied) =
                  Mustache.checkedSubstitute templateCompiled context
                missingKeys =
                  S.fromList $ concatMap onlyMissingKeys substitutionErrors
            pure (LB.fromStrict $ encodeUtf8 applied, missingKeys)

        -- Too large or too binary
        | otherwise = pure (bytes, S.empty)

      -- Accumulate any missing keys as the file is processed
      processFile mks (fpOrig, bytes) = do
        -- Apply the mustache template to the filenames as well, so that we
        -- can have file names depend on the project name.
        (fp, mks1) <- applyMustache $ TLE.encodeUtf8 $ TL.pack fpOrig
        path <- parseRelFile $ TL.unpack $ TLE.decodeUtf8 fp
        (bytes', mks2) <- applyMustache bytes
        pure (mks <> mks1 <> mks2, (dir </> path, bytes'))

  (missingKeys, results) <- mapAccumLM processFile S.empty (M.toList files)
  unless (S.null missingKeys) $
    prettyNote $
      missingParameters
        missingKeys
        config.userGlobalConfigFile
  pure $ M.fromList results
 where
  onlyMissingKeys (Mustache.VariableNotFound ks) = map T.unpack ks
  onlyMissingKeys _ = []

  mapAccumLM :: Monad m => (a -> b -> m(a, c)) -> a -> [b] -> m(a, [c])
  mapAccumLM _ a [] = pure (a, [])
  mapAccumLM f a (x:xs) = do
    (a', c) <- f a x
    (a'', cs) <- mapAccumLM f a' xs
    pure (a'', c:cs)

  missingParameters :: Set String -> Path Abs File -> StyleDoc
  missingParameters missingKeys userConfigPath =
       fillSep
         ( flow "The following parameters were needed by the template but \
                \not provided:"
         : mkNarrativeList Nothing False
             (map toStyleDoc (S.toList missingKeys))
         )
    <> blankLine
    <> fillSep
         [ flow "You can provide them in Stack's global configuration file"
         , "(" <> pretty userConfigPath <> ")"
         , "like this:"
         ]
    <> blankLine
    <> "templates:"
    <> line
    <> "  params:"
    <> line
    <> vsep
         ( map
             (\key -> "    " <> fromString key <> ": value")
             (S.toList missingKeys)
         )
    <> blankLine
    <> flow "Or you can pass each one on the command line as parameters \
            \like this:"
    <> blankLine
    <> style Shell
         ( fillSep
             [ flow "stack new"
             , fromPackageName project
             , fromString $ T.unpack (templateName template)
             , hsep $
                 map
                   ( \key ->
                       fillSep [ "-p"
                               , "\"" <> fromString key <> ":value\""
                               ]
                   )
                   (S.toList missingKeys)
             ]
         )
    <> line
   where
    toStyleDoc :: String -> StyleDoc
    toStyleDoc = fromString

-- | Check if we're going to overwrite any existing files.
checkForOverwrite ::
     (MonadIO m, MonadThrow m)
  => Text
  -> [Path Abs File]
  -> m ()
checkForOverwrite name files = do
  overwrites <- filterM doesFileExist files
  unless (null overwrites) $
    prettyThrowM $ AttemptedOverwrites name overwrites

-- | Write files to the new project directory.
writeTemplateFiles ::
     MonadIO m
  => Map (Path Abs File) LB.ByteString
  -> m ()
writeTemplateFiles files =
  liftIO $
  forM_
    (M.toList files)
    (\(fp,bytes) ->
      do ensureDir (parent fp)
         writeBinaryFileAtomic fp $ lazyByteString bytes)

-- | Run any initialization functions, such as Git.
runTemplateInits :: HasConfig env => Path Abs Dir -> RIO env ()
runTemplateInits dir = do
  config <- view configL
  whenJust config.scmInit $ \Git ->
    withWorkingDir (toFilePath dir) $
      catchAny
        (proc "git" ["init"] runProcess_)
        ( \_ -> prettyWarnL
            [ flow "Stack failed to run a"
            , style Shell (flow "git init")
            , flow "command. Ignoring..."
            ]
        )

--------------------------------------------------------------------------------
-- Defaults

-- | The default service to use to download templates.
defaultRepoService :: RepoService
defaultRepoService = GitHub
