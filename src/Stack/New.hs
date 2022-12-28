{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

-- | Create new a new project directory populated with a basic working
-- project.

module Stack.New
    ( new
    , NewOpts (..)
    , TemplateName
    , templatesHelp
    ) where

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
import           Network.HTTP.StackClient
                   ( HttpException (..), HttpExceptionContent (..)
                   , Response (..), VerifiedDownloadException (..)
                   , getResponseBody, httpLbs, mkDownloadRequest, notFound404
                   , parseRequest, parseUrlThrow, setForceDownload
                   , setGitHubHeaders, setRequestCheckStatus
                   , verifiedDownloadWithProgress
                   )
import           Path ( (</>), dirname, parent, parseRelDir, parseRelFile )
import           Path.IO
                   ( doesDirExist, doesFileExist, ensureDir, getCurrentDir )
import           RIO.Process ( proc, runProcess_, withWorkingDir )
import           Stack.Constants ( backupUrlRelPath, wiredInPackages )
import           Stack.Constants.Config ( templatesDir )
import           Stack.Prelude
import           Stack.Types.Config ( Config (..), HasConfig (..), SCM (..) )
import           Stack.Types.TemplateName
                   ( RepoService (..), RepoTemplatePath (..), TemplateName
                   , TemplatePath (..), defaultTemplateName
                   , parseRepoPathWithService, templateName, templatePath
                   )
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
  | DownloadTemplatesHelpFailed !HttpException
  | TemplatesHelpEncodingInvalid !String !UnicodeException
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
         , style Dir (pretty path)
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
                   <> fromString (show req)
                   <> blankLine
                   <> flow "and the content of the error was:"
                   <> blankLine
                   <> fromString (show content)
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
         , style File (pretty path) <> "."
         ]
  pretty (ExtractTemplateFailed name path err) =
    "[S-9582]"
    <> line
    <> fillSep
         [ flow "Stack failed to extract the loaded template"
         , style Current (fromString $ T.unpack $ templateName name)
         , "at"
         , style File (pretty path) <> "."
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
             ( map toStyleDoc (L.sort $ S.toList wiredInPackages)
             )
         )
   where
    toStyleDoc :: PackageName -> StyleDoc
    toStyleDoc = fromString . packageNameString
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
  pretty (DownloadTemplatesHelpFailed err) =
    "[S-8143]"
    <> line
    <> fillSep
         [ flow "Stack failed to download the help for"
         , style Shell "stack templates" <> "."
         ]
    <> blankLine
    <> flow "While downloading, Stack encountered the following error:"
    <> blankLine
    <> string (displayException err)
  pretty (TemplatesHelpEncodingInvalid url err) =
    "[S-6670]"
    <> line
    <> fillSep
         [ flow "Stack failed to decode the help for"
         , style Shell "stack templates"
         , flow "downloaded from"
         , style Url (fromString url) <> "."
         ]
    <> blankLine
    <> flow "While decoding, Stack encountered the following error:"
    <> blankLine
    <> string (displayException err)


instance Exception NewPrettyException

--------------------------------------------------------------------------------
-- Main project creation

-- | Options for creating a new project.
data NewOpts = NewOpts
  { newOptsProjectName  :: PackageName
  -- ^ Name of the project to create.
  , newOptsCreateBare   :: Bool
  -- ^ Whether to create the project without a directory.
  , newOptsTemplate     :: Maybe TemplateName
  -- ^ Name of the template to use.
  , newOptsNonceParams  :: Map Text Text
  -- ^ Nonce parameters specified just for this invocation.
  }

-- | Create a new project with the given options.
new :: HasConfig env => NewOpts -> Bool -> RIO env (Path Abs Dir)
new opts forceOverwrite = do
  when (project `elem` wiredInPackages) $
      throwM $ PrettyException $ MagicPackageNameInvalid projectName
  pwd <- getCurrentDir
  absDir <- if bare
              then pure pwd
              else do relDir <- parseRelDir (packageNameString project)
                      liftM (pwd </>) (pure relDir)
  exists <- doesDirExist absDir
  configTemplate <- view $ configL.to configDefaultTemplate
  let template = fromMaybe defaultTemplateName $ asum [ cliOptionTemplate
                                                      , configTemplate
                                                      ]
  if exists && not bare
    then throwM $ PrettyException $
             ProjectDirAlreadyExists projectName absDir
    else do
      templateText <- loadTemplate template (logUsing absDir template)
      files <-
        applyTemplate
          project
          template
          (newOptsNonceParams opts)
          absDir
          templateText
      when (not forceOverwrite && bare) $
        checkForOverwrite (templateName template) (M.keys files)
      writeTemplateFiles files
      runTemplateInits absDir
      pure absDir
 where
  cliOptionTemplate = newOptsTemplate opts
  project = newOptsProjectName opts
  projectName = packageNameString project
  bare = newOptsCreateBare opts
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
                              , style Dir (pretty $ dirname absDir)
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
  templateDir <- view $ configL.to templatesDir
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
        (\(e :: PrettyException) -> do
              case relSettings rawParam of
                Just settings -> do
                  let url = tplDownloadUrl settings
                      extract = tplExtract settings
                  downloadTemplate url extract (templateDir </> relFile)
                Nothing -> throwM e
        )
    RepoPath rtp -> do
      let settings = settingsFromRepoTemplatePath rtp
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
          Left err -> throwM $ PrettyException $
              ExtractTemplateFailed name path err
          Right template ->
              pure template
      else throwM $ PrettyException $
        LoadTemplateFailed name path

  relSettings :: String -> Maybe TemplateDownloadSettings
  relSettings req = do
    rtp <- parseRepoPathWithService defaultRepoService (T.pack req)
    pure (settingsFromRepoTemplatePath rtp)

  downloadFromUrl :: TemplateDownloadSettings -> Path Abs Dir -> RIO env Text
  downloadFromUrl settings templateDir = do
    let url = tplDownloadUrl settings
        rel = fromMaybe backupUrlRelPath (parseRelFile url)
    downloadTemplate url (tplExtract settings) (templateDir </> rel)

  downloadTemplate :: String
                   -> (ByteString
                   -> Either String Text)
                   -> Path Abs File
                   -> RIO env Text
  downloadTemplate url extract path = do
    req <- parseRequest url
    let dReq = setForceDownload True $
                   mkDownloadRequest (setRequestCheckStatus req)
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
        throwM $ PrettyException $
          DownloadTemplateFailed (templateName name) url exception

data TemplateDownloadSettings = TemplateDownloadSettings
  { tplDownloadUrl :: String
  , tplExtract :: ByteString -> Either String Text
  }

eitherByteStringToText :: ByteString -> Either String Text
eitherByteStringToText = mapLeft show . decodeUtf8'

asIsFromUrl :: String -> TemplateDownloadSettings
asIsFromUrl url = TemplateDownloadSettings
  { tplDownloadUrl = url
  , tplExtract = eitherByteStringToText
  }

-- | Construct a URL for downloading from a repo.
settingsFromRepoTemplatePath :: RepoTemplatePath -> TemplateDownloadSettings
settingsFromRepoTemplatePath (RepoTemplatePath GitHub user name) =
  -- T.concat ["https://raw.githubusercontent.com", "/", user, "/stack-templates/master/", name]
  TemplateDownloadSettings
  { tplDownloadUrl = concat
      [ "https://api.github.com/repos/"
      , T.unpack user
      , "/stack-templates/contents/"
      , T.unpack name
      ]
  , tplExtract = \bs -> do
      decodedJson <- eitherDecode (LB.fromStrict bs)
      case decodedJson of
        Object o | Just (String content) <- KeyMap.lookup "content" o -> do
                     let noNewlines = T.filter (/= '\n')
                     bsContent <- B64.decode $ T.encodeUtf8 (noNewlines content)
                     mapLeft show $ decodeUtf8' bsContent
        _ ->
          Left "Couldn't parse GitHub response as a JSON object with a \"content\" field"
  }

settingsFromRepoTemplatePath (RepoTemplatePath GitLab user name) =
  asIsFromUrl $ concat
    [ "https://gitlab.com"
    , "/"
    , T.unpack user
    , "/stack-templates/raw/master/"
    , T.unpack name
    ]
settingsFromRepoTemplatePath (RepoTemplatePath Bitbucket user name) =
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
        configParams = configTemplateParams config
        yearParam = M.singleton "year" currentYear
  files :: Map FilePath LB.ByteString <-
    catch
      ( execWriterT $ runConduit $
            yield (T.encodeUtf8 templateText) .|
            unpackTemplate receiveMem id
      )
      ( \(e :: ProjectTemplateException) ->
            throwM $ PrettyException $
                TemplateInvalid template (string $ displayException e)
      )
  when (M.null files) $
    throwM $ PrettyException $
      TemplateInvalid
        template
        (flow "the template does not contain any files.")

  let isPkgSpec f = ".cabal" `L.isSuffixOf` f || f == "package.yaml"
  unless (any isPkgSpec . M.keys $ files) $
    throwM $ PrettyException $
      TemplateInvalid
        template
        ( flow "the template does not contain a Cabal or package.yaml file."
        )

    -- Apply Mustache templating to a single file within the project template.
  let applyMustache bytes
        -- Workaround for performance problems with mustache and
        -- large files, applies to Yesod templates with large
        -- bootstrap CSS files. See
        -- https://github.com/commercialhaskell/stack/issues/4133.
        | LB.length bytes < 50000
        , Right text <- TLE.decodeUtf8' bytes = do
            let etemplateCompiled = Mustache.compileTemplate (T.unpack (templateName template)) $ TL.toStrict text
            templateCompiled <- case etemplateCompiled of
              Left e -> throwM $ PrettyException $
                TemplateInvalid
                  template
                  (  flow "Stack encountered the following error:"
                  <> blankLine
                     -- Text.Parsec.Error.ParseError is not an instance
                     -- of Control.Exception.
                  <> string (show e)
                  )
              Right t -> pure t
            let (substitutionErrors, applied) = Mustache.checkedSubstitute templateCompiled context
                missingKeys = S.fromList $ concatMap onlyMissingKeys substitutionErrors
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
  unless (S.null missingKeys) $ do
    prettyNote $
      missingParameters
        missingKeys
        (configUserConfigPath config)
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
         : mkNarrativeList
             Nothing
             False
             (map toStyleDoc (S.toList missingKeys))
         )
    <> blankLine
    <> fillSep
         [ flow "You can provide them in Stack's global YAML configuration \
                \file"
         , "(" <> style File (pretty userConfigPath) <> ")"
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
             , fromString (packageNameString project)
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
    throwM $ PrettyException $ AttemptedOverwrites name overwrites

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
  case configScmInit config of
    Nothing -> pure ()
    Just Git -> withWorkingDir (toFilePath dir) $
      catchAny
        (proc "git" ["init"] runProcess_)
        ( \_ -> prettyWarn $
                  fillSep
                    [ flow "Stack failed to run a"
                    , style Shell (flow "git init")
                    , flow "command. Ignoring..."
                    ]
        )

-- | Display help for the templates command.
templatesHelp :: HasLogFunc env => RIO env ()
templatesHelp = do
  let url = defaultTemplatesHelpUrl
  req <- liftM setGitHubHeaders (parseUrlThrow url)
  resp <- catch
    (httpLbs req)
    (throwM . PrettyException. DownloadTemplatesHelpFailed)
  case decodeUtf8' $ LB.toStrict $ getResponseBody resp of
    Left err -> throwM $ PrettyException $ TemplatesHelpEncodingInvalid url err
    Right txt -> logInfo $ display txt

--------------------------------------------------------------------------------
-- Defaults

-- | The default service to use to download templates.
defaultRepoService :: RepoService
defaultRepoService = GitHub

-- | Default web URL to get the `stack templates` help output.
defaultTemplatesHelpUrl :: String
defaultTemplatesHelpUrl =
  "https://raw.githubusercontent.com/commercialhaskell/stack-templates/master/STACK_HELP.md"
