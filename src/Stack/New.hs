{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Create new a new project directory populated with a basic working
-- project.

module Stack.New
    ( new
    , NewOpts(..)
    , TemplateName
    , templatesHelp
    ) where

import           Stack.Prelude
import           Control.Monad.Trans.Writer.Strict
import           Control.Monad (void)
import           Data.ByteString.Builder (lazyByteString)
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Time.Calendar
import           Data.Time.Clock
import           Network.HTTP.StackClient (DownloadException (..), Request, HttpException,
                                           getResponseStatusCode, getResponseBody, httpLbs,
                                           parseRequest, parseUrlThrow, redownload, setGithubHeaders)
import           Path
import           Path.IO
import           Stack.Constants
import           Stack.Constants.Config
import           Stack.Types.Config
import           Stack.Types.TemplateName
import           RIO.Process
import qualified Text.Mustache as Mustache
import qualified Text.Mustache.Render as Mustache
import           Text.ProjectTemplate

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
    when (newOptsProjectName opts `elem` wiredInPackages) $
      throwM $ Can'tUseWiredInName (newOptsProjectName opts)
    pwd <- getCurrentDir
    absDir <- if bare then return pwd
                      else do relDir <- parseRelDir (packageNameString project)
                              liftM (pwd </>) (return relDir)
    exists <- doesDirExist absDir
    configTemplate <- view $ configL.to configDefaultTemplate
    let template = fromMaybe defaultTemplateName $ asum [ cliOptionTemplate
                                                        , configTemplate
                                                        ]
    if exists && not bare
        then throwM (AlreadyExists absDir)
        else do
            templateText <- loadTemplate template (logUsing absDir template)
            files <-
                applyTemplate
                    project
                    template
                    (newOptsNonceParams opts)
                    absDir
                    templateText
            when (not forceOverwrite && bare) $ checkForOverwrite (M.keys files)
            writeTemplateFiles files
            runTemplateInits absDir
            return absDir
  where
    cliOptionTemplate = newOptsTemplate opts
    project = newOptsProjectName opts
    bare = newOptsCreateBare opts
    logUsing absDir template templateFrom =
        let loading = case templateFrom of
                          LocalTemp -> "Loading local"
                          RemoteTemp -> "Downloading"
         in
        logInfo
            (loading <> " template \"" <> display (templateName template) <>
             "\" to create project \"" <>
             fromString (packageNameString project) <>
             "\" in " <>
             if bare then "the current directory"
                     else fromString (toFilePath (dirname absDir)) <>
             " ...")

data TemplateFrom = LocalTemp | RemoteTemp

-- | Download and read in a template's text content.
loadTemplate
    :: forall env. HasConfig env
    => TemplateName
    -> (TemplateFrom -> RIO env ())
    -> RIO env Text
loadTemplate name logIt = do
    templateDir <- view $ configL.to templatesDir
    case templatePath name of
        AbsPath absFile -> logIt LocalTemp >> loadLocalFile absFile
        UrlPath s -> downloadFromUrl s templateDir
        RelPath rawParam relFile ->
            catch
                (do f <- loadLocalFile relFile
                    logIt LocalTemp
                    return f)
                (\(e :: NewException) ->
                      case relRequest rawParam of
                        Just req -> downloadTemplate req
                                                     (templateDir </> relFile)
                        Nothing -> throwM e
                )
        RepoPath rtp -> do
            let url = urlFromRepoTemplatePath rtp
            downloadFromUrl (T.unpack url) templateDir
                            
  where
    loadLocalFile :: Path b File -> RIO env Text
    loadLocalFile path = do
        logDebug ("Opening local template: \"" <> fromString (toFilePath path)
                                                <> "\"")
        exists <- doesFileExist path
        if exists
            then readFileUtf8 (toFilePath path)
            else throwM (FailedToLoadTemplate name (toFilePath path))
    relRequest :: String -> Maybe Request
    relRequest req = do
        rtp <- parseRepoPathWithService defaultRepoService (T.pack req)
        let url = urlFromRepoTemplatePath rtp
        parseRequest (T.unpack url)
    downloadFromUrl :: String -> Path Abs Dir -> RIO env Text
    downloadFromUrl s templateDir = do
        req <- parseRequest s
        let rel = fromMaybe backupUrlRelPath (parseRelFile s)
        downloadTemplate req (templateDir </> rel)
    downloadTemplate :: Request -> Path Abs File -> RIO env Text
    downloadTemplate req path = do
        logIt RemoteTemp
        catch
          (void $ redownload req path)
          (useCachedVersionOrThrow path)

        loadLocalFile path
    useCachedVersionOrThrow :: Path Abs File -> DownloadException -> RIO env ()
    useCachedVersionOrThrow path exception = do
      exists <- doesFileExist path

      if exists
        then do logWarn "Tried to download the template but an error was found."
                logWarn "Using cached local version. It may not be the most recent version though."
        else throwM (FailedToDownloadTemplate name exception)

-- | Construct a URL for downloading from a repo.
urlFromRepoTemplatePath :: RepoTemplatePath -> Text
urlFromRepoTemplatePath (RepoTemplatePath Github user name) =
    T.concat ["https://raw.githubusercontent.com", "/", user, "/stack-templates/master/", name]
urlFromRepoTemplatePath (RepoTemplatePath Gitlab user name) =
    T.concat ["https://gitlab.com",                "/", user, "/stack-templates/raw/master/", name]
urlFromRepoTemplatePath (RepoTemplatePath Bitbucket user name) =
    T.concat ["https://bitbucket.org",             "/", user, "/stack-templates/raw/master/", name]

-- | Apply and unpack a template into a directory.
applyTemplate
    :: HasConfig env
    => PackageName
    -> TemplateName
    -> Map Text Text
    -> Path Abs Dir
    -> Text
    -> RIO env  (Map (Path Abs File) LB.ByteString)
applyTemplate project template nonceParams dir templateText = do
    config <- view configL
    currentYear <- do
      now <- liftIO getCurrentTime
      let (year, _, _) = toGregorian (utctDay now)
      return $ T.pack . show $ year
    let context = M.unions [nonceParams, nameParams, configParams, yearParam]
          where
            nameAsVarId = T.replace "-" "_" $ T.pack $ packageNameString project
            nameAsModule = T.filter (/= '-') $ T.toTitle $ T.pack $ packageNameString project
            nameParams = M.fromList [ ("name", T.pack $ packageNameString project)
                                    , ("name-as-varid", nameAsVarId)
                                    , ("name-as-module", nameAsModule) ]
            configParams = configTemplateParams config
            yearParam = M.singleton "year" currentYear
    files :: Map FilePath LB.ByteString <-
        catch (execWriterT $ runConduit $
               yield (T.encodeUtf8 templateText) .|
               unpackTemplate receiveMem id
              )
              (\(e :: ProjectTemplateException) ->
                   throwM (InvalidTemplate template (show e)))
    when (M.null files) $
         throwM (InvalidTemplate template "Template does not contain any files")

    let isPkgSpec f = ".cabal" `isSuffixOf` f || f == "package.yaml"
    unless (any isPkgSpec . M.keys $ files) $
         throwM (InvalidTemplate template "Template does not contain a .cabal \
                                          \or package.yaml file")

    -- Apply Mustache templating to a single file within the project
    -- template.
    let applyMustache bytes
          -- Workaround for performance problems with mustache and
          -- large files, applies to Yesod templates with large
          -- bootstrap CSS files. See
          -- https://github.com/commercialhaskell/stack/issues/4133.
          | LB.length bytes < 50000
          , Right text <- TLE.decodeUtf8' bytes = do
              let etemplateCompiled = Mustache.compileTemplate (T.unpack (templateName template)) $ TL.toStrict text
              templateCompiled <- case etemplateCompiled of
                Left e -> throwM $ InvalidTemplate template (show e)
                Right t -> return t
              let (substitutionErrors, applied) = Mustache.checkedSubstitute templateCompiled context
                  missingKeys = S.fromList $ concatMap onlyMissingKeys substitutionErrors
              unless (S.null missingKeys)
                (logInfo ("\n" <> displayShow (MissingParameters project template missingKeys (configUserConfigPath config)) <> "\n"))
              pure $ LB.fromStrict $ encodeUtf8 applied

          -- Too large or too binary
          | otherwise = pure bytes

    liftM
        M.fromList
        (mapM
             (\(fpOrig,bytes) ->
                   do -- Apply the mustache template to the filenames
                      -- as well, so that we can have file names
                      -- depend on the project name.
                      fp <- applyMustache $ TLE.encodeUtf8 $ TL.pack fpOrig
                      path <- parseRelFile $ TL.unpack $ TLE.decodeUtf8 fp
                      bytes' <- applyMustache bytes
                      return (dir </> path, bytes'))
             (M.toList files))
  where
    onlyMissingKeys (Mustache.VariableNotFound ks) = map T.unpack ks
    onlyMissingKeys _ = []

-- | Check if we're going to overwrite any existing files.
checkForOverwrite :: (MonadIO m, MonadThrow m) => [Path Abs File] -> m ()
checkForOverwrite files = do
    overwrites <- filterM doesFileExist files
    unless (null overwrites) $ throwM (AttemptedOverwrites overwrites)

-- | Write files to the new project directory.
writeTemplateFiles
    :: MonadIO m
    => Map (Path Abs File) LB.ByteString -> m ()
writeTemplateFiles files =
    liftIO $
    forM_
        (M.toList files)
        (\(fp,bytes) ->
              do ensureDir (parent fp)
                 writeBinaryFileAtomic fp $ lazyByteString bytes)

-- | Run any initialization functions, such as Git.
runTemplateInits
    :: HasConfig env
    => Path Abs Dir
    -> RIO env ()
runTemplateInits dir = do
    config <- view configL
    case configScmInit config of
        Nothing -> return ()
        Just Git ->
            withWorkingDir (toFilePath dir) $
            catchAny (proc "git" ["init"] runProcess_)
                  (\_ -> logInfo "git init failed to run, ignoring ...")

-- | Display help for the templates command.
templatesHelp :: HasLogFunc env => RIO env ()
templatesHelp = do
  let url = defaultTemplatesHelpUrl
  req <- liftM setGithubHeaders (parseUrlThrow url)
  resp <- httpLbs req `catch` (throwM . FailedToDownloadTemplatesHelp)
  case decodeUtf8' $ LB.toStrict $ getResponseBody resp of
    Left err -> throwM $ BadTemplatesHelpEncoding url err
    Right txt -> logInfo $ display txt

--------------------------------------------------------------------------------
-- Defaults

-- | The default service to use to download templates.
defaultRepoService :: RepoService
defaultRepoService = Github

-- | Default web URL to get the `stack templates` help output.
defaultTemplatesHelpUrl :: String
defaultTemplatesHelpUrl =
    "https://raw.githubusercontent.com/commercialhaskell/stack-templates/master/STACK_HELP.md"

--------------------------------------------------------------------------------
-- Exceptions

-- | Exception that might occur when making a new project.
data NewException
    = FailedToLoadTemplate !TemplateName
                           !FilePath
    | FailedToDownloadTemplate !TemplateName
                               !DownloadException
    | AlreadyExists !(Path Abs Dir)
    | MissingParameters !PackageName !TemplateName !(Set String) !(Path Abs File)
    | InvalidTemplate !TemplateName !String
    | AttemptedOverwrites [Path Abs File]
    | FailedToDownloadTemplatesHelp !HttpException
    | BadTemplatesHelpEncoding
        !String -- URL it's downloaded from
        !UnicodeException
    | Can'tUseWiredInName !PackageName
    deriving (Typeable)

instance Exception NewException

instance Show NewException where
    show (FailedToLoadTemplate name path) =
        "Failed to load download template " <> T.unpack (templateName name) <>
        " from " <>
        path
    show (FailedToDownloadTemplate name (RedownloadInvalidResponse _ _ resp)) =
        case getResponseStatusCode resp of
            404 ->
                "That template doesn't exist. Run `stack templates' to discover available templates."
            code ->
                "Failed to download template " <> T.unpack (templateName name) <>
                ": unknown reason, status code was: " <>
                show code

    show (FailedToDownloadTemplate name (RedownloadHttpError httpError)) =
          "There was an unexpected HTTP error while downloading template " <>
          T.unpack (templateName name) <> ": " <> show httpError
    show (AlreadyExists path) =
        "Directory " <> toFilePath path <> " already exists. Aborting."
    show (MissingParameters name template missingKeys userConfigPath) =
        intercalate
            "\n"
            [ "The following parameters were needed by the template but not provided: " <>
              intercalate ", " (S.toList missingKeys)
            , "You can provide them in " <>
              toFilePath userConfigPath <>
              ", like this:"
            , "templates:"
            , "  params:"
            , intercalate
                  "\n"
                  (map
                       (\key ->
                             "    " <> key <> ": value")
                       (S.toList missingKeys))
            , "Or you can pass each one as parameters like this:"
            , "stack new " <> packageNameString name <> " " <>
              T.unpack (templateName template) <>
              " " <>
              unwords
                  (map
                       (\key ->
                             "-p \"" <> key <> ":value\"")
                       (S.toList missingKeys))]
    show (InvalidTemplate name why) =
        "The template \"" <> T.unpack (templateName name) <>
        "\" is invalid and could not be used. " <>
        "The error was: " <> why
    show (AttemptedOverwrites fps) =
        "The template would create the following files, but they already exist:\n" <>
        unlines (map (("  " ++) . toFilePath) fps) <>
        "Use --force to ignore this, and overwite these files."
    show (FailedToDownloadTemplatesHelp ex) =
        "Failed to download `stack templates` help. The HTTP error was: " <> show ex
    show (BadTemplatesHelpEncoding url err) =
        "UTF-8 decoding error on template info from\n    " <> url <> "\n\n" <> show err
    show (Can'tUseWiredInName name) =
        "The name \"" <> packageNameString name <> "\" is used by GHC wired-in packages, and so shouldn't be used as a package name"
