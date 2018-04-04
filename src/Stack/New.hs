{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Create new a new project directory populated with a basic working
-- project.

module Stack.New
    ( new
    , NewOpts(..)
    , defaultTemplateName
    , templateNameArgument
    , getTemplates
    , TemplateName
    , listTemplates)
    where

import           Stack.Prelude
import           Control.Monad.Trans.Writer.Strict
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import qualified Data.HashMap.Strict as HM
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T (lenientDecode)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Data.Yaml as Yaml
import           Network.HTTP.Download
import           Network.HTTP.Simple (Request, HttpException, getResponseStatusCode, getResponseBody)
import           Path
import           Path.IO
import           Stack.Constants
import           Stack.Constants.Config
import           Stack.Types.Config
import           Stack.Types.PackageName
import           Stack.Types.TemplateName
import           RIO.Process
import           Text.Hastache
import           Text.Hastache.Context
import           Text.Printf
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
             display (packageNameText project) <>
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
        UrlPath s -> do
            req <- parseRequest s
            let rel = fromMaybe backupUrlRelPath (parseRelFile s)
            downloadTemplate req (templateDir </> rel)
        RelPath relFile ->
            catch
                (do f <- loadLocalFile relFile
                    logIt LocalTemp
                    return f)
                (\(e :: NewException) ->
                      case relRequest relFile of
                        Just req -> downloadTemplate req
                                                     (templateDir </> relFile)
                        Nothing -> throwM e
                )
  where
    loadLocalFile :: Path b File -> RIO env Text
    loadLocalFile path = do
        logDebug ("Opening local template: \"" <> fromString (toFilePath path)
                                                <> "\"")
        exists <- doesFileExist path
        if exists
            then liftIO (fmap (T.decodeUtf8With T.lenientDecode) (SB.readFile (toFilePath path)))
            else throwM (FailedToLoadTemplate name (toFilePath path))
    relRequest :: MonadThrow n => Path Rel File -> n Request
    relRequest rel = parseRequest (defaultTemplateUrl <> "/" <> toFilePath rel)
    downloadTemplate :: Request -> Path Abs File -> RIO env Text
    downloadTemplate req path = do
        logIt RemoteTemp
        _ <-
            catch
                (redownload req path)
                (throwM . FailedToDownloadTemplate name)
        loadLocalFile path
    backupUrlRelPath = $(mkRelFile "downloaded.template.file.hsfiles")

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
            nameAsVarId = T.replace "-" "_" $ packageNameText project
            nameAsModule = T.filter (/= '-') $ T.toTitle $ packageNameText project
            nameParams = M.fromList [ ("name", packageNameText project)
                                    , ("name-as-varid", nameAsVarId)
                                    , ("name-as-module", nameAsModule) ]
            configParams = configTemplateParams config
            yearParam = M.singleton "year" currentYear
    (applied,missingKeys) <-
        runWriterT
            (hastacheStr
                 defaultConfig { muEscapeFunc = id }
                 templateText
                 (mkStrContextM (contextFunction context)))
    unless (S.null missingKeys)
         (logInfo ("\n" <> displayShow (MissingParameters project template missingKeys (configUserConfigPath config)) <> "\n"))
    files :: Map FilePath LB.ByteString <-
        catch (execWriterT $ runConduit $
               yield (T.encodeUtf8 (LT.toStrict applied)) .|
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
    liftM
        M.fromList
        (mapM
             (\(fp,bytes) ->
                   do path <- parseRelFile fp
                      return (dir </> path, bytes))
             (M.toList files))
  where
    -- | Does a lookup in the context and returns a moustache value,
    -- on the side, writes out a set of keys that were requested but
    -- not found.
    contextFunction
        :: Monad m
        => Map Text Text
        -> String
        -> WriterT (Set String) m (MuType (WriterT (Set String) m))
    contextFunction context key =
        case M.lookup (T.pack key) context of
            Nothing -> do
                tell (S.singleton key)
                return MuNothing
            Just value -> return (MuVariable value)

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
    forM_
        (M.toList files)
        (\(fp,bytes) ->
              do ensureDir (parent fp)
                 liftIO (LB.writeFile (toFilePath fp) bytes))

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

-- | Display the set of templates accompanied with description if available.
listTemplates :: HasLogFunc env => RIO env ()
listTemplates = do
    templates <- getTemplates
    templateInfo <- getTemplateInfo
    if not . M.null $ templateInfo then do
      let keySizes  = map (T.length . templateName) $ S.toList templates
          padWidth  = show $ maximum keySizes
          outputfmt = "%-" <> padWidth <> "s %s\n"
          headerfmt = "%-" <> padWidth <> "s   %s\n"
      liftIO $ printf headerfmt ("Template"::String) ("Description"::String)
      forM_ (S.toList templates) (\x -> do
           let name = templateName x
               desc = fromMaybe "" $ liftM (mappend "- ") (M.lookup name templateInfo >>= description)
           liftIO $ printf outputfmt (T.unpack name) (T.unpack desc))
      else mapM_ (liftIO . T.putStrLn . templateName) (S.toList templates)

-- | Get the set of templates.
getTemplates :: HasLogFunc env => RIO env (Set TemplateName)
getTemplates = do
    req <- liftM setGithubHeaders (parseUrlThrow defaultTemplatesList)
    resp <- catch (httpJSON req) (throwM . FailedToDownloadTemplates)
    case getResponseStatusCode resp of
        200 -> return $ unTemplateSet $ getResponseBody resp
        code -> throwM (BadTemplatesResponse code)

getTemplateInfo :: HasLogFunc env => RIO env (Map Text TemplateInfo)
getTemplateInfo = do
  req <- liftM setGithubHeaders (parseUrlThrow defaultTemplateInfoUrl)
  resp <- catch (liftM Right $ httpLbs req) (\(ex :: HttpException) -> return . Left $ "Failed to download template info. The HTTP error was: " <> show ex)
  case resp >>= is200 of
    Left err -> do
      logInfo $ fromString err
      return M.empty
    Right resp' ->
      case Yaml.decodeEither (LB.toStrict $ getResponseBody resp') :: Either String Object of
        Left err ->
          throwM $ BadTemplateInfo err
        Right o ->
          return (M.mapMaybe (Yaml.parseMaybe Yaml.parseJSON) (M.fromList . HM.toList $ o) :: Map Text TemplateInfo)
  where
    is200 resp =
      case getResponseStatusCode resp of
        200 -> return resp
        code -> Left $ "Unexpected status code while retrieving templates info: " <> show code

newtype TemplateSet = TemplateSet { unTemplateSet :: Set TemplateName }
instance FromJSON TemplateSet where
  parseJSON = fmap TemplateSet . parseTemplateSet

-- | Parser the set of templates from the JSON.
parseTemplateSet :: Value -> Parser (Set TemplateName)
parseTemplateSet a = do
    xs <- parseJSON a
    fmap S.fromList (mapMaybeM parseTemplate xs)
  where
    parseTemplate v = do
        o <- parseJSON v
        name <- o .: "name"
        if ".hsfiles" `isSuffixOf` name
            then case parseTemplateNameFromString name of
                     Left{} ->
                         fail ("Unable to parse template name from " <> name)
                     Right template -> return (Just template)
            else return Nothing

--------------------------------------------------------------------------------
-- Defaults

-- | The default template name you can use if you don't have one.
defaultTemplateName :: TemplateName
defaultTemplateName = $(mkTemplateName "new-template")

-- | Default web root URL to download from.
defaultTemplateUrl :: String
defaultTemplateUrl =
    "https://raw.githubusercontent.com/commercialhaskell/stack-templates/master"

-- | Default web URL to get a yaml file containing template metadata.
defaultTemplateInfoUrl :: String
defaultTemplateInfoUrl =
    "https://raw.githubusercontent.com/commercialhaskell/stack-templates/master/template-info.yaml"

-- | Default web URL to list the repo contents.
defaultTemplatesList :: String
defaultTemplatesList =
    "https://api.github.com/repos/commercialhaskell/stack-templates/contents/"

--------------------------------------------------------------------------------
-- Exceptions

-- | Exception that might occur when making a new project.
data NewException
    = FailedToLoadTemplate !TemplateName
                           !FilePath
    | FailedToDownloadTemplate !TemplateName
                               !DownloadException
    | FailedToDownloadTemplates !HttpException
    | BadTemplatesResponse !Int
    | AlreadyExists !(Path Abs Dir)
    | MissingParameters !PackageName !TemplateName !(Set String) !(Path Abs File)
    | InvalidTemplate !TemplateName !String
    | AttemptedOverwrites [Path Abs File]
    | FailedToDownloadTemplateInfo !HttpException
    | BadTemplateInfo !String
    | BadTemplateInfoResponse !Int
    | Can'tUseWiredInName !PackageName
    deriving (Typeable)

instance Exception NewException

instance Show NewException where
    show (FailedToLoadTemplate name path) =
        "Failed to load download template " <> T.unpack (templateName name) <>
        " from " <>
        path
    show (FailedToDownloadTemplate name (RedownloadFailed _ _ resp)) =
        case getResponseStatusCode resp of
            404 ->
                "That template doesn't exist. Run `stack templates' to see a list of available templates."
            code ->
                "Failed to download template " <> T.unpack (templateName name) <>
                ": unknown reason, status code was: " <>
                show code
    show (AlreadyExists path) =
        "Directory " <> toFilePath path <> " already exists. Aborting."
    show (FailedToDownloadTemplates ex) =
        "Failed to download templates. The HTTP error was: " <> show ex
    show (BadTemplatesResponse code) =
        "Unexpected status code while retrieving templates list: " <> show code
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
        "The error was: \"" <> why <> "\""
    show (AttemptedOverwrites fps) =
        "The template would create the following files, but they already exist:\n" <>
        unlines (map (("  " ++) . toFilePath) fps) <>
        "Use --force to ignore this, and overwite these files."
    show (FailedToDownloadTemplateInfo ex) =
        "Failed to download templates info. The HTTP error was: " <> show ex
    show (BadTemplateInfo err) =
        "Template info couldn't be parsed: " <> err
    show (BadTemplateInfoResponse code) =
        "Unexpected status code while retrieving templates info: " <> show code
    show (Can'tUseWiredInName name) =
        "The name \"" <> packageNameString name <> "\" is used by GHC wired-in packages, and so shouldn't be used as a package name"
