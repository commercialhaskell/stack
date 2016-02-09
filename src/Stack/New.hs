{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
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

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Writer.Strict
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Conduit
import           Data.Foldable (asum)
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Maybe.Extra (mapMaybeM)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Typeable
import           Network.HTTP.Client.Conduit hiding (path)
import           Network.HTTP.Download
import           Network.HTTP.Types.Status
import           Path
import           Path.IO
import           Prelude
import           Stack.Constants
import           Stack.Types
import           Stack.Types.TemplateName
import           System.Process.Run
import           Text.Hastache
import           Text.Hastache.Context
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
new
    :: (HasConfig r, MonadReader r m, MonadLogger m, MonadCatch m, MonadThrow m, MonadIO m, HasHttpManager r, Functor m, Applicative m)
    => NewOpts -> m (Path Abs Dir)
new opts = do
    pwd <- getCurrentDir
    absDir <- if bare then return pwd
                      else do relDir <- parseRelDir (packageNameString project)
                              liftM (pwd </>) (return relDir)
    exists <- doesDirExist absDir
    configTemplate <- configDefaultTemplate <$> asks getConfig
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
        $logInfo
            (loading <> " template \"" <> templateName template <>
             "\" to create project \"" <>
             packageNameText project <>
             "\" in " <>
             if bare then "the current directory"
                     else T.pack (toFilePath (dirname absDir)) <>
             " ...")

data TemplateFrom = LocalTemp | RemoteTemp

-- | Download and read in a template's text content.
loadTemplate
    :: forall m r.
       (HasConfig r, HasHttpManager r, MonadReader r m, MonadIO m, MonadThrow m, MonadCatch m, MonadLogger m, Functor m, Applicative m)
    => TemplateName -> (TemplateFrom -> m ()) -> m Text
loadTemplate name logIt = do
    templateDir <- templatesDir <$> asks getConfig
    case templatePath name of
        AbsPath absFile -> logIt LocalTemp >> loadLocalFile absFile
        UrlPath s -> do
            let req = fromMaybe (error "impossible happened: already valid \
                                       \URL couldn't be parsed")
                                (parseUrl s)
                rel = fromMaybe backupUrlRelPath (parseRelFile s)
            downloadTemplate req (templateDir </> rel)
        RelPath relFile ->
            catch
                (loadLocalFile relFile <* logIt LocalTemp)
                (\(e :: NewException) ->
                      case relRequest relFile of
                        Just req -> downloadTemplate req
                                                     (templateDir </> relFile)
                        Nothing -> throwM e
                )
  where
    loadLocalFile :: Path b File -> m Text
    loadLocalFile path = do
        $logDebug ("Opening local template: \"" <> T.pack (toFilePath path)
                                                <> "\"")
        exists <- doesFileExist path
        if exists
            then liftIO (T.readFile (toFilePath path))
            else throwM (FailedToLoadTemplate name (toFilePath path))
    relRequest :: MonadThrow n => Path Rel File -> n Request
    relRequest rel = parseUrl (defaultTemplateUrl <> "/" <> toFilePath rel)
    downloadTemplate :: Request -> Path Abs File -> m Text
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
    :: (MonadIO m, MonadThrow m, MonadCatch m, MonadReader r m, HasConfig r, MonadLogger m)
    => PackageName
    -> TemplateName
    -> Map Text Text
    -> Path Abs Dir
    -> Text
    -> m (Map (Path Abs File) LB.ByteString)
applyTemplate project template nonceParams dir templateText = do
    config <- asks getConfig
    currentYear <- do
      now <- liftIO getCurrentTime
      (year, _, _) <- return $ toGregorian . utctDay $ now
      return $ T.pack . show $ year
    let context = M.union (M.union nonceParams extraParams) configParams
          where
            extraParams = M.fromList [ ("name", packageNameText project)
                                     , ("year", currentYear) ]
            configParams = configTemplateParams config
    (applied,missingKeys) <-
        runWriterT
            (hastacheStr
                 defaultConfig { muEscapeFunc = id }
                 templateText
                 (mkStrContextM (contextFunction context)))
    unless (S.null missingKeys)
         ($logInfo (T.pack (show (MissingParameters project template missingKeys (configUserConfigPath config)))))
    files :: Map FilePath LB.ByteString <-
        catch (execWriterT $
               yield (T.encodeUtf8 (LT.toStrict applied)) $$
               unpackTemplate receiveMem id
              )
              (\(e :: ProjectTemplateException) ->
                   throwM (InvalidTemplate template (show e)))
    when (M.null files) $
         throwM (InvalidTemplate template "Template does not contain any files")
    unless (any (".cabal" `isSuffixOf`) . M.keys $ files) $
         throwM (InvalidTemplate template "Template does not contain a .cabal\
                                          \ file")
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
    :: (MonadIO m, MonadReader r m, HasConfig r, MonadLogger m, MonadCatch m)
    => Path Abs Dir -> m ()
runTemplateInits dir = do
    menv <- getMinimalEnvOverride
    config <- asks getConfig
    case configScmInit config of
        Nothing -> return ()
        Just Git ->
            catch (callProcess $ Cmd (Just dir) "git" menv ["init"])
                  (\(_ :: ProcessExitedUnsuccessfully) ->
                         $logInfo "git init failed to run, ignoring ...")

--------------------------------------------------------------------------------
-- Getting templates list

listTemplates
    :: (MonadIO m, MonadThrow m, MonadReader r m, HasHttpManager r, MonadCatch m, MonadLogger m)
    => m ()
listTemplates = do
    templates <- getTemplates
    mapM_ ($logInfo . templateName) (S.toList templates)

-- | Get the set of templates.
getTemplates
    :: (MonadIO m, MonadThrow m, MonadReader r m, HasHttpManager r, MonadCatch m)
    => m (Set TemplateName)
getTemplates = do
    req <- liftM addHeaders (parseUrl defaultTemplatesList)
    resp <- catch (httpLbs req) (throwM . FailedToDownloadTemplates)
    case statusCode (responseStatus resp) of
        200 ->
            case eitherDecode (responseBody resp) >>=
                 parseEither parseTemplateSet of
                Left err -> throwM (BadTemplatesJSON err (responseBody resp))
                Right value -> return value
        code -> throwM (BadTemplatesResponse code)
  where
    addHeaders req =
        req
        { requestHeaders = [ ("User-Agent", "The Haskell Stack")
                           , ("Accept", "application/vnd.github.v3+json")] <>
          requestHeaders req
        }

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
    | BadTemplatesJSON !String !LB.ByteString
    | AlreadyExists !(Path Abs Dir)
    | MissingParameters !PackageName !TemplateName !(Set String) !(Path Abs File)
    | InvalidTemplate !TemplateName !String
    deriving (Typeable)

instance Exception NewException

instance Show NewException where
    show (FailedToLoadTemplate name path) =
        "Failed to load download template " <> T.unpack (templateName name) <>
        " from " <>
        path
    show (FailedToDownloadTemplate name (RedownloadFailed _ _ resp)) =
        case statusCode (responseStatus resp) of
            404 ->
                "That template doesn't exist. Run `stack templates' to see a list of available templates."
            code ->
                "Failed to download template " <> T.unpack (templateName name) <>
                ": unknown reason, status code was: " <>
                show code
    show (FailedToDownloadTemplate name _) =
        "Failed to download template " <> T.unpack (templateName name) <>
        ", reason unknown."
    show (AlreadyExists path) =
        "Directory " <> toFilePath path <> " already exists. Aborting."
    show (FailedToDownloadTemplates ex) =
        "Failed to download templates. The HTTP error was: " <> show ex
    show (BadTemplatesResponse code) =
        "Unexpected status code while retrieving templates list: " <> show code
    show (BadTemplatesJSON err bytes) =
        "Github returned some JSON that couldn't be parsed: " <> err <> "\n\n" <>
        L8.unpack bytes
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
