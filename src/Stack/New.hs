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
    , TemplateName
    ) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Writer.Strict
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import           Data.Typeable
import           Network.HTTP.Client (Response(..))
import           Network.HTTP.Download
import           Network.HTTP.Types.Status
import           Path
import           Path.IO
import           Stack.Constants
import           Stack.Types
import           Stack.Types.TemplateName
import           Text.Hastache
import           Text.Hastache.Context
import           Text.ProjectTemplate

--------------------------------------------------------------------------------
-- Main project creation

-- | Options for creating a new project.
data NewOpts = NewOpts
    { newOptsProjectName  :: PackageName   -- ^ Name of the project to create.
    , newOptsTemplate     :: TemplateName  -- ^ Name of the template to use.
    }

-- | Create a new project with the given options.
new
    :: (HasConfig r, MonadReader r m, MonadLogger m, MonadCatch m, MonadThrow m, MonadIO m, HasHttpManager r)
    => NewOpts -> m (Path Abs Dir)
new opts = do
    pwd <- getWorkingDir
    relDir <- parseRelDir (packageNameString (newOptsProjectName opts))
    absDir <- liftM (pwd </>) (return relDir)
    exists <- dirExists absDir
    if exists
        then throwM (AlreadyExists absDir)
        else do
            logUsing relDir
            templateText <- loadTemplate template
            files <- applyTemplate project absDir templateText
            writeTemplateFiles files
            return absDir
  where
    template = newOptsTemplate opts
    project = newOptsProjectName opts
    logUsing relDir =
        $logInfo
            ("Downloading template \"" <> templateName template <>
             "\" to create project \"" <>
             packageNameText project <>
             "\" in " <>
             T.pack (toFilePath relDir) <>
             " ...")

-- | Download and read in a template's text content.
loadTemplate
    :: (HasConfig r, HasHttpManager r, MonadReader r m, MonadIO m, MonadThrow m, MonadCatch m)
    => TemplateName -> m Text
loadTemplate name = do
    req <-
        parseUrl (defaultTemplateUrl <> "/" <> toFilePath (templatePath name))
    config <- asks getConfig
    let path = templatesDir config </> templatePath name
    _ <- catch (redownload req path) (throwM . FailedToDownloadTemplate name)
    exists <- fileExists path
    if exists
        then liftIO (T.readFile (toFilePath path))
        else throwM (FailedToLoadTemplate name path)

-- | Apply and unpack a template into a directory.
applyTemplate
    :: (MonadIO m, MonadThrow m)
    => PackageName -> Path Abs Dir -> Text -> m (Map (Path Abs File) LB.ByteString)
applyTemplate project dir template = do
    applied <-
        hastacheStr defaultConfig template (mkStrContext contextFunction)
    files :: Map FilePath LB.ByteString <-
        execWriterT $
        yield (T.encodeUtf8 (LT.toStrict applied)) $$
        unpackTemplate receiveMem id
    liftM
        M.fromList
        (mapM
             (\(fp,bytes) ->
                   do path <- parseRelFile fp
                      return (dir </> path, bytes))
             (M.toList files))
  where contextFunction :: String -> MuType m
        contextFunction "name" = MuVariable (packageNameString project)
        contextFunction _      = MuNothing

-- | Write files to the new project directory.
writeTemplateFiles
    :: MonadIO m
    => Map (Path Abs File) LB.ByteString -> m ()
writeTemplateFiles files = do
    forM_
        (M.toList files)
        (\(fp,bytes) ->
              do createTree (parent fp)
                 liftIO (LB.writeFile (toFilePath fp) bytes))

--------------------------------------------------------------------------------
-- Defaults

-- | The default template name you can use if you don't have one.
defaultTemplateName :: TemplateName
defaultTemplateName = $(mkTemplateName "new-template")

-- | Default web root URL to download from.
defaultTemplateUrl :: String
defaultTemplateUrl =
    "https://raw.githubusercontent.com/commercialhaskell/stack-templates/master"

--------------------------------------------------------------------------------
-- Exceptions

-- | Exception that might occur when making a new project.
data NewException
    = FailedToLoadTemplate !TemplateName
                           !(Path Abs File)
    | FailedToDownloadTemplate !TemplateName
                               !DownloadException
    | AlreadyExists !(Path Abs Dir)
    deriving (Typeable)

instance Exception NewException

instance Show NewException where
    show (FailedToLoadTemplate name path) =
        "Failed to load download template " <> T.unpack (templateName name) <>
        " from " <>
        toFilePath path
    show (FailedToDownloadTemplate name (RedownloadFailed _ _ resp)) =
        "Failed to download template " <> T.unpack (templateName name) <> ": " <>
        case statusCode (responseStatus resp) of
            404 -> "doesn't exist"
            code -> "unknown reason, status code was: " <> show code
    show (FailedToDownloadTemplate name _) =
        "Failed to download template " <> T.unpack (templateName name) <>
        ", reason unknown."
    show (AlreadyExists path) =
        "Directory " <> toFilePath path <> " already exists. Aborting."
