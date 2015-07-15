{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Stack.New
    ( newProject
    , NewOpts(..)
    ) where

import           Control.Monad          (filterM, forM_, unless)
import           Control.Monad.Catch    (MonadCatch, catch)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger   (MonadLogger, logInfo, logDebug)
import           Control.Monad.Reader   (MonadReader, asks)
import           Control.Monad.Trans.Writer (execWriterT)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Lazy   as LByteString
import           Data.Conduit           (($$), yield)
import qualified Data.Map               as Map
import           Data.Monoid            ((<>))
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Data.Text.Lazy         as LText
import           Network.HTTP.Client    (HttpException, getUri)
import           Network.HTTP.Download  (parseUrl, redownload, HasHttpManager)
import           Path                   (parseRelFile, mkRelDir, toFilePath, (</>))
import           Path.IO                (fileExists)
import           System.Directory       (createDirectoryIfMissing,
                                         doesFileExist,
                                         getCurrentDirectory)
import           System.FilePath        (takeDirectory,
                                         takeFileName,
                                         dropTrailingPathSeparator)
import           Text.Hastache
import           Text.Hastache.Context
import           Text.ProjectTemplate   (unpackTemplate, receiveMem)

import           Stack.Init (InitOpts(forceOverwrite))
import           Stack.Types.Config (HasStackRoot, getStackRoot)

data NewOpts = NewOpts
    { newOptsTemplateRepository :: String
    , newOptsTemplate :: Maybe Template
    , newOptsTemplateArgs :: [String]
    , newOptsInitOpts :: InitOpts
    }

type Template = String

defaultTemplate :: Template
defaultTemplate = "new-template"

-- Get the files associated with a given template as a single ByteString.
-- Templates are expected to be in "project-template" format.
getFiles :: (MonadIO m, MonadLogger m, MonadCatch m, MonadReader env m, HasStackRoot env, HasHttpManager env)
         => String -> Template -> m ByteString
getFiles urlBase template = do
    -- TODO(DanBurton): gracefully handle absence of trailing slash in urlBase.
    -- TODO(DanBurton): gracefully handle urls with https:// already present.
    let url = urlBase <> template <> ".hsfiles"
    req <- parseUrl ("https://" <> url)

    stackRoot <- asks getStackRoot
    relFile <- parseRelFile url
    let path = stackRoot </> $(mkRelDir "templates") </> relFile

    let uriString = show $ getUri req
    $logDebug "Attempting to redownload template"
    downloaded <- redownload req path `catch` \(e :: HttpException) -> do
        $logDebug $ "redownload failed for " <> Text.pack uriString
        $logDebug $ "HttpException: " <> Text.pack (show e)
        return False
    exists <- fileExists path
    unless exists $ error $ unlines
        $ "Failed to download template:"
        : uriString
        : []
    unless downloaded $ do
        $logDebug "Using already-downloaded template."

    liftIO $ ByteString.readFile (toFilePath path)

-- Detect default key:value pairs for mustache template.
getDefaultArgs :: (MonadIO m, MonadLogger m) => m [(String, String)]
getDefaultArgs = do
    currentDirectory <- liftIO getCurrentDirectory
    let name = takeFileName $ dropTrailingPathSeparator currentDirectory
    return [("name", name)]

-- Take a list of strings of the form "key:val" and turn into a list of tuples.
toArgs :: [String] -> [(String, String)]
toArgs = map toArg
  where
    toArg s = case break (== ':') s of
        (key, ':':val) -> (key, val)
        _-> (s, "") -- TODO(DanBurton): Handle this error case better.

newProject :: (MonadIO m, MonadLogger m, MonadCatch m, MonadReader env m, HasStackRoot env, HasHttpManager env)
           => NewOpts
           -> m ()
newProject newOpts = do
    let NewOpts repo templateMay args0 initOpts = newOpts

    $logDebug "Calculating template arguments"
    defaultArgs <- getDefaultArgs

    -- TODO(DanBurton): Do this logic in the arg parser instead.
    let (template, args1) = case templateMay of
            Nothing -> (defaultTemplate, args0)
            -- If the "template" arg has a colon, treat as arg instead.
            Just template0 -> case break (== ':') template0 of
               (_, []) -> (template0, args0)
               (_key, _colonVal) -> (defaultTemplate, template0:args0)

    -- Note: this map prefers user-specified args over defaultArgs.
    let args = Map.union
            (Map.fromList $ toArgs args1)
            (Map.fromList defaultArgs)

    $logDebug "Loading template files"
    filesBS <- getFiles repo template

    let contextLookup key = case Map.lookup key args of
            Just val -> MuVariable val
            Nothing  -> MuNothing

    $logDebug "Rendering templates"
    -- There is some unsafety in this regarding file names,
    -- because interpolated file names could collide.
    -- I believe the correct way to handle this is to tell template creators
    -- to be careful to avoid this if they use mustache in file names.
    -- ~ Dan Burton
    filesLText <- hastacheStr
        defaultConfig
        (Text.decodeUtf8 filesBS)
        (mkStrContext contextLookup)
    let filesText = LText.toStrict filesLText

    files <- execWriterT
         $ yield (Text.encodeUtf8 filesText)
        $$ unpackTemplate receiveMem id

    $logDebug "Checking presence of template files"
    exist <- filterM (liftIO . doesFileExist) (Map.keys files)
    unless (forceOverwrite initOpts || null exist) $
       error $ unlines
           $ "The following files already exist, refusing to overwrite (no --force):"
           : map ("- " ++) exist

    $logDebug "Writing template files"
    $logInfo ""

    forM_ (Map.toList files) $ \(fp, lbs) -> do
        $logInfo $ Text.pack $ "Writing: " ++ fp
        liftIO $ do
            createDirectoryIfMissing True $ takeDirectory fp
            LByteString.writeFile fp lbs
