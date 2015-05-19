{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Stack.Setup
  ( setup
  , isSetup
  , Paths(..)
  , getPaths
  ) where

-- Copied imports from stackage-setup
---------------------------------------------------------------------
import qualified ClassyPrelude.Conduit as ClassyPrelude
import ClassyPrelude.Conduit hiding ((<>))
import Control.Applicative
import Crypto.Hash
import Crypto.Hash.Conduit (sinkHash)
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Yaml as Yaml
import Options.Applicative (Parser, strArgument, metavar, value)
import Stackage.CLI
import Network.HTTP.Client.Conduit
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hUserAgent)
import System.Directory
import System.FilePath (searchPathSeparator, getSearchPath)
import System.Environment (lookupEnv, getEnv, setEnv)
import System.Exit (exitFailure)
import System.Process (callProcess, readProcess)
import qualified Paths_stack as CabalInfo

import qualified Prelude
import Prelude (Bool(..))

-- Imports and new definitions for Stack.Setup
-------------------------------------------------------------------
import Stack.Types.Version
import qualified Stack.Config as Stack
import Path (Path, Abs, Dir)
import System.Environment (withArgs)
import Control.Monad.Logger

data SetupException
  = GhcVersionNotRecognized Version
  | AlreadySetup Version
  deriving (Show, Typeable)
instance Exception SetupException

setup :: (MonadThrow m, MonadIO m, MonadReader env m, Stack.HasConfig env, MonadLogger m)
  => Version -> m ()
setup ghcVersion = do
  b <- isSetup ghcVersion
  if b
    then do
      $logDebug ("ghc-" <> Text.pack (show ghcVersion) <> " is already set up")
    else do
      -- TODO: a better implementation
      liftIO $ withArgs ["ghc-" ++ show ghcVersion] main

isSetup :: (MonadIO m, MonadReader env m, Stack.HasConfig env, MonadLogger m)
  => Version -> m Bool
isSetup ghcVersion = do
  -- TODO: a better implementation
  liftIO $ oldIsSetupIO (show ghcVersion)

data Paths = Paths
    { binPaths :: [Path Abs Dir]
    }

-- TODO: implement
getPaths :: (MonadIO m, MonadReader env m, Stack.HasConfig env)
  => Version -> m Paths
getPaths _ghcVersion = return $ Paths []





-- Copied code from stackage-setup follows
---------------------------------------------------------------------
mainVer :: String
mainVer = $(simpleVersion CabalInfo.version)

header :: String
header = "Retrieve ghc, cabal, etc, for use with stackage"

progDesc :: String
progDesc = header

userAgent :: ByteString
userAgent = "stackage-setup"

stackageHostDefault :: String
stackageHostDefault = "https://www.stackage.org"


main :: IO ()
main = do
  (target,()) <- simpleOptions
    mainVer
    header
    progDesc
    setupTargetParser
    empty
  withManagerSettings tlsManagerSettings
    $ withConfig
    $ curryReaderT R
    $ oldSetup target

data R = R
  { rConfig :: Config
  , rManager :: Manager
  }

instance HasHttpManager R where
  getHttpManager = rManager

data Config = Config
  { configStackageRoot :: FilePath
  , configStackageHost :: String
  }

data StackageConfig = StackageConfig
  { _stackageHost :: String }

instance FromJSON StackageConfig where
  parseJSON = withObject "StackageConfig" $ \obj -> do
    _stackageHost <- obj .:? "stackage-host" .!= stackageHostDefault
    return StackageConfig{..}

-- Check if given processes appear to be present.
-- Halts the program with exit failure if any are missing.
-- TODO: make cross platform
checkDependencies :: [String] -> IO ()
checkDependencies deps = do
  missing <- mapM checkIsMissing deps
  when (or missing) $ do
    hPutStrLn stderr $ asText $
      "Please install missing dependencies and try again."
    exitFailure

-- return True if it appears to be missing
checkIsMissing :: String -> IO Bool
checkIsMissing process = do
  isMissing <- procIsMissing process
    `catch` \(_ :: IOException) -> return True
  if isMissing
    then do
      hPutStrLn stderr $
        "Couldn't find required dependency: " <> process
      return True
    else return False

-- return True if it appears to be missing
procIsMissing :: String -> IO Bool
procIsMissing process = do
  output <- readProcess process ["--version"] ""
  return $ null output


getStackageRoot :: GetConfig m => m FilePath
getStackageRoot = liftM configStackageRoot getConfig

getStackageHost :: GetConfig m => m String
getStackageHost = liftM configStackageHost getConfig

class HasConfig env where
  accessConfig :: env -> Config
instance HasConfig R where
  accessConfig = rConfig

-- TODO: check environment properly
getStackageRootIO :: IO FilePath
getStackageRootIO = do
  stackageRoot <- lookupEnv "STACKAGE_ROOT" >>= \case
    Just dir -> return $ dir
    Nothing -> do
      -- TODO: windows-ify
      home <- lookupEnv "HOME" >>= \case
        Just homeStr -> return (fromString homeStr)
        Nothing -> throwIO StackageRootNotFound
      return (home </> ".stackage")
  createDirectoryIfMissing True stackageRoot
  return stackageRoot

readFileMay :: FilePath -> IO (Maybe ByteString)
readFileMay path = do
  exists <- doesFileExist path
  if exists
    then Just <$> ClassyPrelude.readFile path
    else return Nothing

getFirstJust :: [IO (Maybe a)] -> IO (Maybe a)
getFirstJust [] = return Nothing
getFirstJust (xIO:xsIO) = do
  xMay <- xIO
  case xMay of
    Just{} -> return xMay
    Nothing -> getFirstJust xsIO

getStackageHostIO :: FilePath -> IO String
getStackageHostIO stackageRoot = do
  bsMay <- getFirstJust
    [ readFileMay (stackageRoot </> "config")
    , readFileMay (stackageRoot </> "config.yaml")
    , readFileMay (stackageRoot </> "config.yml")
    ]
  case bsMay of
    Nothing -> return stackageHostDefault
    Just bs -> case Yaml.decodeEither' bs of
      Left{} -> return stackageHostDefault
      Right stackageConfig -> return (_stackageHost stackageConfig)


getConfigIO :: IO Config
getConfigIO = do
  configStackageRoot <- getStackageRootIO
  configStackageHost <- getStackageHostIO configStackageRoot
  return Config{..}

class Monad m => GetConfig m where
  getConfig :: m Config
instance GetConfig IO where
  getConfig = getConfigIO
instance (HasConfig env, Monad m)
  => GetConfig (ReaderT env m) where
  getConfig = liftM accessConfig ask


withConfig :: (MonadIO m)
  => ReaderT Config m a -> m a
withConfig m = do
  config <- liftIO getConfigIO
  runReaderT m config

curryReaderT :: (r1 -> r2 -> env) -> ReaderT env m a -> ReaderT r1 (ReaderT r2 m) a
curryReaderT tup m =
  ReaderT $ \r1 ->
  ReaderT $ \r2 ->
  runReaderT m $ tup r1 r2

arch :: String
arch = "linux64"

snapshotsReq :: (MonadThrow m, GetConfig m) => m Request
snapshotsReq = do
  stackageHost <- getStackageHost
  parseUrl $ stackageHost <> "/download/snapshots.json"

ghcMajorVersionReq :: (MonadThrow m, GetConfig m) => Snapshot -> m Request
ghcMajorVersionReq snapshot = do
  stackageHost <- getStackageHost
  parseUrl $ stackageHost <> "/snapshot/" <> snapshot <> "/ghc-major-version"

getLinksReq :: (MonadThrow m, GetConfig m) => GhcMajorVersion -> m Request
getLinksReq ghcMajorVersion = do
  stackageHost <- getStackageHost
  parseUrl $ stackageHost <> "/download/" <> arch <> "/ghc-" <> ghcMajorVersion <> "-links.yaml"

setupTargetParser :: Parser SetupTarget
setupTargetParser = strArgument mods where
  mods = metavar "TARGET" <> value "lts"

refreshSnapshots ::
  ( HasHttpManager env
  , MonadReader env m
  , GetConfig m
  , MonadThrow m
  , MonadIO m
  ) => m (HashMap String String)
refreshSnapshots = do
  stackageRoot <- getStackageRoot
  let path = stackageRoot </> snapshotsPath

  response <- httpLbs =<< snapshotsReq
  let lbs = responseBody response
  liftIO $ LByteString.writeFile path lbs

  either (throwM . ParseSnapshotsError) return $ Aeson.eitherDecode lbs


getLinks ::
  ( MonadIO m
  , MonadThrow m
  , MonadReader env m
  , GetConfig m
  , HasHttpManager env
  ) => GhcMajorVersion -> m [Download]
getLinks ghcMajorVersion = do
  stackageRoot <- getStackageRoot
  response <- httpLbs =<< getLinksReq ghcMajorVersion
  let lbs = responseBody response
      bs = LByteString.toStrict lbs
      path = stackageRoot </> linksPath ghcMajorVersion
  liftIO $ LByteString.writeFile path lbs

  either (throwM . ParseLinksError) return $ Yaml.decodeEither' bs

data Download = Download
  { downloadName :: Text
  , downloadVersion :: Text
  , downloadUrl :: String
  , downloadSha1 :: String
  , downloadInstructions :: [Text]
  }

instance FromJSON Download where
  parseJSON = withObject "Download" $ \obj -> do
    downloadName         <- obj .: "name"
    downloadVersion      <- obj .: "version"
    downloadUrl          <- obj .: "url"
    downloadSha1         <- obj .: "sha1"
    downloadInstructions <- obj .: "instructions"
    return Download{..}

type SetupTarget = String
type GhcMajorVersion = String
type Series = String
type Snapshot = String

data SetupExceptions
  = SeriesNotFound Series
  | ParseSnapshotsError String
  | ParseLinksError Yaml.ParseException
  | StackageRootNotFound
  deriving (Show, Typeable)
instance Exception SetupExceptions

readSeries :: String -> Maybe Series
readSeries s@"lts" = Just s
readSeries s@"nightly" = Just s
readSeries s@(stripPrefix "lts-" -> Just sver)
  | all Char.isNumber sver = Just s
readSeries (stripPrefix "lts/" -> Just sver)
  | all Char.isNumber sver = Just $ "lts-" <> sver
readSeries _ = Nothing

readGhcVersion :: String -> Maybe GhcMajorVersion
readGhcVersion (stripPrefix "ghc-" -> Just s) = case break (== '.') s of
  (m1, '.':m2) | all Char.isNumber m1 && all Char.isNumber m2
    -> Just s
  _ -> Nothing
readGhcVersion _ = Nothing

getGhcMajorVersion ::
  ( HasHttpManager env
  , MonadReader env m
  , GetConfig m
  , MonadThrow m
  , MonadIO m
  ) => String -> m GhcMajorVersion
getGhcMajorVersion target = case readGhcVersion target of
  Just version -> return version
  Nothing -> do
    snapshot <- case readSeries target of
      Just series -> lookupSnapshot series
      Nothing -> return target -- just try using it as a snapshot
    putStrLn $ "Setup for snapshot: " <> pack snapshot
    lookupGhcMajorVersion snapshot

lookupSnapshot ::
  ( HasHttpManager env
  , MonadReader env m
  , GetConfig m
  , MonadThrow m
  , MonadIO m
  ) => Series -> m Snapshot
lookupSnapshot series = do
  snapshots <- refreshSnapshots
  case HashMap.lookup series snapshots of
    Just snapshot -> return snapshot
    Nothing -> throwM $ SeriesNotFound series


lookupGhcMajorVersion ::
  ( HasHttpManager env
  , MonadReader env m
  , GetConfig m
  , MonadThrow m
  , MonadIO m
  ) => Snapshot -> m GhcMajorVersion
lookupGhcMajorVersion snapshot = do
  response <- httpLbs =<< ghcMajorVersionReq snapshot
  let lbs = responseBody response
  return $ unpack $ LText.toStrict $ LText.decodeUtf8 lbs


oldSetup ::
  ( HasHttpManager env
  , MonadReader env m
  , GetConfig m
  , MonadThrow m
  , MonadIO m
  , MonadBaseControl IO m
  ) => SetupTarget -> m ()
oldSetup target = do
  ghcMajorVersion <- getGhcMajorVersion target
  putStrLn $ "Selecting ghc-" <> pack ghcMajorVersion
  links <- getLinks ghcMajorVersion

  stackageRoot <- getStackageRoot
  forM_ links $ \d@Download{..} -> do
    let dir = stackageRoot </> downloadDir downloadName
    liftIO $ createDirectoryIfMissing True dir

    let versionedDir = dir </> downloadPath downloadName downloadVersion
    exists <- liftIO $ doesDirectoryExist versionedDir

    if (not exists)
      then do
        liftIO $ checkDependencies (depsFor downloadName)
        download downloadUrl downloadSha1 dir
        postDownload d dir versionedDir
      else putStrLn $ "Already have: " <> downloadName <> "-" <> downloadVersion

    augmentPath (versionedDir </> "bin")

data NotSetup = NotSetup
  deriving (Show, Typeable)
instance Exception NotSetup

oldIsSetupIO :: GhcMajorVersion -> IO Bool
oldIsSetupIO ghcMajorVersion = do
  ((withManagerSettings tlsManagerSettings
    $ withConfig
    $ curryReaderT R
    $ oldIsSetup ghcMajorVersion
   ) >> return True) `catch` \NotSetup -> return False

-- Just copied a portion of oldSetup
oldIsSetup ::
  ( HasHttpManager env
  , MonadReader env m
  , GetConfig m
  , MonadThrow m
  , MonadIO m
  , MonadBaseControl IO m
  ) => SetupTarget -> m ()
oldIsSetup ghcMajorVersion = do
  links <- getLinks ghcMajorVersion

  stackageRoot <- getStackageRoot
  forM_ links $ \_d@Download{..} -> do
    let dir = stackageRoot </> downloadDir downloadName
    liftIO $ createDirectoryIfMissing True dir

    let versionedDir = dir </> downloadPath downloadName downloadVersion
    exists <- liftIO $ doesDirectoryExist versionedDir

    when (not exists) $ throwM NotSetup
  return ()


-- The dependencies required for stackage-setup's implementation of
-- the instructions for a given download item.
depsFor :: Text -> [String]
depsFor "ghc"      = ["make", "tar", "xz"]
depsFor "cabal"    = ["tar", "gzip"]
depsFor "stackage" = ["tar", "xz"]
depsFor "alex"     = ["tar", "xz"]
depsFor "happy"    = ["tar", "xz"]
depsFor _          = []

-- TODO: ensure this works cross-platform
augmentPath :: MonadIO m => FilePath -> m ()
augmentPath pathHead = liftIO $ do
  pathRest <- getSearchPath
  let paths = pathHead : pathRest
      path = intercalate [searchPathSeparator] paths
  setEnv "PATH" path


postDownload :: (MonadIO m)
  => Download -> FilePath -> FilePath -> m ()
postDownload d@Download{..} dir versionedDir = do
  expectInstructions downloadName d dir versionedDir

expectInstructions :: (MonadIO m) => Text -> Download -> FilePath -> FilePath -> m ()
expectInstructions "ghc" = expectGhcInstructions
expectInstructions "cabal" = expectCabalInstructions
expectInstructions "stackage" = justUnpackInstructions
expectInstructions "alex" = justUnpackInstructions
expectInstructions "happy" = justUnpackInstructions
expectInstructions t = unexpectedInstructions t

unexpectedInstructions :: (MonadIO m) => Text -> Download -> FilePath -> FilePath -> m ()
unexpectedInstructions t Download{..} dir _ = do
  putStrLn $ "Unexpected download: " <> t
  when (not $ null downloadInstructions) $ do
    putStrLn $ "Manual instructions:"
    putStrLn $ "$ cd " <> Text.pack dir
    mapM_ (putStrLn . ("$ " <>)) downloadInstructions

justUnpackInstructions :: (MonadIO m) => Download -> FilePath -> FilePath -> m ()
justUnpackInstructions Download{..} dir _ = do
    liftIO $ go downloadInstructions
  where
    go :: [Text] -> IO ()
    go [] = return ()
    go ( (stripPrefix "tar xJf " -> Just file)
       : next
       ) = unzipXZ dir (Text.unpack file) >> go next
    go ( (stripPrefix "rm " -> Just _file)
       : next
       ) = go next -- already done in unzipXZ
    go (t:_) = fail $ "command not recognized: " <> unpack t


expectGhcInstructions :: (MonadIO m) => Download -> FilePath -> FilePath -> m ()
expectGhcInstructions Download{..} dir versionedDir =
    liftIO $ go downloadInstructions
  where
    go :: [Text] -> IO ()
    go [] = return ()
    go ( (stripPrefix "tar xJf " -> Just file)
       : next
       ) = unzipXZ dir (Text.unpack file) >> go next
    go ( (stripPrefix "rm ghc-" -> Just _file)
       : next
       ) = go next -- already done in unzipXZ
    go ( (stripPrefix "cd ghc-" -> Just version)
       : "./configure --prefix=`pwd`"
       : "make install"
       : "cd .."
       : next
       ) | version == downloadVersion
       = ghcConfigureInstall versionedDir >> go next
    go (t:_) = fail $ "command not recognized: " <> unpack t


inDir :: FilePath -> IO a -> IO a
inDir dir action = do
  currentDir <- getCurrentDirectory
  let enter = setCurrentDirectory dir
      exit = setCurrentDirectory currentDir
  bracket_ enter exit action


ghcConfigureInstall :: (MonadIO m) => FilePath -> m ()
ghcConfigureInstall versionedDir = liftIO $ do
  let srcDir = versionedDir <.> "src"
  whenM (doesDirectoryExist srcDir) $ removeDirectoryRecursive srcDir -- TODO: reuse instead
  renameDirectory versionedDir srcDir
  createDirectoryIfMissing True versionedDir
  let go = inDir srcDir $ do
        callProcess "./configure" ["--prefix=" <> versionedDir]
        callProcess "make" ["install"]
  go `onException` removeDirectoryRecursive versionedDir
  removeDirectoryRecursive srcDir

expectCabalInstructions :: (MonadIO m) => Download -> FilePath -> FilePath -> m ()
expectCabalInstructions Download{..} dir versionedDir =
    liftIO $ rememberingOldCabal $ go downloadInstructions
  where
    go [] = return ()
    go ( (stripPrefix "tar xzf " -> Just file)
       : next
       ) = unzipGZ dir (Text.unpack file) >> go next
    go ( (stripPrefix "rm cabal-install-" ->
          Just (stripSuffix ".tar.gz" -> Just _file))
       : next
       ) = go next -- already done in unzipGZ
    go ( (stripPrefix "cd cabal-install-" -> Just version)
       : "./bootstrap.sh"
       : "cd .."
       : (stripPrefix "rm -r cabal-install-" -> Just version')
       : next
       ) | version == downloadVersion && version == version'
       = cabalBootstrap dir version >> go next
    go ( (stripPrefix "mkdir cabal-" -> Just version)
       : (stripPrefix "mv $HOME/.cabal/bin/cabal cabal-" -> Just _newLoc)
       : next
       ) | version == downloadVersion
       = cabalMoveExe versionedDir >> go next
    go (t:_) = fail $ "command not recognized: " <> unpack t

cabalBootstrap :: FilePath -> Text -> IO ()
cabalBootstrap dir version = do
  let cabalInstallVersionedDir = dir </> Text.unpack ("cabal-install-" <> version)
  inDir cabalInstallVersionedDir $ do
    callProcess "./bootstrap.sh" []
  removeDirectoryRecursive cabalInstallVersionedDir

cabalMoveExe :: FilePath -> IO ()
cabalMoveExe versionedDir = do
  let versionedDirBin = versionedDir </> "bin"
  createDirectoryIfMissing True versionedDirBin
  homeCabal <- getHomeCabal
  renameFile homeCabal (versionedDirBin </> "cabal")

getHomeCabal :: IO FilePath
getHomeCabal = do
  -- TODO: errors
  Just home <- lookupEnv "HOME"
  return $ home </> ".cabal" </> "bin" </> "cabal"

rememberingOldCabal :: IO a -> IO a
rememberingOldCabal action = do
  homeCabal <- getHomeCabal
  exists <- doesFileExist homeCabal
  if exists
    then do
      let tempCabal = homeCabal <.> "old"
          toTemp = renameFile homeCabal tempCabal
          fromTemp = renameFile tempCabal homeCabal
      bracket_ toTemp fromTemp action
    else action

-- TODO: use Text more than String
download ::
  ( MonadIO m
  , MonadBaseControl IO m
  , MonadThrow m
  , MonadReader env m
  , HasHttpManager env
  ) => String -> String -> FilePath -> m ()
download url sha1 dir = do
  -- TODO: deal with partial function last
  let fname = List.drop (List.last slashes + 1) url
      slashes = List.findIndices (== '/') url
      file = dir </> fname
  putStrLn $ "Downloading: " <> pack fname
  req0 <- parseUrl url
  let req = req0
        { requestHeaders = [(hUserAgent, userAgent)] }
  runResourceT $ withResponse req $ \res -> do
    let source = responseBody res
    let sink = getZipSink $
          ZipSink sinkHash <* ZipSink (sinkFile file)
    theDigest <- source $$ sink
    let _ = theDigest :: Digest SHA1
    when (show theDigest /= sha1) $ do
      fail "Corrupted download"
    putStrLn $ "Verified sha1: " <> pack sha1

-- TODO: make cross platform
unzipDownload :: (MonadIO m)
  => FilePath -> FilePath -> m ()
unzipDownload dir file = case stripSuffix ".tar.xz" (Text.pack file) of
  Just{} -> unzipXZ dir file
  _ -> case stripSuffix ".tar.gz" (Text.pack file) of
    Just {} -> unzipGZ dir file
    _ -> fail $ "unzipDownload: unknown extension"
  -- TODO: other extensions

-- TODO: make cross platform
unzipXZ :: (MonadIO m)
  => FilePath -> FilePath -> m ()
unzipXZ dir file = liftIO $ inDir dir $ do
  putStrLn "Decompressing XZ archive"
  callProcess "tar"
    ["xJf"
    , file
    ]
  removeFile file


-- TODO: make cross platform
unzipGZ :: (MonadIO m)
  => FilePath -> FilePath -> m ()
unzipGZ dir file = liftIO $ inDir dir $ do
  putStrLn "Decompressing GZ archive"
  callProcess "tar"
    ["xzf"
    , file
    ]
  removeFile file

downloadDir :: Text -> FilePath
downloadDir name =
  "environment" </> Text.unpack name

downloadPath :: Text -> Text -> FilePath
downloadPath name version =
  Text.unpack (name <> "-" <> version)

snapshotsPath :: FilePath
snapshotsPath = Text.unpack "snapshots.json"

linksPath :: GhcMajorVersion -> FilePath
linksPath ghcMajorVersion = Text.unpack $ "ghc-" <> pack ghcMajorVersion <> "-links.yaml"
