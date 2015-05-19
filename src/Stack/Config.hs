{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | The general Stackage configuration that starts everything off. This should
-- be smart to falback if there is no stackage.config, instead relying on
-- whatever files are available.
--
-- If there is no stackage.config, and there is a cabal.config, we
-- read in those constraints, and if there's a cabal.sandbox.config,
-- we read any constraints from there and also find the package
-- database from there, etc. And if there's nothing, we should
-- probably default to behaving like cabal, possibly with spitting out
-- a warning that "you should run `stk init` to make things better".
module Stack.Config
  ( Config(..)
  , ConfigException(..)
  , configInDocker
  , configBinPaths
  , Docker(..)
  , Mount(..)
  , HasConfig (..)
  , MonadReader
  , ask
  , loadConfig
  , getDocker
  , NotYetImplemented(..)
  , dockerRepoOwnerArgName
  , dockerRepoArgName
  , dockerRepoSuffixArgName
  , dockerImageTagArgName
  , dockerImageArgName
  , dockerRegistryLoginArgName
  , dockerRegistryUsernameArgName
  , dockerRegistryPasswordArgName
  , dockerAutoPullArgName
  , dockerDetachArgName
  , dockerPersistArgName
  , dockerContainerNameArgName
  , dockerRunArgsArgName
  , dockerMountArgName
  , dockerPassHostArgName
  , configPackageIndex
  , configPackageIndexGz
  , configPackageTarball
  , askConfig
  , askLatestSnapshotUrl
  , askPackageIndexGitUrl
  , askPackageIndexHttpUrl
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger hiding (Loc)
import           Control.Monad.Reader (MonadReader, ask)
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString as S (readFile)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import           Path
import           Path.Find
import           Stack.Types
import           System.Directory
import           System.Environment
import           System.FilePath (searchPathSeparator, (<.>))
import           System.Process

-- Some examples of stackage.config

-- (example 1)
-- build:
--   in: docker

-- (example 2)
-- build:
--   with: lts-2

-- (example 3)
-- build:
--   with: nightly-2015-06-01

-- (example 4)
-- build:
--   with:
--     ghc: /home/dan/ghc/ghc-6.12/bin
--     cabal: /home/dan/.cabal/bin
--   in:
--     sandbox: .cabal-sandbox

-- (example 5)
-- build:
--   with:
--     ghc: '7.10'
--     cabal: detect

-- An uninterpreted representation of configuration options.
-- Configurations may be "cascaded" using mappend (left-biased).
data StackageConfig =
  StackageConfig
    { stackageConfigStackageOpts   :: !StackageOpts
    , stackageConfigBuildOpts      :: !BuildOpts
    , stackageConfigDockerOpts     :: !DockerOpts
    , stackageConfigDir            :: !(Maybe (Path Abs Dir))
    , stackageConfigUrls           :: !(Map Text Text)
    , stackageConfigGpgVerifyIndex :: !(Maybe Bool)
    }
  deriving Show

-- | Dummy type to support this monoid business.
newtype DockerOpts = DockerOpts (Maybe Docker)
  deriving (Show)

-- | Left-biased instance.
instance Monoid DockerOpts where
  mappend (DockerOpts Nothing) x = x
  mappend x (DockerOpts Nothing) = x
  mappend x _ = x
  mempty = DockerOpts Nothing

-- Note that the Maybe mappend upon two Justs
-- will mappend their contents.
appendOf :: Monoid b => (a -> b) -> a -> a -> b
appendOf getter l r = getter l <> getter r

-- For when you want to left-prefer,
-- rather than inner mappend.
altOf :: (a -> Maybe b) -> a -> a -> Maybe b
altOf getter l r = getter l <|> getter r

instance Monoid StackageConfig where
  mempty = StackageConfig
    { stackageConfigStackageOpts = mempty
    , stackageConfigBuildOpts = mempty
    , stackageConfigDockerOpts = mempty
    , stackageConfigDir = Nothing
    , stackageConfigUrls = mempty
    , stackageConfigGpgVerifyIndex = Nothing
    }
  mappend l r = StackageConfig
    { stackageConfigStackageOpts = appendOf stackageConfigStackageOpts l r
    , stackageConfigBuildOpts = appendOf stackageConfigBuildOpts l r
    , stackageConfigDockerOpts = stackageConfigDockerOpts l <> stackageConfigDockerOpts r
    , stackageConfigDir = stackageConfigDir l <|> stackageConfigDir r
    , stackageConfigUrls = stackageConfigUrls l <> stackageConfigUrls r
    , stackageConfigGpgVerifyIndex = stackageConfigGpgVerifyIndex l <|> stackageConfigGpgVerifyIndex r
    }

instance FromJSON (Path Abs Dir -> StackageConfig) where
  parseJSON =
    withObject "StackageConfig" $
    \obj ->
      do stackageConfigStackageOpts <- obj .:? "stackage" .!= mempty
         getBuildOpts <- obj .:? "build" .!= mempty
         getTheDocker <- obj .:? "docker"
         stackageConfigUrls <- obj .:? "urls" .!= mempty
         stackageConfigGpgVerifyIndex <- obj .:? "gpg-verify-index"
         return (\parentDir ->
                   let stackageConfigBuildOpts = getBuildOpts parentDir
                       stackageConfigDir =
                         Just parentDir
                       stackageConfigDockerOpts = DockerOpts (fmap ($ parentDir) getTheDocker)
                   in StackageConfig {..})

data StackageOpts =
  StackageOpts
    { stackageOptsRoot :: !(Maybe (Path Abs Dir))
    , stackageOptsHost :: !(Maybe Text)
    }
  deriving Show

instance Monoid StackageOpts where
  mempty = StackageOpts
    { stackageOptsRoot = Nothing
    , stackageOptsHost = Nothing
    }
  mappend l r = StackageOpts
    { stackageOptsRoot = altOf stackageOptsRoot l r
    , stackageOptsHost = altOf stackageOptsHost l r
    }

instance FromJSON StackageOpts where
  parseJSON =
    withObject "StackageOpts" $
    \obj ->
      do stackageOptsRoot <-
           obj .:? "root" >>=
           \x ->
             case x of
               Nothing -> return Nothing
               Just x' ->
                 case parseAbsDir x' of
                   Nothing ->
                     fail "Unable to parse valid absolute root directory."
                   Just dir -> return (Just dir)
         stackageOptsHost <- obj .:? "host"
         return StackageOpts {..}

data BuildOpts =
  BuildOpts
    { buildOptsIn :: !(Maybe BuildIn)
    , buildOptsWith :: !(Maybe BuildWith)
    , buildOptsPackages :: !(Set (Path Abs Dir))
    , buildOptsFlags :: !(Map FlagName Bool)
    , buildOptsPackageFlags :: !(Map PackageName (Map FlagName Bool))
    }
  deriving Show

instance Monoid BuildOpts where
  mempty = BuildOpts
   { buildOptsIn           = Nothing
   , buildOptsWith         = Nothing
   , buildOptsPackages     = mempty
   , buildOptsFlags        = mempty
   , buildOptsPackageFlags = mempty
   }
  mappend l r = BuildOpts
    { buildOptsIn = altOf buildOptsIn l r
    , buildOptsWith = altOf buildOptsWith l r
    , buildOptsPackages = buildOptsPackages l <> buildOptsPackages r
    , buildOptsFlags = buildOptsFlags l <> buildOptsFlags r
    , buildOptsPackageFlags = buildOptsPackageFlags l <> buildOptsPackageFlags r
    }

instance FromJSON (Path Abs Dir -> BuildOpts) where
  parseJSON =
    withObject "BuildOpts" $
    \obj ->
      do buildOptsIn <- obj .:? "in"
         buildOptsWith <- obj .:? "with"
         packages <-
           do ps <- obj .:? "packages" .!= []
              fmap S.fromList
                   (mapM (\x ->
                            if x == "."
                               then return (Left ())
                               else fmap Right
                                         (either (fail . ("Unable to parse relative directory location: " ++) .
                                                         show)
                                                 return
                                                 (parseRelDir x)))
                         ps)
         buildOptsFlags <-
           fmap (fromMaybe mempty)
                (obj .:? "flags")
         buildOptsPackageFlags <-
           fmap (M.fromList .
                 mapMaybe (\(name,x) ->
                             do name' <- parsePackageNameFromString name
                                return (name',x)) .
                 M.toList)
                (fmap (fromMaybe mempty)
                      (obj .:? "package-flags"))
         return (\parentDir ->
                   let buildOptsPackages =
                         S.map (\x ->
                                  case x of
                                    Left () -> parentDir
                                    Right p ->
                                      (parentDir </> p))
                               packages
                   in BuildOpts {..})

newtype BuildIn = BuildIn Text
  deriving Show

instance FromJSON BuildIn where
  parseJSON = withText "BuildIn" $ \t ->
    pure $ BuildIn t

data BuildWith
  = BuildWithSnapshot !SnapshotEnv
  | BuildWithCustom !CustomEnvBins
  deriving Show

data CustomEnvBins
  = CustomEnvBins !(Map Text Text)
  deriving Show

newtype SnapshotEnv = SnapshotEnv Text
  deriving Show


instance FromJSON BuildWith where
  parseJSON j =
        (BuildWithSnapshot <$> parseJSON j)
    <|> (BuildWithCustom   <$> parseJSON j)
    <|> typeMismatch "BuildWith" j

instance FromJSON SnapshotEnv where
  parseJSON = withText "SnapshotEnv" $ \t ->
    pure $ SnapshotEnv t

instance FromJSON CustomEnvBins where
  parseJSON j =
        (CustomEnvBins <$> parseJSON j)
    <|> typeMismatch "CustomEnvBins" j


-- TODO: use where.exe if on Windows?
detectGhcLocation :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m (Path Abs Dir)
detectGhcLocation = do
  whichGhc <- liftIO $ readProcess "which" ["ghc"] ""
  parent `liftM` parseAbsFile whichGhc

detectCabalLocation :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m (Path Abs Dir)
detectCabalLocation = do
  whichCabal <- liftIO $ readProcess "which" ["cabal"] ""
  parent `liftM` parseAbsFile whichCabal


detectPackageDbLocation :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m (Path Abs Dir)
detectPackageDbLocation = do
  mPackageDb <- liftIO $ parsePackageDb
  case mPackageDb of
    Just dbText -> do
      parseAbsDir $ Text.unpack dbText
    Nothing -> throwM $ NotYetImplemented "detectPackageDbLocation: no cabal.sandbox.config"


parsePackageDb :: IO (Maybe Text)
parsePackageDb = do
  cabalSandboxConfigExists <- doesFileExist "cabal.sandbox.config"
  if cabalSandboxConfigExists
    then do
      t <- Text.readFile "cabal.sandbox.config"
      let packageDbLine = Text.stripPrefix "package-db: "
      return $ listToMaybe $ mapMaybe packageDbLine $ Text.lines t
    else
      return Nothing

getEnvFileStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m StackageConfig
getEnvFileStackageConfig = liftIO (lookupEnv "STACKAGE_CONFIG") >>= \case
  Just (parseAbsFile -> Just configFilePath) -> getFileStackageConfig configFilePath
  _ -> return mempty

getGlobalStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m StackageConfig
getGlobalStackageConfig = liftIO (lookupEnv "STACKAGE_GLOBAL_CONFIG") >>= \case
  Just (parseAbsFile -> Just configFilePath) -> getFileStackageConfig configFilePath
  _ -> getFileStackageConfig defaultStackageGlobalConfig

-- TODO: What about Windows?
-- FIXME: This will not build on Windows. (Good!)
defaultStackageGlobalConfig :: Path Abs File
defaultStackageGlobalConfig = $(mkAbsFile "/etc/stackage/config")

getStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m StackageConfig
getStackageConfig = do
  envStackageConfig <- getEnvStackageConfig
  envFileStackageConfig <- getEnvFileStackageConfig
  localStackageConfig <- getLocalStackageConfig

  let config0 = envStackageConfig <> envFileStackageConfig <> localStackageConfig

  configRootDef <- case stackageOptsRoot (stackageConfigStackageOpts config0) of
    -- Root def already present, don't do the work to find the default
    Just _ -> return mempty
    -- No root def so far, do the work to find the default
    Nothing -> do
      dir <- liftIO $ getAppUserDataDirectory "stackage" >>= parseAbsDir
      return mempty
        { stackageConfigStackageOpts = mempty
          { stackageOptsRoot = Just dir
          }
        }

  let config1 = config0 <> configRootDef
      -- The above ensures this is safe
      Just stackageRoot = stackageOptsRoot (stackageConfigStackageOpts config1)

  rootConfig <- getFileStackageConfig (stackageRoot </> stackageDotConfig)
  globalStackageConfig <- getGlobalStackageConfig
  return $ config1 <> rootConfig <> globalStackageConfig

-- | Get the current directory's @stackage.config@ file.
getLocalStackageConfig :: (MonadLogger m,MonadIO m,MonadThrow m)
                       => m StackageConfig
getLocalStackageConfig =
  do pwd <-
       liftIO (getCurrentDirectory >>= parseAbsDir)
     $logDebug ("Current directory is: " <>
                T.pack (show pwd))
     let filePath = pwd </> stackageDotConfig
     $logDebug ("Reading from config file: " <>
                T.pack (show filePath))
     getFileStackageConfig filePath

-- | Parse configuration from the specified file, or return an empty
-- config if it doesn't exist.
getFileStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Path Abs File -> m StackageConfig
getFileStackageConfig configFilePath =
  do exists <-
       liftIO (doesFileExist (toFilePath configFilePath))
     if exists
        then do mconf <-
                  liftM Yaml.decodeEither (liftIO (S.readFile (toFilePath configFilePath)))
                case mconf of
                  Right c ->
                    $logDebug ("Parsed config: " <>
                               T.pack (show (c dir)))
                  Left err ->
                    $logDebug ("Parsing config file failed at " <>
                               T.pack (show configFilePath) <>
                               ": " <>
                               T.pack err)
                return $
                  either (const mempty) ($ dir) mconf
        else do $logDebug ("No config file exist at " <>
                           T.pack (show configFilePath))
                return mempty
  where dir = parent configFilePath

lookupEnvText :: String -> IO (Maybe Text)
lookupEnvText var = fmap Text.pack <$> lookupEnv var

getEnvStackageOpts :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Maybe (Path Abs Dir) -> m StackageOpts
getEnvStackageOpts stackageRoot = liftIO $ do
  stackageHost <- lookupEnvText "STACKAGE_HOST"
  return mempty
    { stackageOptsRoot = stackageRoot
    , stackageOptsHost = stackageHost
    }

getEnvBuildOpts :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m BuildOpts
getEnvBuildOpts = liftIO $ do
  buildIn <- lookupEnvText "STACKAGE_BUILD_IN"
  return mempty
    { buildOptsIn = BuildIn <$> buildIn
    }

-- TODO: support build opts in the env
-- Loads stackage.config options from environment variables.
getEnvStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => m StackageConfig
getEnvStackageConfig = do
  dir <- liftIO (lookupEnv "STACKAGE_ROOT" >>= maybe (return Nothing) (fmap Just . parseAbsDir))
  stackageOpts <- getEnvStackageOpts dir
  buildOpts <- getEnvBuildOpts
  return mempty
    { stackageConfigStackageOpts = stackageOpts
    , stackageConfigBuildOpts = buildOpts
    , stackageConfigDir = dir
    }

-- Interprets StackageConfig options.
configFromStackageConfig :: (MonadLogger m, MonadIO m, MonadThrow m)
  => StackageConfig -> m Config
configFromStackageConfig StackageConfig{..} =
  do let StackageOpts{..} = stackageConfigStackageOpts
     configStackageHost <- resolveStackageHost stackageOptsHost
     configStackageRoot <-
       maybe (error "No stackage root.") return stackageOptsRoot -- FIXME: This is not good.
     let BuildOpts{..} = stackageConfigBuildOpts
     configBuildIn <- resolveBuildIn buildOptsIn
     (configGhcBinLocation,configCabalBinLocation,configPkgDbLocation) <-
       resolveBuildWith configStackageRoot buildOptsWith
     configDocker <-
       return (case stackageConfigDockerOpts of
                 DockerOpts x -> x)
     configFlags <- return buildOptsFlags
     configPackageFlags <- return buildOptsPackageFlags
     configPackages <- return buildOptsPackages
     configDir <-
       maybe (error "Couldn't determine config dir.") return stackageConfigDir -- FIXME: Proper exception.
     let configUrls = stackageConfigUrls
         configGpgVerifyIndex = fromMaybe False stackageConfigGpgVerifyIndex
     return Config {..}

-- | Get docker configuration. Currently only looks in current/parent
-- dirs, not, e.g. $HOME/.stackage.
--
-- TODO: Look in other locations.
getDocker :: (MonadIO m,MonadLogger m,MonadThrow m) => m Docker
getDocker =
  do mdocker <- getDockerLocal
     case mdocker of
       Just docker -> return docker
       Nothing -> throwM ConfigNoDockerConfig

-- | Get local directory docker configuration. Searches upwards for
-- the first parent containing the config file.
getDockerLocal :: (MonadIO m,MonadLogger m,MonadThrow m)
               => m (Maybe Docker)
getDockerLocal =
  do pwd <-
       liftIO (getCurrentDirectory >>= parseAbsDir)
     $logDebug ("Current directory is: " <>
                T.pack (show pwd))
     mfile <- findFileUp pwd ((== stackageDotConfig) . filename) Nothing
     case mfile of
       Nothing -> throwM ConfigNoFile
       Just file -> do $logDebug ("Reading from config file: " <>
                                  T.pack (show file))
                       readDockerFrom file

-- | Read a Docker config, if there is any, from the given YAML file.
readDockerFrom :: (MonadIO m,MonadThrow m)
               => Path Abs File -> m (Maybe Docker)
readDockerFrom fp =
  do result <-
       liftM Yaml.decodeEither (liftIO (S.readFile (toFilePath fp)))
     case result of
       Left err -> throwM (ConfigInvalidYaml err)
       Right (wholeValue :: Value) ->
         case Yaml.parseEither
                (\v ->
                   do o <- parseJSON v
                      mdocker <- o .:? "docker"
                      return mdocker)
                wholeValue of
           Right (Just dockerValue) ->
             case Yaml.parseEither parseJSON dockerValue of
               Left err ->
                 throwM (ConfigInvalidYaml err)
               Right docker -> return (Just (docker (parent fp)))
           _ -> return Nothing

loadConfig :: (MonadLogger m,MonadIO m,MonadThrow m)
           => m Config
loadConfig = do
  stackageConfig <- getStackageConfig
  configFromStackageConfig stackageConfig

resolveBuildIn :: (MonadLogger m, MonadIO m, MonadThrow m)
  => (Maybe BuildIn) -> m Text
resolveBuildIn = return . maybe defaultBuildIn (\(BuildIn t) -> t)

defaultBuildIn :: Text
defaultBuildIn = "sandbox"

resolveBuildWith :: (MonadLogger m,MonadIO m,MonadThrow m)
                 => Path Abs Dir
                 -> (Maybe BuildWith)
                 -> m (Path Abs Dir,Path Abs Dir,Path Abs Dir) -- (ghc, cabal, package-db)
resolveBuildWith sr mbw = resolveBuildWith' sr $ fromMaybe defaultBuildWith mbw

defaultBuildWith :: BuildWith
defaultBuildWith = BuildWithCustom (CustomEnvBins mempty)

-- TODO: BuildWithSnapshot
-- TODO: handle vague things like "ghc: 7.10"
resolveBuildWith' :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Path Abs Dir -> BuildWith -> m (Path Abs Dir, Path Abs Dir, Path Abs Dir)
                                             -- (ghc,          cabal,        package-db)
resolveBuildWith' _stackageRoot (BuildWithSnapshot _) =
  throwM $ NotYetImplemented "resolveBuildWith': BuildWithSnapshot"
resolveBuildWith' _stackageRoot (BuildWithCustom (CustomEnvBins bins)) = do
  let ghcDirText = fromMaybe "detect" $ Map.lookup "ghc" bins
  let cabalDirText = fromMaybe "detect" $ Map.lookup "cabal" bins
  ghcBinLoc <- case ghcDirText of
    "detect" -> detectGhcLocation
    _ -> parseAbsDir $ Text.unpack ghcDirText
  cabalBinLoc <- case cabalDirText of
    "detect" -> detectCabalLocation
    _ -> parseAbsDir $ Text.unpack cabalDirText
  packageDbLoc <- detectPackageDbLocation
  return (ghcBinLoc, cabalBinLoc, packageDbLoc)



-- TODO: copy from stackage-sandbox or wherver this has been well defined
resolveStackageHost :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Maybe Text -> m String
resolveStackageHost = return . maybe stackageHostDefault Text.unpack

stackageHostDefault :: String
stackageHostDefault = "https://www.stackage.org"

-- | The filename used for the stackage config file.
stackageDotConfig :: Path Rel File
stackageDotConfig = $(mkRelFile "stack.config")

-- | Get the binary locations as a string that could be used in the PATH
configBinPaths :: Config -> String
configBinPaths config =
   toFilePath (configGhcBinLocation config) <>
   [searchPathSeparator] <>
   toFilePath (configCabalBinLocation config)

-- | Location of the 00-index.tar file
configPackageIndex :: Config -> Path Abs File
configPackageIndex config = configStackageRoot config </> $(mkRelFile "00-index.tar")

-- | Location of the 00-index.tar.gz file
configPackageIndexGz :: Config -> Path Abs File
configPackageIndexGz config = configStackageRoot config </> $(mkRelFile "00-index.tar.gz")

-- | Location of a package tarball
configPackageTarball :: MonadThrow m => Config -> PackageIdentifier -> m (Path Abs File)
configPackageTarball config ident = do
    name <- parseRelDir $ packageNameString $ packageIdentifierName ident
    ver <- parseRelDir $ versionString $ packageIdentifierVersion ident
    base <- parseRelFile $ packageIdentifierString ident <.> "tar.gz"
    return $ configStackageRoot config </> $(mkRelDir "packages") </> name </> ver </> base

-- | Helper function to ask the environment and apply getConfig
askConfig :: (MonadReader env m, HasConfig env) => m Config
askConfig = liftM getConfig ask

-- | Helper for looking up URLs
askUrl :: (MonadReader env m, HasConfig env)
       => Text -- ^ key
       -> Text -- ^ default
       -> m Text
askUrl key val = liftM (fromMaybe val . Map.lookup key . configUrls) askConfig

-- | Get the URL to request the information on the latest snapshots
askLatestSnapshotUrl :: (MonadReader env m, HasConfig env) => m Text
askLatestSnapshotUrl = askUrl "latest-snapshot-url" "https://www.stackage.org/download/snapshots.json"

-- | Git URL for the package index
askPackageIndexGitUrl :: (MonadReader env m, HasConfig env) => m Text
askPackageIndexGitUrl = askUrl "package-index-git-url" "https://github.com/commercialhaskell/all-cabal-hashes.git"

-- | HTTP URL for the package index
askPackageIndexHttpUrl :: (MonadReader env m, HasConfig env) => m Text
askPackageIndexHttpUrl = askUrl "package-index-http-url" "https://s3.amazonaws.com/hackage.fpcomplete.com/00-index.tar.gz"
