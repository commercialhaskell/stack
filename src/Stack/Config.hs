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
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger hiding (Loc)
import           Control.Monad.Reader (MonadReader, ask)
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString as S (readFile)
import           Data.Default
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
import           Data.Typeable
import qualified Data.Yaml as Yaml
import           Path
import           Path.Find
import           Stack.Types
import           System.Directory
import           System.Environment
import           System.FilePath (searchPathSeparator)
import           System.Process

-- | The top-level Stackage configuration.
data Config =
  Config {configPkgDbLocation    :: !(Path Abs Dir)
         ,configGhcBinLocation   :: !(Path Abs Dir)
         ,configCabalBinLocation :: !(Path Abs Dir)
         ,configStackageRoot     :: !(Path Abs Dir)
         ,configStackageHost     :: !String
         ,configBuildIn          :: !Text
         ,configDocker           :: !(Maybe Docker)
         ,configPackages         :: !(Set (Path Abs Dir))
         ,configFlags            :: !(Map FlagName Bool)
         ,configPackageFlags     :: !(Map PackageName (Map FlagName Bool))
         ,configDir              :: !(Path Abs Dir)}
  -- ^ Flags for each package's Cabal config.
  deriving (Show)

-- | Class for environment values that can provide a 'Config'.
class HasConfig env where
    getConfig :: env -> Config
instance HasConfig Config where
    getConfig = id
    {-# INLINE getConfig #-}

configInDocker :: Config -> Bool
configInDocker = (== "docker") . configBuildIn

data ConfigException
  = ConfigInvalidYaml String
  | ConfigNoFile
  | ConfigNoDockerConfig
  deriving (Typeable,Show)
instance Exception ConfigException

-- TODO: eliminate occurrences of this exception.
data NotYetImplemented = NotYetImplemented Text
  deriving (Show, Typeable)
instance Exception NotYetImplemented

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
    { stackageConfigStackageOpts :: !StackageOpts
    , stackageConfigBuildOpts    :: !BuildOpts
    , stackageConfigDockerOpts   :: !DockerOpts
    , stackageConfigDir          :: !(Maybe (Path Abs Dir))
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
    }
  mappend l r = StackageConfig
    { stackageConfigStackageOpts = appendOf stackageConfigStackageOpts l r
    , stackageConfigBuildOpts = appendOf stackageConfigBuildOpts l r
    , stackageConfigDockerOpts = stackageConfigDockerOpts l <> stackageConfigDockerOpts r
    , stackageConfigDir = stackageConfigDir l <|> stackageConfigDir r
    }

instance FromJSON (Path Abs Dir -> StackageConfig) where
  parseJSON =
    withObject "StackageConfig" $
    \obj ->
      do stackageConfigStackageOpts <- obj .:? "stackage" .!= mempty
         getBuildOpts <- obj .:? "build" .!= mempty
         getTheDocker <- obj .:? "docker"
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

-- | Docker configuration.
data Docker =
  Docker {dockerEnable :: !Bool
           -- ^ Is using Docker enabled?
         ,dockerRepoOwner :: !String
           -- ^ Docker repository (registry and) owner
         ,dockerRepo :: !String
           -- ^ Docker repository name (e.g. @dev@)
         ,dockerRepoSuffix :: !String
           -- ^ Docker repository name's suffix (e.g. stackage ver)
         ,dockerImageTag :: !(Maybe String)
           -- ^ Optional Docker image tag (e.g. the date)
         ,dockerImage :: !(Maybe String)
           -- ^ Exact Docker image tag or ID.  Overrides docker-repo-*/tag.
         ,dockerRegistryLogin :: !Bool
           -- ^ Does registry require login for pulls?
         ,dockerRegistryUsername :: !(Maybe String)
           -- ^ Optional username for Docker registry.
         ,dockerRegistryPassword :: !(Maybe String)
           -- ^ Optional password for Docker registry.
         ,dockerAutoPull :: !Bool
           -- ^ Automatically pull new images.
         ,dockerDetach :: !Bool
           -- ^ Whether to run a detached container
         ,dockerPersist :: !Bool
           -- ^ Create a persistent container (don't remove it when finished).  Implied by
           -- `dockerDetach`.
         ,dockerContainerName :: !(Maybe String)
           -- ^ Container name to use, only makes sense from command-line with `dockerPersist`
           -- or `dockerDetach`.
         ,dockerRunArgsDefault :: ![String]
           -- ^ Arguments to pass directly to @docker run@ (from @stackage-build.config@).
         ,dockerRunArgsExtra :: ![[String]]
           -- ^ Arguments to pass directly to @docker run@ (passed on stackage-build command-line).
         ,dockerMountDefault :: ![Mount]
           -- ^ Volumes to mount in the container (from @stackage-build.config@).
         ,dockerMountExtra :: ![Mount]
           -- ^ Volumes to mount in the container (from stackage-build command-line).
         ,dockerPassHost :: !Bool
           -- ^ Pass Docker daemon connection information into container.
         ,dockerExtra :: ![String]
           -- ^ This is a placeholder for command-line argument parsing.
         ,dockerDir :: (Path Abs Dir) -- ^ Intentionally lazy because of the 'Default' instance.
         }
  deriving (Show)

-- | For YAML.
instance FromJSON (Path Abs Dir -> Docker) where
  parseJSON v =
    do o <- parseJSON v
       x <- Docker <$> o .:? dockerEnableArgName .!= True
                   <*> o .:? dockerRepoOwnerArgName .!= dockerRepoOwner def
                   <*> o .:? dockerRepoArgName .!= dockerRepo def
                   <*> o .:? dockerRepoSuffixArgName .!= dockerRepoSuffix def
                   <*> o .:? dockerImageTagArgName .!= dockerImageTag def
                   <*> o .:? dockerImageArgName
                   <*> o .:? dockerRegistryLoginArgName .!= dockerRegistryLogin def
                   <*> o .:? dockerRegistryUsernameArgName .!= dockerRegistryUsername def
                   <*> o .:? dockerRegistryPasswordArgName .!= dockerRegistryPassword def
                   <*> o .:? dockerAutoPullArgName .!= dockerAutoPull def
                   <*> o .:? dockerDetachArgName .!= dockerDetach def
                   <*> o .:? dockerPersistArgName .!= dockerPersist def
                   <*> o .:? dockerContainerNameArgName .!= dockerContainerName def
                   <*> o .:? dockerRunArgsArgName .!= dockerRunArgsDefault def
                   <*> pure (dockerRunArgsExtra def)
                   <*> o .:? dockerMountArgName .!= dockerMountDefault def
                   <*> pure (dockerMountExtra def)
                   <*> o .:? dockerPassHostArgName .!= dockerPassHost def
                   <*> pure (dockerExtra def)
       return x

-- | Default values for Docker configuration.
instance Default Docker where
  def = Docker {dockerEnable = False
               ,dockerRepoOwner = "fpco"
               ,dockerRepo = "dev"
               ,dockerRepoSuffix = ""
               ,dockerImageTag = Nothing
               ,dockerImage = Nothing
               ,dockerRegistryLogin = False
               ,dockerRegistryUsername = Nothing
               ,dockerRegistryPassword = Nothing
               ,dockerAutoPull = False
               ,dockerDetach = False
               ,dockerPersist = False
               ,dockerContainerName = Nothing
               ,dockerRunArgsDefault = []
               ,dockerRunArgsExtra = []
               ,dockerMountDefault = []
               ,dockerMountExtra = []
               ,dockerPassHost = False
               ,dockerExtra = []
               ,dockerDir = error "Docker dir not determined!"
               -- FIXME: This is ugly but I don't see an obvious
               -- better way to add the value after the fact.
               }

dockerEnableArgName :: Text
dockerEnableArgName = "enable"

dockerRepoOwnerArgName :: Text
dockerRepoOwnerArgName = "repo-owner"

dockerRepoArgName :: Text
dockerRepoArgName = "repo"

dockerRepoSuffixArgName :: Text
dockerRepoSuffixArgName = "repo-suffix"

dockerImageTagArgName :: Text
dockerImageTagArgName = "image-tag"

dockerImageArgName :: Text
dockerImageArgName = "image"

dockerRegistryLoginArgName :: Text
dockerRegistryLoginArgName = "registry-login"

dockerRegistryUsernameArgName :: Text
dockerRegistryUsernameArgName = "registry-username"

dockerRegistryPasswordArgName :: Text
dockerRegistryPasswordArgName = "registry-password"

dockerAutoPullArgName :: Text
dockerAutoPullArgName = "auto-pull"

dockerDetachArgName :: Text
dockerDetachArgName = "detach"

dockerRunArgsArgName :: Text
dockerRunArgsArgName = "run-args"

dockerMountArgName :: Text
dockerMountArgName = "mount"

dockerContainerNameArgName :: Text
dockerContainerNameArgName = "container-name"

dockerPersistArgName :: Text
dockerPersistArgName = "persist"

dockerPassHostArgName :: Text
dockerPassHostArgName = "pass-host"

-- | Docker volume mount.
data Mount = Mount String String

-- | For optparse-applicative.
instance Read Mount where
  readsPrec _ s = case break (== ':') s of
                    (a,(':':b)) -> [(Mount a b,"")]
                    (a,[]) -> [(Mount a a,"")]
                    _ -> fail "Invalid value for mount"

-- | Show instance.
instance Show Mount where
  show (Mount a b) = if a == b
                        then a
                        else concat [a,":",b]

-- | For YAML.
instance FromJSON Mount where
  parseJSON v = fmap read (parseJSON v)

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



--data BuildStrategy
--  = CabalSandbox CabalSandboxBuildStrategy
--  | Docker DockerBuildStrategy
--  deriving Show

--data CabalSandboxBuildStrategy
--  = BuildAgainstLTS LTSBuildStrategy
--  | BuildAgainstNightly NightlyBuildStrategy
--  | BuildAgainstSnapshot SnapshotBuildStrategy
--  | BuildAgainstCustom (CustomBuildStrategy Text)
--  deriving Show

--data DockerBuildStrategy =
--  DockerBuildStrategy
--  deriving Show

--data LTSBuildStrategy =
--  LTSBuildStrategy
--    { configLTS :: Text
--    }
--  deriving Show

--data NightlyBuildStrategy =
--  NightlyBuildStrategy
--    { configNightly :: Text
--    }
--  deriving Show

--data SnapshotBuildStrategy =
--  SnapshotBuildStrategy
--    { configSnapshot :: Text
--    }
--  deriving Show

--data CustomBuildStrategy a =
--  CustomBuildStrategy
--  { customGhcBinLocation :: a
--  , customCabalBinLocation :: a
--  , customSandboxLocation :: a
--  }
--  deriving Show

--instance FromJSON LTSBuildStrategy where
--  parseJSON j = parseAsText j <|> parseAsScientific j where
--    parseAsText = withText "LTSBuildStrategy" $ \t -> do
--      let configLTS = t
--      return LTSBuildStrategy{..}
--    -- allows people to not have to quote the lts field
--    parseAsScientific = withScientific "LTSBuildStrategy" $ \s -> do
--      let configLTS = Text.pack $ show s
--      return LTSBuildStrategy{..}

--instance FromJSON NightlyBuildStrategy where
--  parseJSON = withText "NightlyBuildStrategy" $ \t -> do
--    let configNightly = t
--    return NightlyBuildStrategy{..}

--instance FromJSON SnapshotBuildStrategy where
--  parseJSON = withText "SnapshotBuildStrategy" $ \t -> do
--    let configSnapshot = t
--    return SnapshotBuildStrategy{..}


--instance (a ~ Text) => FromJSON (CustomBuildStrategy a) where
--  parseJSON = withObject "CustomBuildStrategy" $ \obj -> do
--    customGhcBinLocation <- obj .: "ghc" .!= "detect"
--    customCabalBinLocation <- obj .: "cabal" .!= "detect"
--    customSandboxLocation <- obj .:? "sandbox" .!= "detect"
--    return CustomBuildStrategy{..}


--instance FromJSON DockerBuildStrategy where
--  parseJSON = withBool "DockerBuildStrategy" $ \b ->
--    if b
--      then return DockerBuildStrategy
--      else fail "docker: false"

--instance FromJSON CabalSandboxBuildStrategy where
--  parseJSON = withObject "CabalSandboxBuildStrategy" $ \obj ->
--    (BuildAgainstLTS <$> obj .: "lts") <|>
--    (BuildAgainstNightly <$> obj .: "nightly") <|>
--    (BuildAgainstSnapshot <$> obj .: "snapshot") <|>
--    (BuildAgainstCustom <$> obj .: "custom")

--parseBuildStrategy :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => StackageRoot -> CabalSandboxBuildStrategy -> m (CustomBuildStrategy (Path Abs Dir))
--parseBuildStrategy stackageRoot (BuildAgainstLTS lts) =
--  resolveLTSSnapshot stackageRoot lts >>= parseBuildStrategy stackageRoot . BuildAgainstSnapshot
--parseBuildStrategy stackageRoot (BuildAgainstNightly nightly) =
--  resolveNightlySnapshot nightly >>= parseBuildStrategy stackageRoot . BuildAgainstSnapshot
--parseBuildStrategy stackageRoot (BuildAgainstSnapshot snapshot) = do
--  customGhcBinLocation <- resolveSnapshotGhcLoc stackageRoot snapshot
--  customCabalBinLocation <- resolveSnapshotCabalLoc stackageRoot snapshot
--  customSandboxLocation <- resolveSnapshotSandboxLoc stackageRoot snapshot
--  return CustomBuildStrategy{..}
--parseBuildStrategy _stackageRoot (BuildAgainstCustom custom) = do
--  ghcBinLoc <- resolveCustomGhcLoc (customGhcBinLocation custom)
--  cabalBinLoc <- resolveCustomCabalLoc (customCabalBinLocation custom)
--  sandboxLocation <- resolveCustomSandboxLoc (customSandboxLocation custom)
--  return CustomBuildStrategy
--    { customGhcBinLocation = ghcBinLoc
--    , customCabalBinLocation = cabalBinLoc
--    , customSandboxLocation = sandboxLocation
--    }

-- TODO
-- Build strategy based on whatever ghc, cabal, and sandbox are visible.
--defaultBuildStrategy :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => m (CustomBuildStrategy (Path Abs Dir))
--defaultBuildStrategy = do
--  customGhcBinLocation <- detectGhcLocation
--  customCabalBinLocation <- detectCabalLocation
--  customSandboxLocation <- detectSandboxLocation
--  return CustomBuildStrategy{..}


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

--detectSandboxLocation :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => m (Path Abs Dir)
--detectSandboxLocation =  do
--  mPackageDb <- liftIO $ parsePackageDb
--  case mPackageDb of
--    Just dbText -> do
--      dbDir <- parseAbsDir $ Text.unpack dbText
--      return $ parentAbs dbDir
--    Nothing -> throwM $ NotYetImplemented "detectSandboxLocation: no cabal.sandbox.config"

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


--resolveLTSSnapshot :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => StackageRoot -> LTSBuildStrategy -> m SnapshotBuildStrategy
--resolveLTSSnapshot stackageRoot LTSBuildStrategy{..} = do
--  lts <- case Text.stripPrefix "lts-" configLTS of
--    Nothing -> return $ "lts-" <> configLTS
--    Just{} -> return configLTS
--  mSnapshots <- liftIO $ Yaml.decodeFile (toFilePath stackageRoot)
--  configSnapshot <- case Map.lookup lts =<< (mSnapshots :: Maybe (Map Text Text)) of
--    Nothing -> return lts
--    Just lts' -> return lts'
--  return SnapshotBuildStrategy{..}

--resolveNightlySnapshot :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => NightlyBuildStrategy -> m SnapshotBuildStrategy
--resolveNightlySnapshot _ = throwM $ NotYetImplemented "resolveNightlySnapshot"


--resolveSnapshotSandboxLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => StackageRoot -> SnapshotBuildStrategy -> m (Path Abs Dir)
--resolveSnapshotSandboxLoc _ _ = throwM $ NotYetImplemented "resolveSnapshotSandboxLoc"

--resolveSnapshotGhcLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => StackageRoot -> SnapshotBuildStrategy -> m (Path Abs Dir)
--resolveSnapshotGhcLoc _ _ = throwM $ NotYetImplemented "resolveSnapshotGhcLoc"

--resolveSnapshotCabalLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => StackageRoot -> SnapshotBuildStrategy -> m (Path Abs Dir)
--resolveSnapshotCabalLoc _ _ = throwM $ NotYetImplemented "resolveSnapshotCabalLoc"

--resolveCustomSandboxLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => Text -> m (Path Abs Dir)
--resolveCustomSandboxLoc "detect" = detectSandboxLocation
--resolveCustomSandboxLoc _ = throwM $ NotYetImplemented "resolveCustomSandboxLoc"

--resolveCustomGhcLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => Text -> m (Path Abs Dir)
--resolveCustomGhcLoc "detect" = detectGhcLocation
--resolveCustomGhcLoc _ = throwM $ NotYetImplemented "resolveCustomGhcLoc"

--resolveCustomCabalLoc :: (MonadLogger m, MonadIO m, MonadThrow m)
--  => Text -> m (Path Abs Dir)
--resolveCustomCabalLoc "detect" = detectCabalLocation
--resolveCustomCabalLoc _ = throwM $ NotYetImplemented "resolveCustomCabalLoc"

-- TODO: copy from stackage-sandbox or wherver this has been well defined
resolveStackageHost :: (MonadLogger m, MonadIO m, MonadThrow m)
  => Maybe Text -> m String
resolveStackageHost = return . maybe stackageHostDefault Text.unpack

stackageHostDefault :: String
stackageHostDefault = "https://www.stackage.org"

-- | The filename used for the stackage config file.
stackageDotConfig :: Path Rel File
stackageDotConfig = $(mkRelFile "stackage.config")

-- | Get the binary locations as a string that could be used in the PATH
configBinPaths :: Config -> String
configBinPaths config =
   toFilePath (configGhcBinLocation config) <>
   [searchPathSeparator] <>
   toFilePath (configCabalBinLocation config)
