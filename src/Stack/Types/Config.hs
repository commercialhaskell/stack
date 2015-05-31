{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

-- | The Config type.

module Stack.Types.Config where

import           Control.Applicative ((<|>))
import           Control.Exception
import           Control.Monad (liftM)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.Reader (MonadReader, ask, asks, MonadIO, liftIO)
import           Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, withText, withObject, (.:?), (.!=))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Distribution.System (Platform)
import qualified Distribution.Text
import           Path
import           Stack.Types.BuildPlan (SnapName, renderSnapName, parseSnapName)
import           Stack.Types.Docker
import           Stack.Types.FlagName
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version
import           System.Process.Read (EnvOverride)

-- | The top-level Stackage configuration.
data Config =
  Config {configStackRoot        :: !(Path Abs Dir)
         -- ^ ~/.stack more often than not
         ,configDocker           :: !DockerOpts
         ,configUrls             :: !(Map Text Text)
         ,configGpgVerifyIndex   :: !Bool
         ,configEnvOverride      :: !(EnvSettings -> IO EnvOverride)
         -- ^ Environment variables to be passed to external tools
         ,configLocalGHCs        :: !(Path Abs Dir)
         -- ^ Path containing local GHC installations
         ,configConnectionCount  :: !Int
         -- ^ How many concurrent connections are allowed when downloading
         ,configHideTHLoading    :: !Bool
         -- ^ Hide the Template Haskell "Loading package ..." messages from the
         -- console
         ,configPlatform         :: !Platform
         }

-- | Controls which version of the environment is used
data EnvSettings = EnvSettings
    { esIncludeLocals :: !Bool
    -- ^ include local project bin directory, GHC_PACKAGE_PATH, etc
    , esIncludeGhcPackagePath :: !Bool
    -- ^ include the GHC_PACKAGE_PATH variable
    }
    deriving (Show, Eq, Ord)

-- | A superset of 'Config' adding information on how to build code. The reason
-- for this breakdown is because we will need some of the information from
-- 'Config' in order to determine the values here.
data BuildConfig = BuildConfig
    { bcConfig     :: !Config
    , bcResolver   :: !Resolver
      -- ^ How we resolve which dependencies to install given a set of
      -- packages.
    , bcGhcVersion :: !Version
      -- ^ Version of GHC we'll be using for this build, @Nothing@ if no
      -- preference
    , bcPackages   :: !(Set (Path Abs Dir))
      -- ^ Local packages identified by a path
    , bcExtraDeps  :: !(Map PackageName Version)
      -- ^ Extra dependencies specified in configuration.
      --
      -- These dependencies will not be installed to a shared location, and
      -- will override packages provided by the resolver.
    , bcRoot       :: !(Path Abs Dir)
      -- ^ Directory containing the project's stack.yaml file
    , bcFlags      :: !(Map PackageName (Map FlagName Bool))
      -- ^ Per-package flag overrides
    }

-- | Value returned by 'Stack.Config.loadConfig'.
data LoadConfig m = LoadConfig
    { lcConfig          :: !Config
      -- ^ Top-level Stack configuration.
    , lcLoadBuildConfig :: !(m BuildConfig)
        -- ^ Action to load the remaining 'BuildConfig'.  This action will create a project if one
        -- does not already exist.
    , lcProjectRoot     :: !(Maybe (Path Abs Dir))
        -- ^ The project root directory, if in a project.
    }

-- | How we resolve which dependencies to install given a set of packages.
data Resolver
  = ResolverSnapshot SnapName
  -- ^ Use an official snapshot from the Stackage project, either an LTS
  -- Haskell or Stackage Nightly

  | ResolverGhc {-# UNPACK #-} !MajorVersion
  -- ^ Require a specific GHC major version, but otherwise provide no build
  -- plan. Intended for use cases where end user wishes to specify all upstream
  -- dependencies manually, such as using a dependency solver.
  deriving (Show)

instance ToJSON Resolver where
    toJSON = toJSON . renderResolver
instance FromJSON Resolver where
    parseJSON = withText "Resolver" $
        either (fail . show) return . parseResolver

-- | Convert a Resolver into its @Text@ representation, as will be used by JSON/YAML
renderResolver :: Resolver -> Text
renderResolver (ResolverSnapshot name) = renderSnapName name
renderResolver (ResolverGhc (MajorVersion x y)) = T.pack $ concat ["ghc-", show x, ".", show y]

-- | Try to parse a @Resolver@, using same format as JSON/YAML/'renderResolver'
parseResolver :: MonadThrow m => Text -> m Resolver
parseResolver t =
    case parseSnapName t of
        Right x -> return $ ResolverSnapshot x
        Left _ ->
            case parseGhc of
                Just m -> return $ ResolverGhc m
                Nothing -> throwM $ ParseResolverException t
  where
    parseGhc = T.stripPrefix "ghc-" t >>= parseMajorVersionFromString . T.unpack

data ParseResolverException = ParseResolverException Text
    deriving (Show, Typeable)
instance Exception ParseResolverException

-- | Class for environment values which have access to the stack root
class HasStackRoot env where
    getStackRoot :: env -> Path Abs Dir
    default getStackRoot :: HasConfig env => env -> Path Abs Dir
    getStackRoot = configStackRoot . getConfig
    {-# INLINE getStackRoot #-}

-- | Class for environment values which have access to the URLs
class HasUrls env where
    getUrls :: env -> Map Text Text
    default getUrls :: HasConfig env => env -> Map Text Text
    getUrls = configUrls . getConfig
    {-# INLINE getUrls #-}

-- | Class for environment values which have a Platform
class HasPlatform env where
    getPlatform :: env -> Platform
    default getPlatform :: HasConfig env => env -> Platform
    getPlatform = configPlatform . getConfig
    {-# INLINE getPlatform #-}
instance HasPlatform Platform where
    getPlatform = id

-- | Class for environment values that can provide a 'Config'.
class (HasStackRoot env, HasUrls env, HasPlatform env) => HasConfig env where
    getConfig :: env -> Config
    default getConfig :: HasBuildConfig env => env -> Config
    getConfig = bcConfig . getBuildConfig
    {-# INLINE getConfig #-}
instance HasStackRoot Config
instance HasUrls Config
instance HasPlatform Config
instance HasConfig Config where
    getConfig = id
    {-# INLINE getConfig #-}

-- | Class for environment values that can provide a 'BuildConfig'.
class HasConfig env => HasBuildConfig env where
    getBuildConfig :: env -> BuildConfig
instance HasStackRoot BuildConfig
instance HasUrls BuildConfig
instance HasPlatform BuildConfig
instance HasConfig BuildConfig
instance HasBuildConfig BuildConfig where
    getBuildConfig = id
    {-# INLINE getBuildConfig #-}

-- An uninterpreted representation of configuration options.
-- Configurations may be "cascaded" using mappend (left-biased).
data ConfigMonoid =
  ConfigMonoid
    { configMonoidDockerOpts     :: !DockerOptsMonoid
    -- ^ Docker options.
    , configMonoidUrls           :: !(Map Text Text)
    -- ^ Various URLs for downloading things. Yes, this is stringly typed,
    -- making it easy to extend. Please make sure to only access it using
    -- helper functions.
    , configMonoidGpgVerifyIndex :: !(Maybe Bool)
    -- ^ Controls how package index updating occurs
    , configMonoidConnectionCount :: !(Maybe Int)
    -- ^ See: 'configConnectionCount'
    , configMonoidHideTHLoading :: !(Maybe Bool)
    -- ^ See: 'configHideTHLoading'
    }
  deriving Show

instance Monoid ConfigMonoid where
  mempty = ConfigMonoid
    { configMonoidDockerOpts = mempty
    , configMonoidUrls = mempty
    , configMonoidGpgVerifyIndex = Nothing
    , configMonoidConnectionCount = Nothing
    , configMonoidHideTHLoading = Nothing
    }
  mappend l r = ConfigMonoid
    { configMonoidDockerOpts = configMonoidDockerOpts l <> configMonoidDockerOpts r
    , configMonoidUrls = configMonoidUrls l <> configMonoidUrls r
    , configMonoidGpgVerifyIndex = configMonoidGpgVerifyIndex l <|> configMonoidGpgVerifyIndex r
    , configMonoidConnectionCount = configMonoidConnectionCount l <|> configMonoidConnectionCount r
    , configMonoidHideTHLoading = configMonoidHideTHLoading l <|> configMonoidHideTHLoading r
    }

instance FromJSON ConfigMonoid where
  parseJSON =
    withObject "ConfigMonoid" $
    \obj ->
      do configMonoidDockerOpts <- obj .:? T.pack "docker" .!= mempty
         configMonoidUrls <- obj .:? "urls" .!= mempty
         configMonoidGpgVerifyIndex <- obj .:? "gpg-verify-index"
         configMonoidConnectionCount <- obj .:? "connection-count"
         configMonoidHideTHLoading <- obj .:? "hide-th-loading"
         return ConfigMonoid {..}

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

-- | Helper function to ask the environment and apply getConfig
askConfig :: (MonadReader env m, HasConfig env) => m Config
askConfig = liftM getConfig ask

-- | Helper for looking up URLs
askUrl :: (MonadReader env m, HasUrls env)
       => Text -- ^ key
       -> Text -- ^ default
       -> m Text
askUrl key val = liftM (fromMaybe val . Map.lookup key . getUrls) ask

-- | Get the URL to request the information on the latest snapshots
askLatestSnapshotUrl :: (MonadReader env m, HasUrls env) => m Text
askLatestSnapshotUrl = askUrl "latest-snapshot-url" "https://www.stackage.org/download/snapshots.json"

-- | Git URL for the package index
askPackageIndexGitUrl :: (MonadReader env m, HasUrls env) => m Text
askPackageIndexGitUrl = askUrl "package-index-git-url" "https://github.com/commercialhaskell/all-cabal-hashes.git"

-- | HTTP URL for the package index
askPackageIndexHttpUrl :: (MonadReader env m, HasUrls env) => m Text
askPackageIndexHttpUrl = askUrl "package-index-http-url" "https://s3.amazonaws.com/hackage.fpcomplete.com/00-index.tar.gz"

-- | Location of the 00-index.cache file
configPackageIndexCache :: (MonadReader env m, HasConfig env) => m (Path Abs File)
configPackageIndexCache = do
    config <- asks getConfig
    return (configStackRoot config </> $(mkRelFile "00-index.cache"))

-- | Location of the 00-index.urls file
configPackageIndexUrls :: (MonadReader env m, HasConfig env) => m (Path Abs File)
configPackageIndexUrls = do
    config <- asks getConfig
    return (configStackRoot config </> $(mkRelFile "00-index.urls"))

-- | Location of the 00-index.tar file
configPackageIndex :: (MonadReader env m, HasConfig env) => m (Path Abs File)
configPackageIndex = do
    config <- asks getConfig
    return (configStackRoot config </> $(mkRelFile "00-index.tar"))

-- | Location of the 00-index.tar.gz file
configPackageIndexGz :: (MonadReader env m, HasConfig env) => m (Path Abs File)
configPackageIndexGz = do
    config <- asks getConfig
    return (configStackRoot config </> $(mkRelFile "00-index.tar.gz"))

-- | Location of a package tarball
configPackageTarball :: (MonadReader env m, HasConfig env, MonadThrow m) => PackageIdentifier -> m (Path Abs File)
configPackageTarball ident = do
    config <- asks getConfig
    name <- parseRelDir $ packageNameString $ packageIdentifierName ident
    ver <- parseRelDir $ versionString $ packageIdentifierVersion ident
    base <- parseRelFile $ packageIdentifierString ident ++ ".tar.gz"
    return $ configStackRoot config </> $(mkRelDir "packages") </> name </> ver </> base

-- | Per-project work dir
configProjectWorkDir :: (HasBuildConfig env, MonadReader env m) => m (Path Abs Dir)
configProjectWorkDir = do
    bc <- asks getBuildConfig
    return (bcRoot bc </> $(mkRelDir ".stack-work"))

-- | Relative directory for the platform identifier
platformRelDir :: (MonadReader env m, HasPlatform env, MonadThrow m) => m (Path Rel Dir)
platformRelDir = asks getPlatform >>= parseRelDir . Distribution.Text.display

-- | Path to .shake files.
configShakeFilesDir :: (MonadReader env m, HasBuildConfig env) => m (Path Abs Dir)
configShakeFilesDir = liftM (</> $(mkRelDir "shake")) configProjectWorkDir

-- | Where to unpack packages for local build
configLocalUnpackDir :: (MonadReader env m, HasBuildConfig env) => m (Path Abs Dir)
configLocalUnpackDir = liftM (</> $(mkRelDir "unpacked")) configProjectWorkDir

-- | Directory containing snapshots
snapshotsDir :: (MonadReader env m, HasConfig env, MonadThrow m) => m (Path Abs Dir)
snapshotsDir = do
    config <- asks getConfig
    platform <- platformRelDir
    return $ configStackRoot config </> $(mkRelDir "snapshots") </> platform

-- | Installation root for dependencies
installationRootDeps :: (MonadThrow m, MonadReader env m, HasBuildConfig env) => m (Path Abs Dir)
installationRootDeps = do
    snapshots <- snapshotsDir
    bc <- asks getBuildConfig
    name <- parseRelDir $ T.unpack $ renderResolver $ bcResolver bc
    ghc <- parseRelDir $ versionString $ bcGhcVersion bc
    return $ snapshots </> name </> ghc

-- | Installation root for locals
installationRootLocal :: (MonadThrow m, MonadReader env m, HasBuildConfig env) => m (Path Abs Dir)
installationRootLocal = do
    bc <- asks getBuildConfig
    name <- parseRelDir $ T.unpack $ renderResolver $ bcResolver bc
    ghc <- parseRelDir $ versionString $ bcGhcVersion bc
    platform <- platformRelDir
    return $ configProjectWorkDir bc </> $(mkRelDir "install") </> platform </> name </> ghc

-- | Package database for installing dependencies into
packageDatabaseDeps :: (MonadThrow m, MonadReader env m, HasBuildConfig env) => m (Path Abs Dir)
packageDatabaseDeps = do
    root <- installationRootDeps
    return $ root </> $(mkRelDir "pkgdb")

-- | Package database for installing local packages into
packageDatabaseLocal :: (MonadThrow m, MonadReader env m, HasBuildConfig env) => m (Path Abs Dir)
packageDatabaseLocal = do
    root <- installationRootLocal
    return $ root </> $(mkRelDir "pkgdb")

-- | Where to store mini build plan caches
configMiniBuildPlanCache :: (MonadThrow m, MonadReader env m, HasStackRoot env, HasPlatform env)
                         => SnapName
                         -> m (Path Abs File)
configMiniBuildPlanCache name = do
    root <- asks getStackRoot
    platform <- platformRelDir
    file <- parseRelFile $ T.unpack (renderSnapName name) ++ ".cache"
    -- Yes, cached plans differ based on platform
    return (root </> $(mkRelDir "build-plan-cache") </> platform </> file)

-- | Suffix applied to an installation root to get the bin dir
bindirSuffix :: Path Rel Dir
bindirSuffix = $(mkRelDir "bin")

-- | Get the extra bin directories (for the PATH). Puts more local first
--
-- Bool indicates whether or not to include the locals
extraBinDirs :: (MonadThrow m, MonadReader env m, HasBuildConfig env)
             => m (Bool -> [Path Abs Dir])
extraBinDirs = do
    deps <- installationRootDeps
    local <- installationRootLocal
    return $ \locals -> if locals
        then [local </> bindirSuffix, deps </> bindirSuffix]
        else [deps </> bindirSuffix]

-- | Get the minimal environment override, useful for just calling external
-- processes like git or ghc
getMinimalEnvOverride :: (MonadReader env m, HasConfig env, MonadIO m) => m EnvOverride
getMinimalEnvOverride = do
    config <- asks getConfig
    liftIO $ configEnvOverride config EnvSettings
                    { esIncludeLocals = False
                    , esIncludeGhcPackagePath = False
                    }

-- | File indicating that a specific package-version combo has been installed.
-- Should only be used for non-library dependencies (ghc-pkg tracks library).
configPackageInstalled :: (HasBuildConfig env, MonadReader env m, MonadThrow m)
                       => PackageIdentifier -> m (Path Abs File)
configPackageInstalled ident = do
    deps <- installationRootDeps
    ident' <- parseRelFile $ packageIdentifierString ident
    return $ deps </> $(mkRelDir "installed-packages") </> ident'
