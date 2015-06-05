{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, withText, withObject, object
                            ,(.=), (.:?), (.!=), (.:))
import           Data.Binary (Binary)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Hashable (Hashable)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
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
         -- ^ The platform we're building for, used in many directory names
         ,configLatestSnapshotUrl :: !Text
         -- ^ URL for a JSON file containing information on the latest
         -- snapshots available.
         ,configPackageIndices   :: ![PackageIndex]
         -- ^ Information on package indices. This is left biased, meaning that
         -- packages in an earlier index will shadow those in a later index.
         --
         -- Warning: if you override packages in an index vs what's available
         -- upstream, you may correct your compiled snapshots, as different
         -- projects may have different definitions of what pkg-ver means! This
         -- feature is primarily intended for adding local packages, not
         -- overriding. Overriding is better accomplished by adding to your
         -- list of packages.
         --
         -- Note that indices specified in a later config file will override
         -- previous indices, /not/ extend them.
         --
         -- Using an assoc list instead of a Map to keep track of priority
         }

-- | Information on a single package index
data PackageIndex = PackageIndex
    { indexName :: !IndexName
    , indexLocation :: !IndexLocation
    , indexDownloadPrefix :: !Text
    -- ^ URL prefix for downloading packages
    , indexGpgVerify :: !Bool
    -- ^ GPG-verify the package index during download. Only applies to Git
    -- repositories for now.
    , indexRequireHashes :: !Bool
    -- ^ Require that hashes and package size information be available for packages in this index
    }
    deriving Show
instance FromJSON PackageIndex where
    parseJSON = withObject "PackageIndex" $ \o -> do
        name <- o .: "name"
        prefix <- o .: "download-prefix"
        mgit <- o .:? "git"
        mhttp <- o .:? "http"
        loc <-
            case (mgit, mhttp) of
                (Nothing, Nothing) -> fail $
                    "Must provide either Git or HTTP URL for " ++
                    T.unpack (indexNameText name)
                (Just git, Nothing) -> return $ ILGit git
                (Nothing, Just http) -> return $ ILHttp http
                (Just git, Just http) -> return $ ILGitHttp git http
        gpgVerify <- o .:? "gpg-verify" .!= False
        reqHashes <- o .:? "require-hashes" .!= False
        return PackageIndex
            { indexName = name
            , indexLocation = loc
            , indexDownloadPrefix = prefix
            , indexGpgVerify = gpgVerify
            , indexRequireHashes = reqHashes
            }

-- | Unique name for a package index
newtype IndexName = IndexName { unIndexName :: ByteString }
    deriving (Show, Eq, Ord, Hashable, Binary)
indexNameText :: IndexName -> Text
indexNameText = decodeUtf8 . unIndexName
instance ToJSON IndexName where
    toJSON = toJSON . indexNameText
instance FromJSON IndexName where
    parseJSON = withText "IndexName" $ \t ->
        case parseRelDir (T.unpack t) of
            Left e -> fail $ "Invalid index name: " ++ show e
            Right _ -> return $ IndexName $ encodeUtf8 t

-- | Location of the package index. This ensures that at least one of Git or
-- HTTP is available.
data IndexLocation = ILGit !Text | ILHttp !Text | ILGitHttp !Text !Text
    deriving (Show, Eq, Ord)

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
    , bcStackYaml  :: !(Path Abs File)
      -- ^ Location of the stack.yaml file.
      --
      -- Note: if the STACK_YAML environment variable is used, this may be
      -- different from bcRoot </> "stack.yaml"
    , bcFlags      :: !(Map PackageName (Map FlagName Bool))
      -- ^ Per-package flag overrides
    }

-- | Value returned by 'Stack.Config.loadConfig'.
data LoadConfig m = LoadConfig
    { lcConfig          :: !Config
      -- ^ Top-level Stack configuration.
    , lcLoadBuildConfig :: !(NoBuildConfigStrategy -> m BuildConfig)
        -- ^ Action to load the remaining 'BuildConfig'.
    , lcProjectRoot     :: !(Maybe (Path Abs Dir))
        -- ^ The project root directory, if in a project.
    }

data NoBuildConfigStrategy
    = ThrowException
    | CreateConfig
    | ExecStrategy
    deriving (Show, Eq, Ord)

-- | A project is a collection of packages. We can have multiple stack.yaml
-- files, but only one of them may contain project information.
data Project = Project
    { projectPackages :: ![FilePath]
    -- ^ Components of the package list which refer to local directories
    --
    -- Note that we use @FilePath@ and not @Path@s. The goal is: first parse
    -- the value raw, and then use @canonicalizePath@ and @parseAbsDir@.
    , projectExtraDeps :: !(Map PackageName Version)
    -- ^ Components of the package list referring to package/version combos,
    -- see: https://github.com/fpco/stack/issues/41
    , projectFlags :: !(Map PackageName (Map FlagName Bool))
    -- ^ Per-package flag overrides
    , projectResolver :: !Resolver
    -- ^ How we resolve which dependencies to use
    }
  deriving Show

instance ToJSON Project where
    toJSON p = object
        [ "packages"   .= projectPackages p
        , "extra-deps" .= map fromTuple (Map.toList $ projectExtraDeps p)
        , "flags"      .= projectFlags p
        , "resolver"   .= projectResolver p
        ]

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

-- | Class for environment values which have access to the stack root
class HasStackRoot env where
    getStackRoot :: env -> Path Abs Dir
    default getStackRoot :: HasConfig env => env -> Path Abs Dir
    getStackRoot = configStackRoot . getConfig
    {-# INLINE getStackRoot #-}

-- | Class for environment values which have a Platform
class HasPlatform env where
    getPlatform :: env -> Platform
    default getPlatform :: HasConfig env => env -> Platform
    getPlatform = configPlatform . getConfig
    {-# INLINE getPlatform #-}
instance HasPlatform Platform where
    getPlatform = id

-- | Class for environment values that can provide a 'Config'.
class (HasStackRoot env, HasPlatform env) => HasConfig env where
    getConfig :: env -> Config
    default getConfig :: HasBuildConfig env => env -> Config
    getConfig = bcConfig . getBuildConfig
    {-# INLINE getConfig #-}
instance HasStackRoot Config
instance HasPlatform Config
instance HasConfig Config where
    getConfig = id
    {-# INLINE getConfig #-}

-- | Class for environment values that can provide a 'BuildConfig'.
class HasConfig env => HasBuildConfig env where
    getBuildConfig :: env -> BuildConfig
instance HasStackRoot BuildConfig
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
    , configMonoidConnectionCount :: !(Maybe Int)
    -- ^ See: 'configConnectionCount'
    , configMonoidHideTHLoading :: !(Maybe Bool)
    -- ^ See: 'configHideTHLoading'
    , configMonoidLatestSnapshotUrl :: !(Maybe Text)
    -- ^ See: 'configLatestSnapshotUrl'
    , configMonoidPackageIndices :: !(Maybe [PackageIndex])
    -- ^ See: 'configPackageIndices'
    }
  deriving Show

instance Monoid ConfigMonoid where
  mempty = ConfigMonoid
    { configMonoidDockerOpts = mempty
    , configMonoidConnectionCount = Nothing
    , configMonoidHideTHLoading = Nothing
    , configMonoidLatestSnapshotUrl = Nothing
    , configMonoidPackageIndices = Nothing
    }
  mappend l r = ConfigMonoid
    { configMonoidDockerOpts = configMonoidDockerOpts l <> configMonoidDockerOpts r
    , configMonoidConnectionCount = configMonoidConnectionCount l <|> configMonoidConnectionCount r
    , configMonoidHideTHLoading = configMonoidHideTHLoading l <|> configMonoidHideTHLoading r
    , configMonoidLatestSnapshotUrl = configMonoidLatestSnapshotUrl l <|> configMonoidLatestSnapshotUrl r
    , configMonoidPackageIndices = configMonoidPackageIndices l <|> configMonoidPackageIndices r
    }

instance FromJSON ConfigMonoid where
  parseJSON =
    withObject "ConfigMonoid" $
    \obj ->
      do configMonoidDockerOpts <- obj .:? T.pack "docker" .!= mempty
         configMonoidConnectionCount <- obj .:? "connection-count"
         configMonoidHideTHLoading <- obj .:? "hide-th-loading"
         configMonoidLatestSnapshotUrl <- obj .:? "latest-snapshot-url"
         configMonoidPackageIndices <- obj .:? "package-indices"
         return ConfigMonoid {..}

data ConfigException
  = ParseResolverException Text
  | NoProjectConfigFound (Path Abs Dir)
  deriving Typeable
instance Show ConfigException where
    show (ParseResolverException t) = concat
        [ "Invalid resolver value: "
        , T.unpack t
        , ". Possible valid values include lts-2.12, nightly-2015-01-01, and ghc-7.10."
        ]
    show (NoProjectConfigFound dir) = concat
        [ "Unable to find a stack.yaml file in the current directory ("
        , toFilePath dir
        , ") or its ancestors"
        ]
instance Exception ConfigException

-- | Helper function to ask the environment and apply getConfig
askConfig :: (MonadReader env m, HasConfig env) => m Config
askConfig = liftM getConfig ask

-- | Get the URL to request the information on the latest snapshots
askLatestSnapshotUrl :: (MonadReader env m, HasConfig env) => m Text
askLatestSnapshotUrl = asks (configLatestSnapshotUrl . getConfig)

-- | Root for a specific package index
configPackageIndexRoot :: (MonadReader env m, HasConfig env, MonadThrow m) => IndexName -> m (Path Abs Dir)
configPackageIndexRoot (IndexName name) = do
    config <- asks getConfig
    dir <- parseRelDir $ S8.unpack name
    return (configStackRoot config </> $(mkRelDir "indices") </> dir)

-- | Location of the 00-index.cache file
configPackageIndexCache :: (MonadReader env m, HasConfig env, MonadThrow m) => IndexName -> m (Path Abs File)
configPackageIndexCache = liftM (</> $(mkRelFile "00-index.cache")) . configPackageIndexRoot

-- | Location of the 00-index.tar file
configPackageIndex :: (MonadReader env m, HasConfig env, MonadThrow m) => IndexName -> m (Path Abs File)
configPackageIndex = liftM (</> $(mkRelFile "00-index.tar")) . configPackageIndexRoot

-- | Location of the 00-index.tar.gz file
configPackageIndexGz :: (MonadReader env m, HasConfig env, MonadThrow m) => IndexName -> m (Path Abs File)
configPackageIndexGz = liftM (</> $(mkRelFile "00-index.tar.gz")) . configPackageIndexRoot

-- | Location of a package tarball
configPackageTarball :: (MonadReader env m, HasConfig env, MonadThrow m) => IndexName -> PackageIdentifier -> m (Path Abs File)
configPackageTarball iname ident = do
    root <- configPackageIndexRoot iname
    name <- parseRelDir $ packageNameString $ packageIdentifierName ident
    ver <- parseRelDir $ versionString $ packageIdentifierVersion ident
    base <- parseRelFile $ packageIdentifierString ident ++ ".tar.gz"
    return (root </> $(mkRelDir "packages") </> name </> ver </> base)

-- | Per-project work dir
configProjectWorkDir :: (HasBuildConfig env, MonadReader env m) => m (Path Abs Dir)
configProjectWorkDir = do
    bc <- asks getBuildConfig
    return (bcRoot bc </> $(mkRelDir ".stack-work"))

-- | File containing the profiling cache, see "Stack.PackageDump"
configProfilingCache :: (HasBuildConfig env, MonadReader env m) => m (Path Abs File)
configProfilingCache = liftM (</> $(mkRelFile "profiling-cache.bin")) configProjectWorkDir

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
