{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The Config type.

module Stack.Types.Config
  (
  -- * Main configuration types and classes
  -- ** HasPlatform & HasStackRoot
   HasPlatform(..)
  ,HasStackRoot(..)
  ,PlatformVariant(..)
  -- ** Config & HasConfig
  ,Config(..)
  ,HasConfig(..)
  ,askConfig
  ,askLatestSnapshotUrl
  ,explicitSetupDeps
  ,getMinimalEnvOverride
  -- ** BuildConfig & HasBuildConfig
  ,BuildConfig(..)
  ,bcRoot
  ,bcWorkDir
  ,HasBuildConfig(..)
  -- ** GHCVariant & HasGHCVariant
  ,GHCVariant(..)
  ,ghcVariantName
  ,ghcVariantSuffix
  ,parseGHCVariant
  ,HasGHCVariant(..)
  ,snapshotsDir
  -- ** EnvConfig & HasEnvConfig
  ,EnvConfig(..)
  ,HasEnvConfig(..)
  ,getWhichCompiler
  ,getCompilerPath
  -- * Details
  -- ** ApplyGhcOptions
  ,ApplyGhcOptions(..)
  -- ** ConfigException
  ,ConfigException(..)
  -- ** ConfigMonoid
  ,ConfigMonoid(..)
  -- ** EnvSettings
  ,EnvSettings(..)
  ,minimalEnvSettings
  -- ** GlobalOpts & GlobalOptsMonoid
  ,GlobalOpts(..)
  ,GlobalOptsMonoid(..)
  ,defaultLogLevel
  -- ** LoadConfig
  ,LoadConfig(..)
  -- ** PackageEntry & PackageLocation
  ,PackageEntry(..)
  ,TreatLikeExtraDep
  ,PackageLocation(..)
  ,RemotePackageType(..)
  -- ** PackageIndex, IndexName & IndexLocation
  ,PackageIndex(..)
  ,IndexName(..)
  ,configPackageIndex
  ,configPackageIndexCache
  ,configPackageIndexGz
  ,configPackageIndexRoot
  ,configPackageTarball
  ,indexNameText
  ,IndexLocation(..)
  -- ** Project & ProjectAndConfigMonoid
  ,Project(..)
  ,ProjectAndConfigMonoid(..)
  -- ** PvpBounds
  ,PvpBounds(..)
  ,parsePvpBounds
  -- ** Resolver & AbstractResolver
  ,Resolver(..)
  ,parseResolverText
  ,resolverName
  ,AbstractResolver(..)
  -- ** SCM
  ,SCM(..)
  -- * Paths
  ,bindirSuffix
  ,configInstalledCache
  ,configMiniBuildPlanCache
  ,getProjectWorkDir
  ,docDirSuffix
  ,flagCacheLocal
  ,extraBinDirs
  ,hpcReportDir
  ,installationRootDeps
  ,installationRootLocal
  ,packageDatabaseDeps
  ,packageDatabaseExtra
  ,packageDatabaseLocal
  ,platformOnlyRelDir
  ,platformGhcRelDir
  ,useShaPathOnWindows
  ,getWorkDir
  -- * Command-specific types
  -- ** Eval
  ,EvalOpts(..)
  -- ** Exec
  ,ExecOpts(..)
  ,SpecialExecCmd(..)
  ,ExecOptsExtra(..)
  -- ** Setup
  ,DownloadInfo(..)
  ,VersionedDownloadInfo(..)
  ,SetupInfo(..)
  ,SetupInfoLocation(..)
  -- ** Docker entrypoint
  ,DockerEntrypoint(..)
  ,DockerUser(..)
  ,module X
  ) where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Exception
import           Control.Monad (liftM, mzero, forM, join)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.Logger (LogLevel(..))
import           Control.Monad.Reader (MonadReader, ask, asks, MonadIO, liftIO)
import           Data.Aeson.Extended
                 (ToJSON, toJSON, FromJSON, parseJSON, withText, object,
                  (.=), (..:), (..:?), (..!=), Value(String, Object),
                  withObjectWarnings, WarningParser, Object, jsonSubWarnings,
                  jsonSubWarningsT, jsonSubWarningsTT, WithJSONWarnings(..), noJSONWarnings)
import           Data.Attoparsec.Args
import           Data.Binary (Binary)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Either (partitionEithers)
import           Data.IORef (IORef)
import           Data.List (stripPrefix)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Hashable (Hashable)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Typeable
import           Data.Yaml (ParseException)
import           Distribution.System (Platform)
import qualified Distribution.Text
import           Distribution.Version (anyVersion)
import           Network.HTTP.Client (parseUrl)
import           Path
import qualified Paths_stack as Meta
import           Stack.Types.BuildPlan (SnapName, renderSnapName, parseSnapName)
import           Stack.Types.Urls
import           Stack.Types.Compiler
import           Stack.Types.Docker
import           Stack.Types.Nix
import           Stack.Types.FlagName
import           Stack.Types.Image
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageIndex
import           Stack.Types.PackageName
import           Stack.Types.TemplateName
import           Stack.Types.Version
import           System.PosixCompat.Types (UserID, GroupID, FileMode)
import           System.Process.Read (EnvOverride, findExecutable)

-- Re-exports
import          Stack.Types.Config.Build as X

#ifdef mingw32_HOST_OS
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base16 as B16
#endif

-- | The top-level Stackage configuration.
data Config =
  Config {configStackRoot           :: !(Path Abs Dir)
         -- ^ ~/.stack more often than not
         ,configWorkDir             :: !(Path Rel Dir)
         -- ^ this allows to override .stack-work directory
         ,configUserConfigPath      :: !(Path Abs File)
         -- ^ Path to user configuration file (usually ~/.stack/config.yaml)
         ,configBuild               :: !BuildOpts
         -- ^ Build configuration
         ,configDocker              :: !DockerOpts
         -- ^ Docker configuration
         ,configNix                 :: !NixOpts
         -- ^ Execution environment (e.g nix-shell) configuration
         ,configEnvOverride         :: !(EnvSettings -> IO EnvOverride)
         -- ^ Environment variables to be passed to external tools
         ,configLocalProgramsBase   :: !(Path Abs Dir)
         -- ^ Non-platform-specific path containing local installations
         ,configLocalPrograms       :: !(Path Abs Dir)
         -- ^ Path containing local installations (mainly GHC)
         ,configConnectionCount     :: !Int
         -- ^ How many concurrent connections are allowed when downloading
         ,configHideTHLoading       :: !Bool
         -- ^ Hide the Template Haskell "Loading package ..." messages from the
         -- console
         ,configPlatform            :: !Platform
         -- ^ The platform we're building for, used in many directory names
         ,configPlatformVariant     :: !PlatformVariant
         -- ^ Variant of the platform, also used in directory names
         ,configGHCVariant0         :: !(Maybe GHCVariant)
         -- ^ The variant of GHC requested by the user.
         -- In most cases, use 'BuildConfig' or 'MiniConfig's version instead,
         -- which will have an auto-detected default.
         ,configUrls                :: !Urls
         -- ^ URLs for other files used by stack.
         -- TODO: Better document
         -- e.g. The latest snapshot file.
         -- A build plan name (e.g. lts5.9.yaml) is appended when downloading
         -- the build plan actually.
         ,configPackageIndices      :: ![PackageIndex]
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
         ,configSystemGHC           :: !Bool
         -- ^ Should we use the system-installed GHC (on the PATH) if
         -- available? Can be overridden by command line options.
         ,configInstallGHC          :: !Bool
         -- ^ Should we automatically install GHC if missing or the wrong
         -- version is available? Can be overridden by command line options.
         ,configSkipGHCCheck        :: !Bool
         -- ^ Don't bother checking the GHC version or architecture.
         ,configSkipMsys            :: !Bool
         -- ^ On Windows: don't use a locally installed MSYS
         ,configCompilerCheck       :: !VersionCheck
         -- ^ Specifies which versions of the compiler are acceptable.
         ,configLocalBin            :: !(Path Abs Dir)
         -- ^ Directory we should install executables into
         ,configRequireStackVersion :: !VersionRange
         -- ^ Require a version of stack within this range.
         ,configJobs                :: !Int
         -- ^ How many concurrent jobs to run, defaults to number of capabilities
         ,configExtraIncludeDirs    :: !(Set Text)
         -- ^ --extra-include-dirs arguments
         ,configExtraLibDirs        :: !(Set Text)
         -- ^ --extra-lib-dirs arguments
         ,configConfigMonoid        :: !ConfigMonoid
         -- ^ @ConfigMonoid@ used to generate this
         ,configConcurrentTests     :: !Bool
         -- ^ Run test suites concurrently
         ,configImage               :: !ImageOpts
         ,configTemplateParams      :: !(Map Text Text)
         -- ^ Parameters for templates.
         ,configScmInit             :: !(Maybe SCM)
         -- ^ Initialize SCM (e.g. git) when creating new projects.
         ,configGhcOptions          :: !(Map (Maybe PackageName) [Text])
         -- ^ Additional GHC options to apply to either all packages (Nothing)
         -- or a specific package (Just).
         ,configSetupInfoLocations  :: ![SetupInfoLocation]
         -- ^ Additional SetupInfo (inline or remote) to use to find tools.
         ,configPvpBounds           :: !PvpBounds
         -- ^ How PVP upper bounds should be added to packages
         ,configModifyCodePage      :: !Bool
         -- ^ Force the code page to UTF-8 on Windows
         ,configExplicitSetupDeps   :: !(Map (Maybe PackageName) Bool)
         -- ^ See 'explicitSetupDeps'. 'Nothing' provides the default value.
         ,configRebuildGhcOptions   :: !Bool
         -- ^ Rebuild on GHC options changes
         ,configApplyGhcOptions     :: !ApplyGhcOptions
         -- ^ Which packages to ghc-options on the command line apply to?
         ,configAllowNewer          :: !Bool
         -- ^ Ignore version ranges in .cabal files. Funny naming chosen to
         -- match cabal.
         ,configDefaultTemplate     :: !(Maybe TemplateName)
         -- ^ The default template to use when none is specified.
         -- (If Nothing, the default default is used.)
         ,configAllowDifferentUser  :: !Bool
         -- ^ Allow users other than the stack root owner to use the stack
         -- installation.
         ,configPackageCaches       :: !(IORef (Maybe (Map PackageIdentifier (PackageIndex, PackageCache))))
         -- ^ In memory cache of hackage index.
         ,configMaybeProject        :: !(Maybe (Project, Path Abs File))
         }

-- | Which packages to ghc-options on the command line apply to?
data ApplyGhcOptions = AGOTargets -- ^ all local targets
                     | AGOLocals -- ^ all local packages, even non-targets
                     | AGOEverything -- ^ every package
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance FromJSON ApplyGhcOptions where
    parseJSON = withText "ApplyGhcOptions" $ \t ->
        case t of
            "targets" -> return AGOTargets
            "locals" -> return AGOLocals
            "everything" -> return AGOEverything
            _ -> fail $ "Invalid ApplyGhcOptions: " ++ show t

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
instance FromJSON (WithJSONWarnings PackageIndex) where
    parseJSON = withObjectWarnings "PackageIndex" $ \o -> do
        name <- o ..: "name"
        prefix <- o ..: "download-prefix"
        mgit <- o ..:? "git"
        mhttp <- o ..:? "http"
        loc <-
            case (mgit, mhttp) of
                (Nothing, Nothing) -> fail $
                    "Must provide either Git or HTTP URL for " ++
                    T.unpack (indexNameText name)
                (Just git, Nothing) -> return $ ILGit git
                (Nothing, Just http) -> return $ ILHttp http
                (Just git, Just http) -> return $ ILGitHttp git http
        gpgVerify <- o ..:? "gpg-verify" ..!= False
        reqHashes <- o ..:? "require-hashes" ..!= False
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
    , esStackExe :: !Bool
    -- ^ set the STACK_EXE variable to the current executable name
    , esLocaleUtf8 :: !Bool
    -- ^ set the locale to C.UTF-8
    }
    deriving (Show, Eq, Ord)

data ExecOpts = ExecOpts
    { eoCmd :: !SpecialExecCmd
    , eoArgs :: ![String]
    , eoExtra :: !ExecOptsExtra
    } deriving (Show)

data SpecialExecCmd
    = ExecCmd String
    | ExecGhc
    | ExecRunGhc
    deriving (Show, Eq)

data ExecOptsExtra
    = ExecOptsPlain
    | ExecOptsEmbellished
        { eoEnvSettings :: !EnvSettings
        , eoPackages :: ![String]
        }
    deriving (Show)

data EvalOpts = EvalOpts
    { evalArg :: !String
    , evalExtra :: !ExecOptsExtra
    } deriving (Show)

-- | Parsed global command-line options.
data GlobalOpts = GlobalOpts
    { globalReExecVersion :: !(Maybe String) -- ^ Expected re-exec in container version
    , globalDockerEntrypoint :: !(Maybe DockerEntrypoint)
      -- ^ Data used when stack is acting as a Docker entrypoint (internal use only)
    , globalLogLevel     :: !LogLevel -- ^ Log level
    , globalConfigMonoid :: !ConfigMonoid -- ^ Config monoid, for passing into 'loadConfig'
    , globalResolver     :: !(Maybe AbstractResolver) -- ^ Resolver override
    , globalCompiler     :: !(Maybe CompilerVersion) -- ^ Compiler override
    , globalTerminal     :: !Bool -- ^ We're in a terminal?
    , globalStackYaml    :: !(Maybe FilePath) -- ^ Override project stack.yaml
    } deriving (Show)

-- | Parsed global command-line options monoid.
data GlobalOptsMonoid = GlobalOptsMonoid
    { globalMonoidReExecVersion :: !(Maybe String) -- ^ Expected re-exec in container version
    , globalMonoidDockerEntrypoint :: !(Maybe DockerEntrypoint)
      -- ^ Data used when stack is acting as a Docker entrypoint (internal use only)
    , globalMonoidLogLevel     :: !(Maybe LogLevel) -- ^ Log level
    , globalMonoidConfigMonoid :: !ConfigMonoid -- ^ Config monoid, for passing into 'loadConfig'
    , globalMonoidResolver     :: !(Maybe AbstractResolver) -- ^ Resolver override
    , globalMonoidCompiler     :: !(Maybe CompilerVersion) -- ^ Compiler override
    , globalMonoidTerminal     :: !(Maybe Bool) -- ^ We're in a terminal?
    , globalMonoidStackYaml    :: !(Maybe FilePath) -- ^ Override project stack.yaml
    } deriving (Show)

instance Monoid GlobalOptsMonoid where
    mempty = GlobalOptsMonoid Nothing Nothing Nothing mempty Nothing Nothing Nothing Nothing
    mappend l r = GlobalOptsMonoid
        { globalMonoidReExecVersion = globalMonoidReExecVersion l <|> globalMonoidReExecVersion r
        , globalMonoidDockerEntrypoint =
            globalMonoidDockerEntrypoint l <|> globalMonoidDockerEntrypoint r
        , globalMonoidLogLevel = globalMonoidLogLevel l <|> globalMonoidLogLevel r
        , globalMonoidConfigMonoid = globalMonoidConfigMonoid l <> globalMonoidConfigMonoid r
        , globalMonoidResolver = globalMonoidResolver l <|> globalMonoidResolver r
        , globalMonoidCompiler = globalMonoidCompiler l <|> globalMonoidCompiler r
        , globalMonoidTerminal = globalMonoidTerminal l <|> globalMonoidTerminal r
        , globalMonoidStackYaml = globalMonoidStackYaml l <|> globalMonoidStackYaml r }

-- | Either an actual resolver value, or an abstract description of one (e.g.,
-- latest nightly).
data AbstractResolver
    = ARLatestNightly
    | ARLatestLTS
    | ARLatestLTSMajor !Int
    | ARResolver !Resolver
    | ARGlobal
    deriving Show

-- | Default logging level should be something useful but not crazy.
defaultLogLevel :: LogLevel
defaultLogLevel = LevelInfo

-- | A superset of 'Config' adding information on how to build code. The reason
-- for this breakdown is because we will need some of the information from
-- 'Config' in order to determine the values here.
data BuildConfig = BuildConfig
    { bcConfig     :: !Config
    , bcResolver   :: !Resolver
      -- ^ How we resolve which dependencies to install given a set of
      -- packages.
    , bcWantedCompiler :: !CompilerVersion
      -- ^ Compiler version wanted for this build
    , bcPackageEntries :: ![PackageEntry]
      -- ^ Local packages
    , bcExtraDeps  :: !(Map PackageName Version)
      -- ^ Extra dependencies specified in configuration.
      --
      -- These dependencies will not be installed to a shared location, and
      -- will override packages provided by the resolver.
    , bcExtraPackageDBs :: ![Path Abs Dir]
      -- ^ Extra package databases
    , bcStackYaml  :: !(Path Abs File)
      -- ^ Location of the stack.yaml file.
      --
      -- Note: if the STACK_YAML environment variable is used, this may be
      -- different from bcRoot </> "stack.yaml"
    , bcFlags      :: !(Map PackageName (Map FlagName Bool))
      -- ^ Per-package flag overrides
    , bcImplicitGlobal :: !Bool
      -- ^ Are we loading from the implicit global stack.yaml? This is useful
      -- for providing better error messages.
    , bcGHCVariant :: !GHCVariant
      -- ^ The variant of GHC used to select a GHC bindist.
    }

-- | Directory containing the project's stack.yaml file
bcRoot :: BuildConfig -> Path Abs Dir
bcRoot = parent . bcStackYaml

-- | @"'bcRoot'/.stack-work"@
bcWorkDir :: (MonadReader env m, HasConfig env) => BuildConfig -> m (Path Abs Dir)
bcWorkDir bconfig = do
  workDir <- getWorkDir
  return (bcRoot bconfig </> workDir)

-- | Configuration after the environment has been setup.
data EnvConfig = EnvConfig
    {envConfigBuildConfig :: !BuildConfig
    ,envConfigCabalVersion :: !Version
    ,envConfigCompilerVersion :: !CompilerVersion
    ,envConfigPackages   :: !(Map (Path Abs Dir) TreatLikeExtraDep)}
instance HasBuildConfig EnvConfig where
    getBuildConfig = envConfigBuildConfig
instance HasConfig EnvConfig
instance HasPlatform EnvConfig
instance HasGHCVariant EnvConfig
instance HasStackRoot EnvConfig
class (HasBuildConfig r, HasGHCVariant r) => HasEnvConfig r where
    getEnvConfig :: r -> EnvConfig
instance HasEnvConfig EnvConfig where
    getEnvConfig = id

-- | Value returned by 'Stack.Config.loadConfig'.
data LoadConfig m = LoadConfig
    { lcConfig          :: !Config
      -- ^ Top-level Stack configuration.
    , lcLoadBuildConfig :: !(Maybe CompilerVersion -> m BuildConfig)
        -- ^ Action to load the remaining 'BuildConfig'.
    , lcProjectRoot     :: !(Maybe (Path Abs Dir))
        -- ^ The project root directory, if in a project.
    }

data PackageEntry = PackageEntry
    { peExtraDep :: !TreatLikeExtraDep
    , peLocation :: !PackageLocation
    , peSubdirs :: ![FilePath]
    }
    deriving Show

-- | Should a package be treated just like an extra-dep?
--
-- 'True' means, it will only be built as a dependency
-- for others, and its test suite/benchmarks will not be run.
--
-- Useful modifying an upstream package, see:
-- https://github.com/commercialhaskell/stack/issues/219
-- https://github.com/commercialhaskell/stack/issues/386
type TreatLikeExtraDep = Bool

instance ToJSON PackageEntry where
    toJSON pe | not (peExtraDep pe) && null (peSubdirs pe) =
        toJSON $ peLocation pe
    toJSON pe = object
        [ "extra-dep" .= peExtraDep pe
        , "location" .= peLocation pe
        , "subdirs" .= peSubdirs pe
        ]
instance FromJSON (WithJSONWarnings PackageEntry) where
    parseJSON (String t) = do
        WithJSONWarnings loc _ <- parseJSON $ String t
        return $ noJSONWarnings
               (PackageEntry
                { peExtraDep = False
                , peLocation = loc
                , peSubdirs = []
                })
    parseJSON v = withObjectWarnings "PackageEntry" (\o -> PackageEntry
        <$> o ..:? "extra-dep" ..!= False
        <*> jsonSubWarnings (o ..: "location")
        <*> o ..:? "subdirs" ..!= []) v

data PackageLocation
    = PLFilePath FilePath
    -- ^ Note that we use @FilePath@ and not @Path@s. The goal is: first parse
    -- the value raw, and then use @canonicalizePath@ and @parseAbsDir@.
    | PLRemote Text RemotePackageType
     -- ^ URL and further details
    deriving Show

data RemotePackageType
    = RPTHttp
    | RPTGit Text -- ^ Commit
    | RPTHg  Text -- ^ Commit
    deriving Show

instance ToJSON PackageLocation where
    toJSON (PLFilePath fp) = toJSON fp
    toJSON (PLRemote t RPTHttp) = toJSON t
    toJSON (PLRemote x (RPTGit y)) = toJSON $ T.unwords ["git", x, y]
    toJSON (PLRemote x (RPTHg  y)) = toJSON $ T.unwords ["hg",  x, y]

instance FromJSON (WithJSONWarnings PackageLocation) where
    parseJSON v
        = (noJSONWarnings <$> withText "PackageLocation" (\t -> http t <|> file t) v)
        <|> git v
        <|> hg  v
      where
        file t = pure $ PLFilePath $ T.unpack t
        http t =
            case parseUrl $ T.unpack t of
                Left  _ -> mzero
                Right _ -> return $ PLRemote t RPTHttp

        git = withObjectWarnings "PackageGitLocation" $ \o -> PLRemote
            <$> o ..: "git"
            <*> (RPTGit <$> o ..: "commit")
        hg  = withObjectWarnings "PackageHgLocation"  $ \o -> PLRemote
            <$> o ..: "hg"
            <*> (RPTHg  <$> o ..: "commit")

-- | A project is a collection of packages. We can have multiple stack.yaml
-- files, but only one of them may contain project information.
data Project = Project
    { projectUserMsg :: !(Maybe String)
    -- ^ A warning message to display to the user when the auto generated
    -- config may have issues.
    , projectPackages :: ![PackageEntry]
    -- ^ Components of the package list
    , projectExtraDeps :: !(Map PackageName Version)
    -- ^ Components of the package list referring to package/version combos,
    -- see: https://github.com/fpco/stack/issues/41
    , projectFlags :: !(Map PackageName (Map FlagName Bool))
    -- ^ Per-package flag overrides
    , projectResolver :: !Resolver
    -- ^ How we resolve which dependencies to use
    , projectCompiler :: !(Maybe CompilerVersion)
    -- ^ When specified, overrides which compiler to use
    , projectExtraPackageDBs :: ![FilePath]
    }
  deriving Show

instance ToJSON Project where
    toJSON p = object $
        (maybe id (\cv -> (("compiler" .= cv) :)) (projectCompiler p))
        ((maybe id (\msg -> (("user-message" .= msg) :)) (projectUserMsg p))
        [ "packages"          .= projectPackages p
        , "extra-deps"        .= map fromTuple (Map.toList $ projectExtraDeps p)
        , "flags"             .= projectFlags p
        , "resolver"          .= projectResolver p
        , "extra-package-dbs" .= projectExtraPackageDBs p
        ])

-- | How we resolve which dependencies to install given a set of packages.
data Resolver
  = ResolverSnapshot SnapName
  -- ^ Use an official snapshot from the Stackage project, either an LTS
  -- Haskell or Stackage Nightly

  | ResolverCompiler !CompilerVersion
  -- ^ Require a specific compiler version, but otherwise provide no build plan.
  -- Intended for use cases where end user wishes to specify all upstream
  -- dependencies manually, such as using a dependency solver.

  | ResolverCustom !Text !Text
  -- ^ A custom resolver based on the given name and URL. This file is assumed
  -- to be completely immutable.
  deriving (Show)

instance ToJSON Resolver where
    toJSON (ResolverCustom name location) = object
        [ "name" .= name
        , "location" .= location
        ]
    toJSON x = toJSON $ resolverName x
instance FromJSON (WithJSONWarnings Resolver) where
    -- Strange structuring is to give consistent error messages
    parseJSON v@(Object _) = withObjectWarnings "Resolver" (\o -> ResolverCustom
        <$> o ..: "name"
        <*> o ..: "location") v

    parseJSON (String t) = either (fail . show) return (noJSONWarnings <$> parseResolverText t)

    parseJSON _ = fail $ "Invalid Resolver, must be Object or String"

-- | Convert a Resolver into its @Text@ representation, as will be used by
-- directory names
resolverName :: Resolver -> Text
resolverName (ResolverSnapshot name) = renderSnapName name
resolverName (ResolverCompiler v) = compilerVersionText v
resolverName (ResolverCustom name _) = "custom-" <> name

-- | Try to parse a @Resolver@ from a @Text@. Won't work for complex resolvers (like custom).
parseResolverText :: MonadThrow m => Text -> m Resolver
parseResolverText t
    | Right x <- parseSnapName t = return $ ResolverSnapshot x
    | Just v <- parseCompilerVersion t = return $ ResolverCompiler v
    | otherwise = throwM $ ParseResolverException t

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
    getPlatformVariant :: env -> PlatformVariant
    default getPlatformVariant :: HasConfig env => env -> PlatformVariant
    getPlatformVariant = configPlatformVariant . getConfig
    {-# INLINE getPlatformVariant #-}
instance HasPlatform (Platform,PlatformVariant) where
    getPlatform (p,_) = p
    getPlatformVariant (_,v) = v

-- | Class for environment values which have a GHCVariant
class HasGHCVariant env where
    getGHCVariant :: env -> GHCVariant
    default getGHCVariant :: HasBuildConfig env => env -> GHCVariant
    getGHCVariant = bcGHCVariant . getBuildConfig
    {-# INLINE getGHCVariant #-}
instance HasGHCVariant GHCVariant where
    getGHCVariant = id

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
instance HasGHCVariant BuildConfig
instance HasConfig BuildConfig
instance HasBuildConfig BuildConfig where
    getBuildConfig = id
    {-# INLINE getBuildConfig #-}

-- An uninterpreted representation of configuration options.
-- Configurations may be "cascaded" using mappend (left-biased).
data ConfigMonoid =
  ConfigMonoid
    { configMonoidStackRoot          :: !(Maybe (Path Abs Dir))
    -- ^ See: 'configStackRoot'
    , configMonoidWorkDir            :: !(Maybe FilePath)
    -- ^ See: 'configWorkDir'.
    , configMonoidBuildOpts          :: !BuildOptsMonoid
    -- ^ build options.
    , configMonoidDockerOpts         :: !DockerOptsMonoid
    -- ^ Docker options.
    , configMonoidNixOpts            :: !NixOptsMonoid
    -- ^ Options for the execution environment (nix-shell or container)
    , configMonoidConnectionCount    :: !(Maybe Int)
    -- ^ See: 'configConnectionCount'
    , configMonoidHideTHLoading      :: !(Maybe Bool)
    -- ^ See: 'configHideTHLoading'
    , configMonoidLatestSnapshotUrl  :: !(Maybe Text)
    -- ^ Deprecated in favour of 'urlsMonoidLatestSnapshot'
    , configMonoidUrls               :: !UrlsMonoid
    -- ^ See: 'configUrls
    , configMonoidPackageIndices     :: !(Maybe [PackageIndex])
    -- ^ See: 'configPackageIndices'
    , configMonoidSystemGHC          :: !(Maybe Bool)
    -- ^ See: 'configSystemGHC'
    ,configMonoidInstallGHC          :: !(Maybe Bool)
    -- ^ See: 'configInstallGHC'
    ,configMonoidSkipGHCCheck        :: !(Maybe Bool)
    -- ^ See: 'configSkipGHCCheck'
    ,configMonoidSkipMsys            :: !(Maybe Bool)
    -- ^ See: 'configSkipMsys'
    ,configMonoidCompilerCheck       :: !(Maybe VersionCheck)
    -- ^ See: 'configCompilerCheck'
    ,configMonoidRequireStackVersion :: !VersionRange
    -- ^ See: 'configRequireStackVersion'
    ,configMonoidOS                  :: !(Maybe String)
    -- ^ Used for overriding the platform
    ,configMonoidArch                :: !(Maybe String)
    -- ^ Used for overriding the platform
    ,configMonoidGHCVariant          :: !(Maybe GHCVariant)
    -- ^ Used for overriding the GHC variant
    ,configMonoidJobs                :: !(Maybe Int)
    -- ^ See: 'configJobs'
    ,configMonoidExtraIncludeDirs    :: !(Set Text)
    -- ^ See: 'configExtraIncludeDirs'
    ,configMonoidExtraLibDirs        :: !(Set Text)
    -- ^ See: 'configExtraLibDirs'
    ,configMonoidConcurrentTests     :: !(Maybe Bool)
    -- ^ See: 'configConcurrentTests'
    ,configMonoidLocalBinPath        :: !(Maybe FilePath)
    -- ^ Used to override the binary installation dir
    ,configMonoidImageOpts           :: !ImageOptsMonoid
    -- ^ Image creation options.
    ,configMonoidTemplateParameters  :: !(Map Text Text)
    -- ^ Template parameters.
    ,configMonoidScmInit             :: !(Maybe SCM)
    -- ^ Initialize SCM (e.g. git init) when making new projects?
    ,configMonoidGhcOptions          :: !(Map (Maybe PackageName) [Text])
    -- ^ See 'configGhcOptions'
    ,configMonoidExtraPath           :: ![Path Abs Dir]
    -- ^ Additional paths to search for executables in
    ,configMonoidSetupInfoLocations  :: ![SetupInfoLocation]
    -- ^ Additional setup info (inline or remote) to use for installing tools
    ,configMonoidPvpBounds           :: !(Maybe PvpBounds)
    -- ^ See 'configPvpBounds'
    ,configMonoidModifyCodePage      :: !(Maybe Bool)
    -- ^ See 'configModifyCodePage'
    ,configMonoidExplicitSetupDeps   :: !(Map (Maybe PackageName) Bool)
    -- ^ See 'configExplicitSetupDeps'
    ,configMonoidRebuildGhcOptions   :: !(Maybe Bool)
    -- ^ See 'configMonoidRebuildGhcOptions'
    ,configMonoidApplyGhcOptions     :: !(Maybe ApplyGhcOptions)
    -- ^ See 'configApplyGhcOptions'
    ,configMonoidAllowNewer          :: !(Maybe Bool)
    -- ^ See 'configMonoidAllowNewer'
    ,configMonoidDefaultTemplate     :: !(Maybe TemplateName)
    -- ^ The default template to use when none is specified.
    -- (If Nothing, the default default is used.)
    , configMonoidAllowDifferentUser :: !(Maybe Bool)
    -- ^ Allow users other than the stack root owner to use the stack
    -- installation.
    }
  deriving Show

instance Monoid ConfigMonoid where
  mempty = ConfigMonoid
    { configMonoidStackRoot = Nothing
    , configMonoidWorkDir = Nothing
    , configMonoidBuildOpts = mempty
    , configMonoidDockerOpts = mempty
    , configMonoidNixOpts = mempty
    , configMonoidConnectionCount = Nothing
    , configMonoidHideTHLoading = Nothing
    , configMonoidLatestSnapshotUrl = Nothing
    , configMonoidUrls = mempty
    , configMonoidPackageIndices = Nothing
    , configMonoidSystemGHC = Nothing
    , configMonoidInstallGHC = Nothing
    , configMonoidSkipGHCCheck = Nothing
    , configMonoidSkipMsys = Nothing
    , configMonoidRequireStackVersion = anyVersion
    , configMonoidOS = Nothing
    , configMonoidArch = Nothing
    , configMonoidGHCVariant = Nothing
    , configMonoidJobs = Nothing
    , configMonoidExtraIncludeDirs = Set.empty
    , configMonoidExtraLibDirs = Set.empty
    , configMonoidConcurrentTests = Nothing
    , configMonoidLocalBinPath = Nothing
    , configMonoidImageOpts = mempty
    , configMonoidTemplateParameters = mempty
    , configMonoidScmInit = Nothing
    , configMonoidCompilerCheck = Nothing
    , configMonoidGhcOptions = mempty
    , configMonoidExtraPath = []
    , configMonoidSetupInfoLocations = mempty
    , configMonoidPvpBounds = Nothing
    , configMonoidModifyCodePage = Nothing
    , configMonoidExplicitSetupDeps = mempty
    , configMonoidRebuildGhcOptions = Nothing
    , configMonoidApplyGhcOptions = Nothing
    , configMonoidAllowNewer = Nothing
    , configMonoidDefaultTemplate = Nothing
    , configMonoidAllowDifferentUser = Nothing
    }
  mappend l r = ConfigMonoid
    { configMonoidStackRoot = configMonoidStackRoot l <|> configMonoidStackRoot r
    , configMonoidWorkDir = configMonoidWorkDir l <|> configMonoidWorkDir r
    , configMonoidBuildOpts = configMonoidBuildOpts l <> configMonoidBuildOpts r
    , configMonoidDockerOpts = configMonoidDockerOpts l <> configMonoidDockerOpts r
    , configMonoidNixOpts = configMonoidNixOpts l <> configMonoidNixOpts r
    , configMonoidConnectionCount = configMonoidConnectionCount l <|> configMonoidConnectionCount r
    , configMonoidHideTHLoading = configMonoidHideTHLoading l <|> configMonoidHideTHLoading r
    , configMonoidLatestSnapshotUrl = configMonoidLatestSnapshotUrl l <|> configMonoidLatestSnapshotUrl r
    , configMonoidUrls = configMonoidUrls l <> configMonoidUrls r
    , configMonoidPackageIndices = configMonoidPackageIndices l <|> configMonoidPackageIndices r
    , configMonoidSystemGHC = configMonoidSystemGHC l <|> configMonoidSystemGHC r
    , configMonoidInstallGHC = configMonoidInstallGHC l <|> configMonoidInstallGHC r
    , configMonoidSkipGHCCheck = configMonoidSkipGHCCheck l <|> configMonoidSkipGHCCheck r
    , configMonoidSkipMsys = configMonoidSkipMsys l <|> configMonoidSkipMsys r
    , configMonoidRequireStackVersion = intersectVersionRanges (configMonoidRequireStackVersion l)
                                                               (configMonoidRequireStackVersion r)
    , configMonoidOS = configMonoidOS l <|> configMonoidOS r
    , configMonoidArch = configMonoidArch l <|> configMonoidArch r
    , configMonoidGHCVariant = configMonoidGHCVariant l <|> configMonoidGHCVariant r
    , configMonoidJobs = configMonoidJobs l <|> configMonoidJobs r
    , configMonoidExtraIncludeDirs = Set.union (configMonoidExtraIncludeDirs l) (configMonoidExtraIncludeDirs r)
    , configMonoidExtraLibDirs = Set.union (configMonoidExtraLibDirs l) (configMonoidExtraLibDirs r)
    , configMonoidConcurrentTests = configMonoidConcurrentTests l <|> configMonoidConcurrentTests r
    , configMonoidLocalBinPath = configMonoidLocalBinPath l <|> configMonoidLocalBinPath r
    , configMonoidImageOpts = configMonoidImageOpts l <> configMonoidImageOpts r
    , configMonoidTemplateParameters = configMonoidTemplateParameters l <> configMonoidTemplateParameters r
    , configMonoidScmInit = configMonoidScmInit l <|> configMonoidScmInit r
    , configMonoidCompilerCheck = configMonoidCompilerCheck l <|> configMonoidCompilerCheck r
    , configMonoidGhcOptions = Map.unionWith (++) (configMonoidGhcOptions l) (configMonoidGhcOptions r)
    , configMonoidExtraPath = configMonoidExtraPath l ++ configMonoidExtraPath r
    , configMonoidSetupInfoLocations = configMonoidSetupInfoLocations l ++ configMonoidSetupInfoLocations r
    , configMonoidPvpBounds = configMonoidPvpBounds l <|> configMonoidPvpBounds r
    , configMonoidModifyCodePage = configMonoidModifyCodePage l <|> configMonoidModifyCodePage r
    , configMonoidExplicitSetupDeps = configMonoidExplicitSetupDeps l <> configMonoidExplicitSetupDeps r
    , configMonoidRebuildGhcOptions = configMonoidRebuildGhcOptions l <|> configMonoidRebuildGhcOptions r
    , configMonoidApplyGhcOptions = configMonoidApplyGhcOptions l <|> configMonoidApplyGhcOptions r
    , configMonoidAllowNewer = configMonoidAllowNewer l <|> configMonoidAllowNewer r
    , configMonoidDefaultTemplate = configMonoidDefaultTemplate l <|> configMonoidDefaultTemplate r
    , configMonoidAllowDifferentUser = configMonoidAllowDifferentUser l <|> configMonoidAllowDifferentUser r
    }

instance FromJSON (WithJSONWarnings ConfigMonoid) where
  parseJSON = withObjectWarnings "ConfigMonoid" parseConfigMonoidJSON

-- | Parse a partial configuration.  Used both to parse both a standalone config
-- file and a project file, so that a sub-parser is not required, which would interfere with
-- warnings for missing fields.
parseConfigMonoidJSON :: Object -> WarningParser ConfigMonoid
parseConfigMonoidJSON obj = do
    -- Parsing 'stackRoot' from 'stackRoot'/config.yaml would be nonsensical
    let configMonoidStackRoot = Nothing
    configMonoidWorkDir <- obj ..:? configMonoidWorkDirName
    configMonoidBuildOpts <- jsonSubWarnings (obj ..:? configMonoidBuildOptsName ..!= mempty)
    configMonoidDockerOpts <- jsonSubWarnings (obj ..:? configMonoidDockerOptsName ..!= mempty)
    configMonoidNixOpts <- jsonSubWarnings (obj ..:? configMonoidNixOptsName ..!= mempty)
    configMonoidConnectionCount <- obj ..:? configMonoidConnectionCountName
    configMonoidHideTHLoading <- obj ..:? configMonoidHideTHLoadingName
    configMonoidLatestSnapshotUrl <- obj ..:? configMonoidLatestSnapshotUrlName
    configMonoidUrls <- jsonSubWarnings (obj ..:? configMonoidUrlsName ..!= mempty)
    configMonoidPackageIndices <- jsonSubWarningsTT (obj ..:?  configMonoidPackageIndicesName)
    configMonoidSystemGHC <- obj ..:? configMonoidSystemGHCName
    configMonoidInstallGHC <- obj ..:? configMonoidInstallGHCName
    configMonoidSkipGHCCheck <- obj ..:? configMonoidSkipGHCCheckName
    configMonoidSkipMsys <- obj ..:? configMonoidSkipMsysName
    configMonoidRequireStackVersion <- unVersionRangeJSON <$>
                                       obj ..:? configMonoidRequireStackVersionName
                                           ..!= VersionRangeJSON anyVersion
    configMonoidOS <- obj ..:? configMonoidOSName
    configMonoidArch <- obj ..:? configMonoidArchName
    configMonoidGHCVariant <- obj ..:? configMonoidGHCVariantName
    configMonoidJobs <- obj ..:? configMonoidJobsName
    configMonoidExtraIncludeDirs <- obj ..:?  configMonoidExtraIncludeDirsName ..!= Set.empty
    configMonoidExtraLibDirs <- obj ..:?  configMonoidExtraLibDirsName ..!= Set.empty
    configMonoidConcurrentTests <- obj ..:? configMonoidConcurrentTestsName
    configMonoidLocalBinPath <- obj ..:? configMonoidLocalBinPathName
    configMonoidImageOpts <- jsonSubWarnings (obj ..:?  configMonoidImageOptsName ..!= mempty)
    templates <- obj ..:? "templates"
    (configMonoidScmInit,configMonoidTemplateParameters) <-
      case templates of
        Nothing -> return (Nothing,M.empty)
        Just tobj -> do
          scmInit <- tobj ..:? configMonoidScmInitName
          params <- tobj ..:? configMonoidTemplateParametersName
          return (scmInit,fromMaybe M.empty params)
    configMonoidCompilerCheck <- obj ..:? configMonoidCompilerCheckName

    mghcoptions <- obj ..:? configMonoidGhcOptionsName
    configMonoidGhcOptions <-
        case mghcoptions of
            Nothing -> return mempty
            Just m -> fmap Map.fromList $ mapM handleGhcOptions $ Map.toList m

    extraPath <- obj ..:? configMonoidExtraPathName ..!= []
    configMonoidExtraPath <- forM extraPath $
        either (fail . show) return . parseAbsDir . T.unpack

    configMonoidSetupInfoLocations <-
        maybeToList <$> jsonSubWarningsT (obj ..:?  configMonoidSetupInfoLocationsName)
    configMonoidPvpBounds <- obj ..:? configMonoidPvpBoundsName
    configMonoidModifyCodePage <- obj ..:? configMonoidModifyCodePageName
    configMonoidExplicitSetupDeps <-
        (obj ..:? configMonoidExplicitSetupDepsName ..!= mempty)
        >>= fmap Map.fromList . mapM handleExplicitSetupDep . Map.toList
    configMonoidRebuildGhcOptions <- obj ..:? configMonoidRebuildGhcOptionsName
    configMonoidApplyGhcOptions <- obj ..:? configMonoidApplyGhcOptionsName
    configMonoidAllowNewer <- obj ..:? configMonoidAllowNewerName
    configMonoidDefaultTemplate <- obj ..:? configMonoidDefaultTemplateName
    configMonoidAllowDifferentUser <- obj ..:? configMonoidAllowDifferentUserName

    return ConfigMonoid {..}
  where
    handleGhcOptions :: Monad m => (Text, Text) -> m (Maybe PackageName, [Text])
    handleGhcOptions (name', vals') = do
        name <-
            if name' == "*"
                then return Nothing
                else case parsePackageNameFromString $ T.unpack name' of
                        Left e -> fail $ show e
                        Right x -> return $ Just x

        case parseArgs Escaping vals' of
            Left e -> fail e
            Right vals -> return (name, map T.pack vals)

    handleExplicitSetupDep :: Monad m => (Text, Bool) -> m (Maybe PackageName, Bool)
    handleExplicitSetupDep (name', b) = do
        name <-
            if name' == "*"
                then return Nothing
                else case parsePackageNameFromString $ T.unpack name' of
                        Left e -> fail $ show e
                        Right x -> return $ Just x
        return (name, b)

configMonoidWorkDirName :: Text
configMonoidWorkDirName = "work-dir"

configMonoidBuildOptsName :: Text
configMonoidBuildOptsName = "build"

configMonoidDockerOptsName :: Text
configMonoidDockerOptsName = "docker"

configMonoidNixOptsName :: Text
configMonoidNixOptsName = "nix"

configMonoidConnectionCountName :: Text
configMonoidConnectionCountName = "connection-count"

configMonoidHideTHLoadingName :: Text
configMonoidHideTHLoadingName = "hide-th-loading"

configMonoidLatestSnapshotUrlName :: Text
configMonoidLatestSnapshotUrlName = "latest-snapshot-url"

configMonoidUrlsName :: Text
configMonoidUrlsName = "urls"

configMonoidPackageIndicesName :: Text
configMonoidPackageIndicesName = "package-indices"

configMonoidSystemGHCName :: Text
configMonoidSystemGHCName = "system-ghc"

configMonoidInstallGHCName :: Text
configMonoidInstallGHCName = "install-ghc"

configMonoidSkipGHCCheckName :: Text
configMonoidSkipGHCCheckName = "skip-ghc-check"

configMonoidSkipMsysName :: Text
configMonoidSkipMsysName = "skip-msys"

configMonoidRequireStackVersionName :: Text
configMonoidRequireStackVersionName = "require-stack-version"

configMonoidOSName :: Text
configMonoidOSName = "os"

configMonoidArchName :: Text
configMonoidArchName = "arch"

configMonoidGHCVariantName :: Text
configMonoidGHCVariantName = "ghc-variant"

configMonoidJobsName :: Text
configMonoidJobsName = "jobs"

configMonoidExtraIncludeDirsName :: Text
configMonoidExtraIncludeDirsName = "extra-include-dirs"

configMonoidExtraLibDirsName :: Text
configMonoidExtraLibDirsName = "extra-lib-dirs"

configMonoidConcurrentTestsName :: Text
configMonoidConcurrentTestsName = "concurrent-tests"

configMonoidLocalBinPathName :: Text
configMonoidLocalBinPathName = "local-bin-path"

configMonoidImageOptsName :: Text
configMonoidImageOptsName = "image"

configMonoidScmInitName :: Text
configMonoidScmInitName = "scm-init"

configMonoidTemplateParametersName :: Text
configMonoidTemplateParametersName = "params"

configMonoidCompilerCheckName :: Text
configMonoidCompilerCheckName = "compiler-check"

configMonoidGhcOptionsName :: Text
configMonoidGhcOptionsName = "ghc-options"

configMonoidExtraPathName :: Text
configMonoidExtraPathName = "extra-path"

configMonoidSetupInfoLocationsName :: Text
configMonoidSetupInfoLocationsName = "setup-info"

configMonoidPvpBoundsName :: Text
configMonoidPvpBoundsName = "pvp-bounds"

configMonoidModifyCodePageName :: Text
configMonoidModifyCodePageName = "modify-code-page"

configMonoidExplicitSetupDepsName :: Text
configMonoidExplicitSetupDepsName = "explicit-setup-deps"

configMonoidRebuildGhcOptionsName :: Text
configMonoidRebuildGhcOptionsName = "rebuild-ghc-options"

configMonoidApplyGhcOptionsName :: Text
configMonoidApplyGhcOptionsName = "apply-ghc-options"

configMonoidAllowNewerName :: Text
configMonoidAllowNewerName = "allow-newer"

configMonoidDefaultTemplateName :: Text
configMonoidDefaultTemplateName = "default-template"

configMonoidAllowDifferentUserName :: Text
configMonoidAllowDifferentUserName = "allow-different-user"

data ConfigException
  = ParseConfigFileException (Path Abs File) ParseException
  | ParseResolverException Text
  | NoProjectConfigFound (Path Abs Dir) (Maybe Text)
  | UnexpectedArchiveContents [Path Abs Dir] [Path Abs File]
  | UnableToExtractArchive Text (Path Abs File)
  | BadStackVersionException VersionRange
  | NoMatchingSnapshot (NonEmpty SnapName)
  | ResolverMismatch Resolver String
  | ResolverPartial Resolver String
  | NoSuchDirectory FilePath
  | ParseGHCVariantException String
  | BadStackRoot (Path Abs Dir)
  | Won'tCreateStackRootInDirectoryOwnedByDifferentUser (Path Abs Dir) (Path Abs Dir) -- ^ @$STACK_ROOT@, parent dir
  | UserDoesn'tOwnDirectory (Path Abs Dir)
  deriving Typeable
instance Show ConfigException where
    show (ParseConfigFileException configFile exception) = concat
        [ "Could not parse '"
        , toFilePath configFile
        , "':\n"
        , show exception
        , "\nSee http://docs.haskellstack.org/en/stable/yaml_configuration/."
        ]
    show (ParseResolverException t) = concat
        [ "Invalid resolver value: "
        , T.unpack t
        , ". Possible valid values include lts-2.12, nightly-YYYY-MM-DD, ghc-7.10.2, and ghcjs-0.1.0_ghc-7.10.2. "
        , "See https://www.stackage.org/snapshots for a complete list."
        ]
    show (NoProjectConfigFound dir mcmd) = concat
        [ "Unable to find a stack.yaml file in the current directory ("
        , toFilePath dir
        , ") or its ancestors"
        , case mcmd of
            Nothing -> ""
            Just cmd -> "\nRecommended action: stack " ++ T.unpack cmd
        ]
    show (UnexpectedArchiveContents dirs files) = concat
        [ "When unpacking an archive specified in your stack.yaml file, "
        , "did not find expected contents. Expected: a single directory. Found: "
        , show ( map (toFilePath . dirname) dirs
               , map (toFilePath . filename) files
               )
        ]
    show (UnableToExtractArchive url file) = concat
        [ "Archive extraction failed. We support tarballs and zip, couldn't handle the following URL, "
        , T.unpack url, " downloaded to the file ", toFilePath $ filename file
        ]
    show (BadStackVersionException requiredRange) = concat
        [ "The version of stack you are using ("
        , show (fromCabalVersion Meta.version)
        , ") is outside the required\n"
        ,"version range specified in stack.yaml ("
        , T.unpack (versionRangeText requiredRange)
        , ")." ]
    show (NoMatchingSnapshot names) = concat
        [ "None of the following snapshots provides a compiler matching "
        , "your package(s):\n"
        , unlines $ map (\name -> "    - " <> T.unpack (renderSnapName name))
                        (NonEmpty.toList names)
        , "\nYou can try the following options:\n"
        , "    - Use '--omit-packages to exclude mismatching package(s).\n"
        , "    - Use '--resolver' to specify a matching snapshot/resolver\n"
        ]
    show (ResolverMismatch resolver errDesc) = concat
        [ "Resolver '"
        , T.unpack (resolverName resolver)
        , "' does not have a matching compiler to build some or all of your "
        , "package(s).\n"
        , errDesc
        , "\nHowever, you can try '--omit-packages to exclude mismatching "
        , "package(s)."
        ]
    show (ResolverPartial resolver errDesc) = concat
        [ "Resolver '"
        , T.unpack (resolverName resolver)
        , "' does not have all the packages to match your requirements.\n"
        , unlines $ fmap ("    " <>) (lines errDesc)
        , "\nHowever, you can try '--solver' to use external packages."
        , "\nUse '--omit-packages' if you want to create a config anyway."
        ]
    show (NoSuchDirectory dir) = concat
        ["No directory could be located matching the supplied path: "
        ,dir
        ]
    show (ParseGHCVariantException v) = concat
        [ "Invalid ghc-variant value: "
        , v
        ]
    show (BadStackRoot stackRoot) = concat
        [ "Invalid stack root: '"
        , toFilePath stackRoot
        , "'. Please provide a valid absolute path."
        ]
    show (Won'tCreateStackRootInDirectoryOwnedByDifferentUser envStackRoot parentDir) = concat
        [ "Preventing creation of stack root '"
        , toFilePath envStackRoot
        , "'. Parent directory '"
        , toFilePath parentDir
        , "' is owned by someone else."
        ]
    show (UserDoesn'tOwnDirectory dir) = concat
        [ "You are not the owner of '"
        , toFilePath dir
        , "'. Aborting to protect file permissions."
        , "\nRetry with '--"
        , T.unpack configMonoidAllowDifferentUserName
        , "' to disable this precaution."
        ]
instance Exception ConfigException

-- | Helper function to ask the environment and apply getConfig
askConfig :: (MonadReader env m, HasConfig env) => m Config
askConfig = liftM getConfig ask

-- | Get the URL to request the information on the latest snapshots
askLatestSnapshotUrl :: (MonadReader env m, HasConfig env) => m Text
askLatestSnapshotUrl = asks (urlsLatestSnapshot . configUrls . getConfig)

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

-- | @".stack-work"@
getWorkDir :: (MonadReader env m, HasConfig env) => m (Path Rel Dir)
getWorkDir = configWorkDir `liftM` asks getConfig

-- | Per-project work dir
getProjectWorkDir :: (HasBuildConfig env, MonadReader env m) => m (Path Abs Dir)
getProjectWorkDir = do
    bc      <- asks getBuildConfig
    workDir <- getWorkDir
    return (bcRoot bc </> workDir)

-- | File containing the installed cache, see "Stack.PackageDump"
configInstalledCache :: (HasBuildConfig env, MonadReader env m) => m (Path Abs File)
configInstalledCache = liftM (</> $(mkRelFile "installed-cache.bin")) getProjectWorkDir

-- | Relative directory for the platform identifier
platformOnlyRelDir
    :: (MonadReader env m, HasPlatform env, MonadThrow m)
    => m (Path Rel Dir)
platformOnlyRelDir = do
    platform <- asks getPlatform
    platformVariant <- asks getPlatformVariant
    parseRelDir (Distribution.Text.display platform ++ platformVariantSuffix platformVariant)

-- | Directory containing snapshots
snapshotsDir :: (MonadReader env m, HasConfig env, HasGHCVariant env, MonadThrow m) => m (Path Abs Dir)
snapshotsDir = do
    config <- asks getConfig
    platform <- platformGhcRelDir
    return $ configStackRoot config </> $(mkRelDir "snapshots") </> platform

-- | Installation root for dependencies
installationRootDeps :: (MonadThrow m, MonadReader env m, HasEnvConfig env) => m (Path Abs Dir)
installationRootDeps = do
    config <- asks getConfig
    -- TODO: also useShaPathOnWindows here, once #1173 is resolved.
    psc <- platformSnapAndCompilerRel
    return $ configStackRoot config </> $(mkRelDir "snapshots") </> psc

-- | Installation root for locals
installationRootLocal :: (MonadThrow m, MonadReader env m, HasEnvConfig env) => m (Path Abs Dir)
installationRootLocal = do
    bc <- asks getBuildConfig
    psc <- useShaPathOnWindows =<< platformSnapAndCompilerRel
    return $ getProjectWorkDir bc </> $(mkRelDir "install") </> psc

-- | Path for platform followed by snapshot name followed by compiler
-- name.
platformSnapAndCompilerRel
    :: (MonadReader env m, HasPlatform env, HasEnvConfig env, MonadThrow m)
    => m (Path Rel Dir)
platformSnapAndCompilerRel = do
    bc <- asks getBuildConfig
    platform <- platformGhcRelDir
    name <- parseRelDir $ T.unpack $ resolverName $ bcResolver bc
    ghc <- compilerVersionDir
    useShaPathOnWindows (platform </> name </> ghc)

-- | Relative directory for the platform identifier
platformGhcRelDir
    :: (MonadReader env m, HasPlatform env, HasGHCVariant env, MonadThrow m)
    => m (Path Rel Dir)
platformGhcRelDir = do
    platform <- asks getPlatform
    platformVariant <- asks getPlatformVariant
    ghcVariant <- asks getGHCVariant
    parseRelDir (mconcat [ Distribution.Text.display platform
                         , platformVariantSuffix platformVariant
                         , ghcVariantSuffix ghcVariant ])

-- | This is an attempt to shorten stack paths on Windows to decrease our
-- chances of hitting 260 symbol path limit. The idea is to calculate
-- SHA1 hash of the path used on other architectures, encode with base
-- 16 and take first 8 symbols of it.
useShaPathOnWindows :: MonadThrow m => Path Rel Dir -> m (Path Rel Dir)
useShaPathOnWindows =
#ifdef mingw32_HOST_OS
    parseRelDir . S8.unpack . S8.take 8 . B16.encode . SHA1.hash . encodeUtf8 . T.pack . toFilePath
#else
    return
#endif

compilerVersionDir :: (MonadThrow m, MonadReader env m, HasEnvConfig env) => m (Path Rel Dir)
compilerVersionDir = do
    compilerVersion <- asks (envConfigCompilerVersion . getEnvConfig)
    parseRelDir $ case compilerVersion of
        GhcVersion version -> versionString version
        GhcjsVersion {} -> compilerVersionString compilerVersion

-- | Package database for installing dependencies into
packageDatabaseDeps :: (MonadThrow m, MonadReader env m, HasEnvConfig env) => m (Path Abs Dir)
packageDatabaseDeps = do
    root <- installationRootDeps
    return $ root </> $(mkRelDir "pkgdb")

-- | Package database for installing local packages into
packageDatabaseLocal :: (MonadThrow m, MonadReader env m, HasEnvConfig env) => m (Path Abs Dir)
packageDatabaseLocal = do
    root <- installationRootLocal
    return $ root </> $(mkRelDir "pkgdb")

-- | Extra package databases
packageDatabaseExtra :: (MonadThrow m, MonadReader env m, HasEnvConfig env) => m [Path Abs Dir]
packageDatabaseExtra = do
    bc <- asks getBuildConfig
    return $ bcExtraPackageDBs bc

-- | Directory for holding flag cache information
flagCacheLocal :: (MonadThrow m, MonadReader env m, HasEnvConfig env) => m (Path Abs Dir)
flagCacheLocal = do
    root <- installationRootLocal
    return $ root </> $(mkRelDir "flag-cache")

-- | Where to store mini build plan caches
configMiniBuildPlanCache :: (MonadThrow m, MonadReader env m, HasConfig env, HasGHCVariant env)
                         => SnapName
                         -> m (Path Abs File)
configMiniBuildPlanCache name = do
    root <- asks getStackRoot
    platform <- platformGhcRelDir
    file <- parseRelFile $ T.unpack (renderSnapName name) ++ ".cache"
    -- Yes, cached plans differ based on platform
    return (root </> $(mkRelDir "build-plan-cache") </> platform </> file)

-- | Suffix applied to an installation root to get the bin dir
bindirSuffix :: Path Rel Dir
bindirSuffix = $(mkRelDir "bin")

-- | Suffix applied to an installation root to get the doc dir
docDirSuffix :: Path Rel Dir
docDirSuffix = $(mkRelDir "doc")

-- | Where HPC reports and tix files get stored.
hpcReportDir :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
             => m (Path Abs Dir)
hpcReportDir = do
   root <- installationRootLocal
   return $ root </> $(mkRelDir "hpc")

-- | Get the extra bin directories (for the PATH). Puts more local first
--
-- Bool indicates whether or not to include the locals
extraBinDirs :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
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
    liftIO $ configEnvOverride config minimalEnvSettings

minimalEnvSettings :: EnvSettings
minimalEnvSettings =
    EnvSettings
    { esIncludeLocals = False
    , esIncludeGhcPackagePath = False
    , esStackExe = False
    , esLocaleUtf8 = False
    }

getWhichCompiler :: (MonadReader env m, HasEnvConfig env) => m WhichCompiler
getWhichCompiler = asks (whichCompiler . envConfigCompilerVersion . getEnvConfig)

-- | Get the path for the given compiler ignoring any local binaries.
--
-- https://github.com/commercialhaskell/stack/issues/1052
getCompilerPath
    :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env)
    => WhichCompiler
    -> m (Path Abs File)
getCompilerPath wc = do
    config <- asks getConfig
    eoWithoutLocals <- liftIO $
        configEnvOverride config minimalEnvSettings { esLocaleUtf8 = True }
    join (findExecutable eoWithoutLocals (compilerExeName wc))

data ProjectAndConfigMonoid
  = ProjectAndConfigMonoid !Project !ConfigMonoid

instance FromJSON (WithJSONWarnings ProjectAndConfigMonoid) where
    parseJSON = withObjectWarnings "ProjectAndConfigMonoid" $ \o -> do
        dirs <- jsonSubWarningsTT (o ..:? "packages") ..!= [packageEntryCurrDir]
        extraDeps' <- o ..:? "extra-deps" ..!= []
        extraDeps <-
            case partitionEithers $ goDeps extraDeps' of
                ([], x) -> return $ Map.fromList x
                (errs, _) -> fail $ unlines errs

        flags <- o ..:? "flags" ..!= mempty
        resolver <- jsonSubWarnings (o ..: "resolver")
        compiler <- o ..:? "compiler"
        msg <- o ..:? "user-message"
        config <- parseConfigMonoidJSON o
        extraPackageDBs <- o ..:? "extra-package-dbs" ..!= []
        let project = Project
                { projectUserMsg = msg
                , projectPackages = dirs
                , projectExtraDeps = extraDeps
                , projectFlags = flags
                , projectResolver = resolver
                , projectCompiler = compiler
                , projectExtraPackageDBs = extraPackageDBs
                }
        return $ ProjectAndConfigMonoid project config
      where
        goDeps =
            map toSingle . Map.toList . Map.unionsWith Set.union . map toMap
          where
            toMap i = Map.singleton
                (packageIdentifierName i)
                (Set.singleton (packageIdentifierVersion i))

        toSingle (k, s) =
            case Set.toList s of
                [x] -> Right (k, x)
                xs -> Left $ concat
                    [ "Multiple versions for package "
                    , packageNameString k
                    , ": "
                    , unwords $ map versionString xs
                    ]

-- | A PackageEntry for the current directory, used as a default
packageEntryCurrDir :: PackageEntry
packageEntryCurrDir = PackageEntry
    { peExtraDep = False
    , peLocation = PLFilePath "."
    , peSubdirs = []
    }

-- | A software control system.
data SCM = Git
  deriving (Show)

instance FromJSON SCM where
    parseJSON v = do
        s <- parseJSON v
        case s of
            "git" -> return Git
            _ -> fail ("Unknown or unsupported SCM: " <> s)

instance ToJSON SCM where
    toJSON Git = toJSON ("git" :: Text)

-- | A variant of the platform, used to differentiate Docker builds from host
data PlatformVariant = PlatformVariantNone
                     | PlatformVariant String

-- | Render a platform variant to a String suffix.
platformVariantSuffix :: PlatformVariant -> String
platformVariantSuffix PlatformVariantNone = ""
platformVariantSuffix (PlatformVariant v) = "-" ++ v

-- | Specialized bariant of GHC (e.g. libgmp4 or integer-simple)
data GHCVariant
    = GHCStandard -- ^ Standard bindist
    | GHCGMP4 -- ^ Bindist that supports libgmp4 (centos66)
    | GHCArch -- ^ Bindist built on Arch Linux (bleeding-edge)
    | GHCIntegerSimple -- ^ Bindist that uses integer-simple
    | GHCCustom String -- ^ Other bindists
    deriving (Show)

instance FromJSON GHCVariant where
    -- Strange structuring is to give consistent error messages
    parseJSON =
        withText
            "GHCVariant"
            (either (fail . show) return . parseGHCVariant . T.unpack)

-- | Render a GHC variant to a String.
ghcVariantName :: GHCVariant -> String
ghcVariantName GHCStandard = "standard"
ghcVariantName GHCGMP4 = "gmp4"
ghcVariantName GHCArch = "arch"
ghcVariantName GHCIntegerSimple = "integersimple"
ghcVariantName (GHCCustom name) = "custom-" ++ name

-- | Render a GHC variant to a String suffix.
ghcVariantSuffix :: GHCVariant -> String
ghcVariantSuffix GHCStandard = ""
ghcVariantSuffix v = "-" ++ ghcVariantName v

-- | Parse GHC variant from a String.
parseGHCVariant :: (MonadThrow m) => String -> m GHCVariant
parseGHCVariant s =
    case stripPrefix "custom-" s of
        Just name -> return (GHCCustom name)
        Nothing
          | s == "" -> return GHCStandard
          | s == "standard" -> return GHCStandard
          | s == "gmp4" -> return GHCGMP4
          | s == "arch" -> return GHCArch
          | s == "integersimple" -> return GHCIntegerSimple
          | otherwise -> return (GHCCustom s)

-- | Information for a file to download.
data DownloadInfo = DownloadInfo
    { downloadInfoUrl :: Text
    , downloadInfoContentLength :: Maybe Int
    , downloadInfoSha1 :: Maybe ByteString
    } deriving (Show)

instance FromJSON (WithJSONWarnings DownloadInfo) where
    parseJSON = withObjectWarnings "DownloadInfo" parseDownloadInfoFromObject

-- | Parse JSON in existing object for 'DownloadInfo'
parseDownloadInfoFromObject :: Object -> WarningParser DownloadInfo
parseDownloadInfoFromObject o = do
    url <- o ..: "url"
    contentLength <- o ..:? "content-length"
    sha1TextMay <- o ..:? "sha1"
    return
        DownloadInfo
        { downloadInfoUrl = url
        , downloadInfoContentLength = contentLength
        , downloadInfoSha1 = fmap encodeUtf8 sha1TextMay
        }

data VersionedDownloadInfo = VersionedDownloadInfo
    { vdiVersion :: Version
    , vdiDownloadInfo :: DownloadInfo
    }
    deriving Show

instance FromJSON (WithJSONWarnings VersionedDownloadInfo) where
    parseJSON = withObjectWarnings "VersionedDownloadInfo" $ \o -> do
        version <- o ..: "version"
        downloadInfo <- parseDownloadInfoFromObject o
        return VersionedDownloadInfo
            { vdiVersion = version
            , vdiDownloadInfo = downloadInfo
            }

data SetupInfo = SetupInfo
    { siSevenzExe :: Maybe DownloadInfo
    , siSevenzDll :: Maybe DownloadInfo
    , siMsys2 :: Map Text VersionedDownloadInfo
    , siGHCs :: Map Text (Map Version DownloadInfo)
    , siGHCJSs :: Map Text (Map CompilerVersion DownloadInfo)
    , siStack :: Map Text (Map Version DownloadInfo)
    }
    deriving Show

instance FromJSON (WithJSONWarnings SetupInfo) where
    parseJSON = withObjectWarnings "SetupInfo" $ \o -> do
        siSevenzExe <- jsonSubWarningsT (o ..:? "sevenzexe-info")
        siSevenzDll <- jsonSubWarningsT (o ..:? "sevenzdll-info")
        siMsys2 <- jsonSubWarningsT (o ..:? "msys2" ..!= mempty)
        siGHCs <- jsonSubWarningsTT (o ..:? "ghc" ..!= mempty)
        siGHCJSs <- jsonSubWarningsTT (o ..:? "ghcjs" ..!= mempty)
        siStack <- jsonSubWarningsTT (o ..:? "stack" ..!= mempty)
        return SetupInfo {..}

-- | For @siGHCs@ and @siGHCJSs@ fields maps are deeply merged.
-- For all fields the values from the last @SetupInfo@ win.
instance Monoid SetupInfo where
    mempty =
        SetupInfo
        { siSevenzExe = Nothing
        , siSevenzDll = Nothing
        , siMsys2 = Map.empty
        , siGHCs = Map.empty
        , siGHCJSs = Map.empty
        , siStack = Map.empty
        }
    mappend l r =
        SetupInfo
        { siSevenzExe = siSevenzExe r <|> siSevenzExe l
        , siSevenzDll = siSevenzDll r <|> siSevenzDll l
        , siMsys2 = siMsys2 r <> siMsys2 l
        , siGHCs = Map.unionWith (<>) (siGHCs r) (siGHCs l)
        , siGHCJSs = Map.unionWith (<>) (siGHCJSs r) (siGHCJSs l)
        , siStack = Map.unionWith (<>) (siStack l) (siStack r) }

-- | Remote or inline 'SetupInfo'
data SetupInfoLocation
    = SetupInfoFileOrURL String
    | SetupInfoInline SetupInfo
    deriving (Show)

instance FromJSON (WithJSONWarnings SetupInfoLocation) where
    parseJSON v =
        (noJSONWarnings <$>
         withText "SetupInfoFileOrURL" (pure . SetupInfoFileOrURL . T.unpack) v) <|>
        inline
      where
        inline = do
            WithJSONWarnings si w <- parseJSON v
            return $ WithJSONWarnings (SetupInfoInline si) w

-- | How PVP bounds should be added to .cabal files
data PvpBounds
  = PvpBoundsNone
  | PvpBoundsUpper
  | PvpBoundsLower
  | PvpBoundsBoth
  deriving (Show, Read, Eq, Typeable, Ord, Enum, Bounded)

pvpBoundsText :: PvpBounds -> Text
pvpBoundsText PvpBoundsNone = "none"
pvpBoundsText PvpBoundsUpper = "upper"
pvpBoundsText PvpBoundsLower = "lower"
pvpBoundsText PvpBoundsBoth = "both"

parsePvpBounds :: Text -> Either String PvpBounds
parsePvpBounds t =
    case Map.lookup t m of
        Nothing -> Left $ "Invalid PVP bounds: " ++ T.unpack t
        Just x -> Right x
  where
    m = Map.fromList $ map (pvpBoundsText &&& id) [minBound..maxBound]

instance ToJSON PvpBounds where
  toJSON = toJSON . pvpBoundsText
instance FromJSON PvpBounds where
  parseJSON = withText "PvpBounds" (either fail return . parsePvpBounds)

-- | Provide an explicit list of package dependencies when running a custom Setup.hs
explicitSetupDeps :: (MonadReader env m, HasConfig env) => PackageName -> m Bool
explicitSetupDeps name = do
    m <- asks $ configExplicitSetupDeps . getConfig
    return $
        -- Yes there are far cleverer ways to write this. I honestly consider
        -- the explicit pattern matching much easier to parse at a glance.
        case Map.lookup (Just name) m of
            Just b -> b
            Nothing ->
                case Map.lookup Nothing m of
                    Just b -> b
                    Nothing -> False -- default value

-- | Data passed into Docker container for the Docker entrypoint's use
data DockerEntrypoint = DockerEntrypoint
    { deUser :: !(Maybe DockerUser)
      -- ^ UID/GID/etc of host user, if we wish to perform UID/GID switch in container
    } deriving (Read,Show)

-- | Docker host user info
data DockerUser = DockerUser
    { duUid :: UserID -- ^ uid
    , duGid :: GroupID -- ^ gid
    , duGroups :: [GroupID] -- ^ Supplemantal groups
    , duUmask :: FileMode -- ^ File creation mask }
    } deriving (Read,Show)
