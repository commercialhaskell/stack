{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

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
  ,bcWantedCompiler
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
  -- ** WhichSolverCmd
  ,WhichSolverCmd(..)
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

  -- Re-exports
  ,PackageIndex(..)
  ,IndexName(..)
  ,indexNameText
  ,IndexLocation(..)
  -- Config fields
  ,configPackageIndex
  ,configPackageIndexCache
  ,configPackageIndexGz
  ,configPackageIndexRoot
  ,configPackageIndexRepo
  ,configPackageTarball
  -- ** Project & ProjectAndConfigMonoid
  ,Project(..)
  ,ProjectAndConfigMonoid(..)
  -- ** PvpBounds
  ,PvpBounds(..)
  ,parsePvpBounds
  -- ** Resolver & AbstractResolver
  ,Resolver
  ,LoadedResolver
  ,ResolverThat's(..)
  ,parseResolverText
  ,resolverDirName
  ,resolverName
  ,customResolverHash
  ,toResolverNotLoaded
  ,AbstractResolver(..)
  -- ** SCM
  ,SCM(..)
  -- ** CustomSnapshot
  ,CustomSnapshot(..)
  -- ** GhcOptions
  ,GhcOptions(..)
  ,ghcOptionsFor
  -- ** PackageFlags
  ,PackageFlags(..)
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
  ,hoogleRoot
  ,hoogleDatabasePath
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
import           Control.Monad (liftM, mzero, join)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.Logger (LogLevel(..))
import           Control.Monad.Reader (MonadReader, ask, asks, MonadIO, liftIO)
import           Data.Aeson.Extended
                 (ToJSON, toJSON, FromJSON, parseJSON, withText, object,
                  (.=), (..:), (..:?), (..!=), Value(String, Object),
                  withObjectWarnings, WarningParser, Object, jsonSubWarnings,
                  jsonSubWarningsT, jsonSubWarningsTT, WithJSONWarnings(..), noJSONWarnings)
import           Data.Attoparsec.Args
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Either (partitionEithers)
import           Data.IORef (IORef)
import           Data.List (stripPrefix)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid.Extra
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Typeable
import           Data.Yaml (ParseException)
import qualified Data.Yaml as Yaml
import           Distribution.System (Platform)
import qualified Distribution.Text
import           Distribution.Version (anyVersion)
import           GHC.Generics (Generic)
import           Generics.Deriving.Monoid (memptydefault, mappenddefault)
import           Network.HTTP.Client (parseRequest)
import           Path
import qualified Paths_stack as Meta
import           Stack.Types.BuildPlan (MiniBuildPlan(..), SnapName, renderSnapName, parseSnapName, SnapshotHash (..), trimmedSnapshotHash)
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
import           System.FilePath (takeBaseName)
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
         -- ^ On Windows: don't use a sandboxed MSYS
         ,configCompilerCheck       :: !VersionCheck
         -- ^ Specifies which versions of the compiler are acceptable.
         ,configLocalBin            :: !(Path Abs Dir)
         -- ^ Directory we should install executables into
         ,configRequireStackVersion :: !VersionRange
         -- ^ Require a version of stack within this range.
         ,configJobs                :: !Int
         -- ^ How many concurrent jobs to run, defaults to number of capabilities
         ,configOverrideGccPath     :: !(Maybe (Path Abs File))
         -- ^ Optional gcc override path
         ,configExtraIncludeDirs    :: !(Set (Path Abs Dir))
         -- ^ --extra-include-dirs arguments
         ,configExtraLibDirs        :: !(Set (Path Abs Dir))
         -- ^ --extra-lib-dirs arguments
         ,configConcurrentTests     :: !Bool
         -- ^ Run test suites concurrently
         ,configImage               :: !ImageOpts
         ,configTemplateParams      :: !(Map Text Text)
         -- ^ Parameters for templates.
         ,configScmInit             :: !(Maybe SCM)
         -- ^ Initialize SCM (e.g. git) when creating new projects.
         ,configGhcOptions          :: !GhcOptions
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

-- | Which packages do ghc-options on the command line apply to?
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
    { globalMonoidReExecVersion :: !(First String) -- ^ Expected re-exec in container version
    , globalMonoidDockerEntrypoint :: !(First DockerEntrypoint)
      -- ^ Data used when stack is acting as a Docker entrypoint (internal use only)
    , globalMonoidLogLevel     :: !(First LogLevel) -- ^ Log level
    , globalMonoidConfigMonoid :: !ConfigMonoid -- ^ Config monoid, for passing into 'loadConfig'
    , globalMonoidResolver     :: !(First AbstractResolver) -- ^ Resolver override
    , globalMonoidCompiler     :: !(First CompilerVersion) -- ^ Compiler override
    , globalMonoidTerminal     :: !(First Bool) -- ^ We're in a terminal?
    , globalMonoidStackYaml    :: !(First FilePath) -- ^ Override project stack.yaml
    } deriving (Show, Generic)

instance Monoid GlobalOptsMonoid where
    mempty = memptydefault
    mappend = mappenddefault

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
    , bcResolver   :: !LoadedResolver
      -- ^ How we resolve which dependencies to install given a set of
      -- packages.
    , bcWantedMiniBuildPlan :: !MiniBuildPlan
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
    , bcFlags      :: !PackageFlags
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

bcWantedCompiler :: BuildConfig -> CompilerVersion
bcWantedCompiler = mbpCompilerVersion . bcWantedMiniBuildPlan

-- | Configuration after the environment has been setup.
data EnvConfig = EnvConfig
    {envConfigBuildConfig :: !BuildConfig
    ,envConfigCabalVersion :: !Version
    -- ^ This is the version of Cabal that stack will use to compile Setup.hs files
    -- in the build process.
    --
    -- Note that this is not necessarily the same version as the one that stack
    -- depends on as a library and which is displayed when running
    -- @stack list-dependencies | grep Cabal@ in the stack project.
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
            PackageEntry
                { peExtraDep = False
                , peLocation = loc
                , peSubdirs = []
                }
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
    toJSON (PLRemote x (RPTGit y)) = object [("git", toJSON x), ("commit", toJSON y)]
    toJSON (PLRemote x (RPTHg  y)) = object [( "hg", toJSON x), ("commit", toJSON y)]

instance FromJSON (WithJSONWarnings PackageLocation) where
    parseJSON v
        = (noJSONWarnings <$> withText "PackageLocation" (\t -> http t <|> file t) v)
        <|> git v
        <|> hg  v
      where
        file t = pure $ PLFilePath $ T.unpack t
        http t =
            case parseRequest $ T.unpack t of
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
    , projectFlags :: !PackageFlags
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
        maybe id (\cv -> (("compiler" .= cv) :)) (projectCompiler p) $
        maybe id (\msg -> (("user-message" .= msg) :)) (projectUserMsg p)
        [ "packages"          .= projectPackages p
        , "extra-deps"        .= map fromTuple (Map.toList $ projectExtraDeps p)
        , "flags"             .= projectFlags p
        , "resolver"          .= projectResolver p
        , "extra-package-dbs" .= projectExtraPackageDBs p
        ]

data IsLoaded = Loaded | NotLoaded

type LoadedResolver = ResolverThat's 'Loaded
type Resolver = ResolverThat's 'NotLoaded

-- TODO: once GHC 8.0 is the lowest version we support, make these into
-- actual haddock comments...

-- | How we resolve which dependencies to install given a set of packages.
data ResolverThat's (l :: IsLoaded) where
    -- Use an official snapshot from the Stackage project, either an LTS
    -- Haskell or Stackage Nightly.
    ResolverSnapshot :: !SnapName -> ResolverThat's l
    -- Require a specific compiler version, but otherwise provide no
    -- build plan. Intended for use cases where end user wishes to
    -- specify all upstream dependencies manually, such as using a
    -- dependency solver.
    ResolverCompiler :: !CompilerVersion -> ResolverThat's l
    -- A custom resolver based on the given name and URL. When a URL is
    -- provided, it file is to be completely immutable. Filepaths are
    -- always loaded. This constructor is used before the build-plan has
    -- been loaded, as we do not yet know the custom snapshot's hash.
    ResolverCustom :: !Text -> !Text -> ResolverThat's 'NotLoaded
    -- Like 'ResolverCustom', but after loading the build-plan, so we
    -- have a hash. This is necessary in order to identify the location
    -- files are stored for the resolver.
    ResolverCustomLoaded :: !Text -> !Text -> !SnapshotHash -> ResolverThat's 'Loaded

deriving instance Eq (ResolverThat's k)
deriving instance Show (ResolverThat's k)

instance ToJSON (ResolverThat's k) where
    toJSON x = case x of
        ResolverSnapshot{} -> toJSON $ resolverName x
        ResolverCompiler{} -> toJSON $ resolverName x
        ResolverCustom n l -> handleCustom n l
        ResolverCustomLoaded n l _ -> handleCustom n l
      where
        handleCustom n l = object
             [ "name" .= n
             , "location" .= l
             ]
instance FromJSON (WithJSONWarnings (ResolverThat's 'NotLoaded)) where
    -- Strange structuring is to give consistent error messages
    parseJSON v@(Object _) = withObjectWarnings "Resolver" (\o -> ResolverCustom
        <$> o ..: "name"
        <*> o ..: "location") v

    parseJSON (String t) = either (fail . show) return (noJSONWarnings <$> parseResolverText t)

    parseJSON _ = fail "Invalid Resolver, must be Object or String"

-- | Convert a Resolver into its @Text@ representation, as will be used by
-- directory names
resolverDirName :: LoadedResolver -> Text
resolverDirName (ResolverSnapshot name) = renderSnapName name
resolverDirName (ResolverCompiler v) = compilerVersionText v
resolverDirName (ResolverCustomLoaded name _ hash) = "custom-" <> name <> "-" <> decodeUtf8 (trimmedSnapshotHash hash)

-- | Convert a Resolver into its @Text@ representation for human
-- presentation.
resolverName :: ResolverThat's l -> Text
resolverName (ResolverSnapshot name) = renderSnapName name
resolverName (ResolverCompiler v) = compilerVersionText v
resolverName (ResolverCustom name _) = "custom-" <> name
resolverName (ResolverCustomLoaded name _ _) = "custom-" <> name

customResolverHash :: LoadedResolver-> Maybe SnapshotHash
customResolverHash (ResolverCustomLoaded _ _ hash) = Just hash
customResolverHash _ = Nothing

-- | Try to parse a @Resolver@ from a @Text@. Won't work for complex resolvers (like custom).
parseResolverText :: MonadThrow m => Text -> m Resolver
parseResolverText t
    | Right x <- parseSnapName t = return $ ResolverSnapshot x
    | Just v <- parseCompilerVersion t = return $ ResolverCompiler v
    | otherwise = throwM $ ParseResolverException t

toResolverNotLoaded :: LoadedResolver -> Resolver
toResolverNotLoaded r = case r of
    ResolverSnapshot s -> ResolverSnapshot s
    ResolverCompiler v -> ResolverCompiler v
    ResolverCustomLoaded n l _ -> ResolverCustom n l

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
    { configMonoidStackRoot          :: !(First (Path Abs Dir))
    -- ^ See: 'configStackRoot'
    , configMonoidWorkDir            :: !(First (Path Rel Dir))
    -- ^ See: 'configWorkDir'.
    , configMonoidBuildOpts          :: !BuildOptsMonoid
    -- ^ build options.
    , configMonoidDockerOpts         :: !DockerOptsMonoid
    -- ^ Docker options.
    , configMonoidNixOpts            :: !NixOptsMonoid
    -- ^ Options for the execution environment (nix-shell or container)
    , configMonoidConnectionCount    :: !(First Int)
    -- ^ See: 'configConnectionCount'
    , configMonoidHideTHLoading      :: !(First Bool)
    -- ^ See: 'configHideTHLoading'
    , configMonoidLatestSnapshotUrl  :: !(First Text)
    -- ^ Deprecated in favour of 'urlsMonoidLatestSnapshot'
    , configMonoidUrls               :: !UrlsMonoid
    -- ^ See: 'configUrls
    , configMonoidPackageIndices     :: !(First [PackageIndex])
    -- ^ See: 'configPackageIndices'
    , configMonoidSystemGHC          :: !(First Bool)
    -- ^ See: 'configSystemGHC'
    ,configMonoidInstallGHC          :: !(First Bool)
    -- ^ See: 'configInstallGHC'
    ,configMonoidSkipGHCCheck        :: !(First Bool)
    -- ^ See: 'configSkipGHCCheck'
    ,configMonoidSkipMsys            :: !(First Bool)
    -- ^ See: 'configSkipMsys'
    ,configMonoidCompilerCheck       :: !(First VersionCheck)
    -- ^ See: 'configCompilerCheck'
    ,configMonoidRequireStackVersion :: !IntersectingVersionRange
    -- ^ See: 'configRequireStackVersion'
    ,configMonoidOS                  :: !(First String)
    -- ^ Used for overriding the platform
    ,configMonoidArch                :: !(First String)
    -- ^ Used for overriding the platform
    ,configMonoidGHCVariant          :: !(First GHCVariant)
    -- ^ Used for overriding the GHC variant
    ,configMonoidJobs                :: !(First Int)
    -- ^ See: 'configJobs'
    ,configMonoidExtraIncludeDirs    :: !(Set (Path Abs Dir))
    -- ^ See: 'configExtraIncludeDirs'
    ,configMonoidExtraLibDirs        :: !(Set (Path Abs Dir))
    -- ^ See: 'configExtraLibDirs'
    , configMonoidOverrideGccPath    :: !(First (Path Abs File))
    -- ^ Allow users to override the path to gcc
    ,configMonoidConcurrentTests     :: !(First Bool)
    -- ^ See: 'configConcurrentTests'
    ,configMonoidLocalBinPath        :: !(First FilePath)
    -- ^ Used to override the binary installation dir
    ,configMonoidImageOpts           :: !ImageOptsMonoid
    -- ^ Image creation options.
    ,configMonoidTemplateParameters  :: !(Map Text Text)
    -- ^ Template parameters.
    ,configMonoidScmInit             :: !(First SCM)
    -- ^ Initialize SCM (e.g. git init) when making new projects?
    ,configMonoidGhcOptions          :: !GhcOptions
    -- ^ See 'configGhcOptions'
    ,configMonoidExtraPath           :: ![Path Abs Dir]
    -- ^ Additional paths to search for executables in
    ,configMonoidSetupInfoLocations  :: ![SetupInfoLocation]
    -- ^ Additional setup info (inline or remote) to use for installing tools
    ,configMonoidPvpBounds           :: !(First PvpBounds)
    -- ^ See 'configPvpBounds'
    ,configMonoidModifyCodePage      :: !(First Bool)
    -- ^ See 'configModifyCodePage'
    ,configMonoidExplicitSetupDeps   :: !(Map (Maybe PackageName) Bool)
    -- ^ See 'configExplicitSetupDeps'
    ,configMonoidRebuildGhcOptions   :: !(First Bool)
    -- ^ See 'configMonoidRebuildGhcOptions'
    ,configMonoidApplyGhcOptions     :: !(First ApplyGhcOptions)
    -- ^ See 'configApplyGhcOptions'
    ,configMonoidAllowNewer          :: !(First Bool)
    -- ^ See 'configMonoidAllowNewer'
    ,configMonoidDefaultTemplate     :: !(First TemplateName)
    -- ^ The default template to use when none is specified.
    -- (If Nothing, the default default is used.)
    , configMonoidAllowDifferentUser :: !(First Bool)
    -- ^ Allow users other than the stack root owner to use the stack
    -- installation.
    }
  deriving (Show, Generic)

instance Monoid ConfigMonoid where
    mempty = memptydefault
    mappend = mappenddefault

instance FromJSON (WithJSONWarnings ConfigMonoid) where
  parseJSON = withObjectWarnings "ConfigMonoid" parseConfigMonoidJSON

-- | Parse a partial configuration.  Used both to parse both a standalone config
-- file and a project file, so that a sub-parser is not required, which would interfere with
-- warnings for missing fields.
parseConfigMonoidJSON :: Object -> WarningParser ConfigMonoid
parseConfigMonoidJSON obj = do
    -- Parsing 'stackRoot' from 'stackRoot'/config.yaml would be nonsensical
    let configMonoidStackRoot = First Nothing
    configMonoidWorkDir <- First <$> obj ..:? configMonoidWorkDirName
    configMonoidBuildOpts <- jsonSubWarnings (obj ..:? configMonoidBuildOptsName ..!= mempty)
    configMonoidDockerOpts <- jsonSubWarnings (obj ..:? configMonoidDockerOptsName ..!= mempty)
    configMonoidNixOpts <- jsonSubWarnings (obj ..:? configMonoidNixOptsName ..!= mempty)
    configMonoidConnectionCount <- First <$> obj ..:? configMonoidConnectionCountName
    configMonoidHideTHLoading <- First <$> obj ..:? configMonoidHideTHLoadingName
    configMonoidLatestSnapshotUrl <- First <$> obj ..:? configMonoidLatestSnapshotUrlName
    configMonoidUrls <- jsonSubWarnings (obj ..:? configMonoidUrlsName ..!= mempty)
    configMonoidPackageIndices <- First <$> jsonSubWarningsTT (obj ..:?  configMonoidPackageIndicesName)
    configMonoidSystemGHC <- First <$> obj ..:? configMonoidSystemGHCName
    configMonoidInstallGHC <- First <$> obj ..:? configMonoidInstallGHCName
    configMonoidSkipGHCCheck <- First <$> obj ..:? configMonoidSkipGHCCheckName
    configMonoidSkipMsys <- First <$> obj ..:? configMonoidSkipMsysName
    configMonoidRequireStackVersion <- IntersectingVersionRange <$> unVersionRangeJSON <$>
                                       obj ..:? configMonoidRequireStackVersionName
                                           ..!= VersionRangeJSON anyVersion
    configMonoidOS <- First <$> obj ..:? configMonoidOSName
    configMonoidArch <- First <$> obj ..:? configMonoidArchName
    configMonoidGHCVariant <- First <$> obj ..:? configMonoidGHCVariantName
    configMonoidJobs <- First <$> obj ..:? configMonoidJobsName
    configMonoidExtraIncludeDirs <- obj ..:?  configMonoidExtraIncludeDirsName ..!= Set.empty
    configMonoidExtraLibDirs <- obj ..:?  configMonoidExtraLibDirsName ..!= Set.empty
    configMonoidOverrideGccPath <- First <$> obj ..:? configMonoidOverrideGccPathName
    configMonoidConcurrentTests <- First <$> obj ..:? configMonoidConcurrentTestsName
    configMonoidLocalBinPath <- First <$> obj ..:? configMonoidLocalBinPathName
    configMonoidImageOpts <- jsonSubWarnings (obj ..:?  configMonoidImageOptsName ..!= mempty)
    templates <- obj ..:? "templates"
    (configMonoidScmInit,configMonoidTemplateParameters) <-
      case templates of
        Nothing -> return (First Nothing,M.empty)
        Just tobj -> do
          scmInit <- tobj ..:? configMonoidScmInitName
          params <- tobj ..:? configMonoidTemplateParametersName
          return (First scmInit,fromMaybe M.empty params)
    configMonoidCompilerCheck <- First <$> obj ..:? configMonoidCompilerCheckName

    configMonoidGhcOptions <- obj ..:? configMonoidGhcOptionsName ..!= mempty
    configMonoidExtraPath <- obj ..:? configMonoidExtraPathName ..!= []
    configMonoidSetupInfoLocations <-
        maybeToList <$> jsonSubWarningsT (obj ..:?  configMonoidSetupInfoLocationsName)
    configMonoidPvpBounds <- First <$> obj ..:? configMonoidPvpBoundsName
    configMonoidModifyCodePage <- First <$> obj ..:? configMonoidModifyCodePageName
    configMonoidExplicitSetupDeps <-
        (obj ..:? configMonoidExplicitSetupDepsName ..!= mempty)
        >>= fmap Map.fromList . mapM handleExplicitSetupDep . Map.toList
    configMonoidRebuildGhcOptions <- First <$> obj ..:? configMonoidRebuildGhcOptionsName
    configMonoidApplyGhcOptions <- First <$> obj ..:? configMonoidApplyGhcOptionsName
    configMonoidAllowNewer <- First <$> obj ..:? configMonoidAllowNewerName
    configMonoidDefaultTemplate <- First <$> obj ..:? configMonoidDefaultTemplateName
    configMonoidAllowDifferentUser <- First <$> obj ..:? configMonoidAllowDifferentUserName

    return ConfigMonoid {..}
  where
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

configMonoidOverrideGccPathName :: Text
configMonoidOverrideGccPathName = "with-gcc"

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
  | ParseCustomSnapshotException Text ParseException
  | ParseResolverException Text
  | NoProjectConfigFound (Path Abs Dir) (Maybe Text)
  | UnexpectedArchiveContents [Path Abs Dir] [Path Abs File]
  | UnableToExtractArchive Text (Path Abs File)
  | BadStackVersionException VersionRange
  | NoMatchingSnapshot WhichSolverCmd (NonEmpty SnapName)
  | forall l. ResolverMismatch WhichSolverCmd (ResolverThat's l) String
  | ResolverPartial WhichSolverCmd Resolver String
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
        , Yaml.prettyPrintParseException exception
        , "\nSee http://docs.haskellstack.org/en/stable/yaml_configuration/."
        ]
    show (ParseCustomSnapshotException url exception) = concat
        [ "Could not parse '"
        , T.unpack url
        , "':\n"
        , Yaml.prettyPrintParseException exception
        -- FIXME: Link to docs about custom snapshots
        -- , "\nSee http://docs.haskellstack.org/en/stable/yaml_configuration/."
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
    show (NoMatchingSnapshot whichCmd names) = concat
        [ "None of the following snapshots provides a compiler matching "
        , "your package(s):\n"
        , unlines $ map (\name -> "    - " <> T.unpack (renderSnapName name))
                        (NonEmpty.toList names)
        , showOptions whichCmd
        ]
    show (ResolverMismatch whichCmd resolver errDesc) = concat
        [ "Resolver '"
        , T.unpack (resolverName resolver)
        , "' does not have a matching compiler to build some or all of your "
        , "package(s).\n"
        , errDesc
        , showOptions whichCmd
        ]
    show (ResolverPartial whichCmd resolver errDesc) = concat
        [ "Resolver '"
        , T.unpack (resolverName resolver)
        , "' does not have all the packages to match your requirements.\n"
        , unlines $ fmap ("    " <>) (lines errDesc)
        , showOptions whichCmd
        ]
    show (NoSuchDirectory dir) =
        "No directory could be located matching the supplied path: " ++ dir
    show (ParseGHCVariantException v) =
        "Invalid ghc-variant value: " ++ v
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

showOptions :: WhichSolverCmd -> String
showOptions whichCmd = unlines $ "\nThis may be resolved by:" : options
  where
    options =
        case whichCmd of
            IsSolverCmd -> [useResolver]
            IsInitCmd -> both
            IsNewCmd -> both
    both = [omitPackages, useResolver]
    omitPackages = "    - Using '--omit-packages to exclude mismatching package(s)."
    useResolver  = "    - Using '--resolver' to specify a matching snapshot/resolver"

data WhichSolverCmd = IsInitCmd | IsSolverCmd | IsNewCmd

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

-- | Git repo directory for a specific package index, returns 'Nothing' if not
-- a Git repo
configPackageIndexRepo :: (MonadReader env m, HasConfig env, MonadThrow m) => IndexName -> m (Maybe (Path Abs Dir))
configPackageIndexRepo name = do
    indices <- asks $ configPackageIndices . getConfig
    case filter (\p -> indexName p == name) indices of
        [index] -> do
            let murl =
                    case indexLocation index of
                        ILGit x -> Just x
                        ILHttp _ -> Nothing
                        ILGitHttp x _ -> Just x
            case murl of
                Nothing -> return Nothing
                Just url -> do
                    sDir <- configPackageIndexRoot name
                    repoName <- parseRelDir $ takeBaseName $ T.unpack url
                    let suDir =
                          sDir </>
                          $(mkRelDir "git-update")
                    return $ Just $ suDir </> repoName
        _ -> assert False $ return Nothing

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

-- | Hoogle directory.
hoogleRoot :: (MonadThrow m, MonadReader env m, HasEnvConfig env) => m (Path Abs Dir)
hoogleRoot = do
    bc <- asks getBuildConfig
    psc <- useShaPathOnWindows =<< platformSnapAndCompilerRel
    return $ getProjectWorkDir bc </> $(mkRelDir "hoogle") </> psc

-- | Get the hoogle database path.
hoogleDatabasePath :: (MonadThrow m, MonadReader env m, HasEnvConfig env) => m (Path Abs File)
hoogleDatabasePath = do
    dir <- hoogleRoot
    return (dir </> $(mkRelFile "database.hoo"))

-- | Path for platform followed by snapshot name followed by compiler
-- name.
platformSnapAndCompilerRel
    :: (MonadReader env m, HasEnvConfig env, MonadThrow m)
    => m (Path Rel Dir)
platformSnapAndCompilerRel = do
    bc <- asks getBuildConfig
    platform <- platformGhcRelDir
    name <- parseRelDir $ T.unpack $ resolverDirName $ bcResolver bc
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
      -- ^ URL or absolute file path
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

-- TODO: See section of
-- https://github.com/commercialhaskell/stack/issues/1265 about
-- rationalizing the config. It would also be nice to share more code.
-- For now it's more convenient just to extend this type. However, it's
-- unpleasant that it has overlap with both 'Project' and 'Config'.
data CustomSnapshot = CustomSnapshot
    { csCompilerVersion :: !(Maybe CompilerVersion)
    , csPackages :: !(Set PackageIdentifier)
    , csDropPackages :: !(Set PackageName)
    , csFlags :: !PackageFlags
    , csGhcOptions :: !GhcOptions
    }

instance FromJSON (WithJSONWarnings (CustomSnapshot, Maybe Resolver)) where
    parseJSON = withObjectWarnings "CustomSnapshot" $ \o -> (,)
        <$> (CustomSnapshot
            <$> o ..:? "compiler"
            <*> o ..:? "packages" ..!= mempty
            <*> o ..:? "drop-packages" ..!= mempty
            <*> o ..:? "flags" ..!= mempty
            <*> o ..:? configMonoidGhcOptionsName ..!= mempty)
        <*> jsonSubWarningsT (o ..:? "resolver")

newtype GhcOptions = GhcOptions
    { unGhcOptions :: Map (Maybe PackageName) [Text] }
    deriving Show

instance FromJSON GhcOptions where
    parseJSON val = do
        ghcOptions <- parseJSON val
        fmap (GhcOptions . Map.fromList) $ mapM handleGhcOptions $ Map.toList ghcOptions
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

instance Monoid GhcOptions where
    mempty = GhcOptions mempty
    -- FIXME: Should GhcOptions really monoid like this? Keeping it this
    -- way preserves the behavior of the ConfigMonoid. However, this
    -- means there isn't the ability to fully override snapshot
    -- ghc-options in the same way there is for flags. Do we want to
    -- change the semantics here? (particularly for extensible
    -- snapshots)
    mappend (GhcOptions l) (GhcOptions r) =
        GhcOptions (Map.unionWith (++) l r)

ghcOptionsFor :: PackageName -> GhcOptions -> [Text]
ghcOptionsFor name (GhcOptions mp) =
    M.findWithDefault [] Nothing mp ++
    M.findWithDefault [] (Just name) mp

newtype PackageFlags = PackageFlags
    { unPackageFlags :: Map PackageName (Map FlagName Bool) }
    deriving Show

instance FromJSON PackageFlags where
    parseJSON val = PackageFlags <$> parseJSON val

instance ToJSON PackageFlags where
    toJSON = toJSON . unPackageFlags

instance Monoid PackageFlags where
    mempty = PackageFlags mempty
    mappend (PackageFlags l) (PackageFlags r) =
        PackageFlags (Map.unionWith Map.union l r)
