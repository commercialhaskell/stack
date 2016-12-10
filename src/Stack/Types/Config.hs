{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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
  ,PlatformVariant(..)
  -- ** Config & HasConfig
  ,Config(..)
  ,HasConfig(..)
  ,askLatestSnapshotUrl
  ,explicitSetupDeps
  ,getMinimalEnvOverride
  -- ** BuildConfig & HasBuildConfig
  ,BuildConfigNoLocal(..)
  ,BuildConfigLocal(..)
  ,BuildConfig(..)
  ,stackYamlL
  ,projectRootL
  ,HasBuildConfigNoLocal(..)
  ,HasBuildConfig(..)
  -- ** GHCVariant & HasGHCVariant
  ,GHCVariant(..)
  ,ghcVariantName
  ,ghcVariantSuffix
  ,parseGHCVariant
  ,HasGHCVariant(..)
  ,snapshotsDir
  -- ** Constraint synonym for use with StackMini
  ,StackMiniM
  -- ** EnvConfig & HasEnvConfig
  ,EnvConfig(..)
  ,EnvConfigNoLocal(..)
  ,EnvConfigLocal(..)
  ,HasEnvConfigNoLocal(..)
  ,HasEnvConfig(..)
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
  ,configMonoidInstallGHCName
  ,configMonoidSystemGHCName
  -- ** DumpLogs
  ,DumpLogs(..)
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
  -- ** ColorWhen
  ,ColorWhen(..)
  ,readColorWhen
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
  ,workDirL
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
  ,GHCDownloadInfo(..)
  ,SetupInfo(..)
  ,SetupInfoLocation(..)
  -- ** Docker entrypoint
  ,DockerEntrypoint(..)
  ,DockerUser(..)
  ,module X
  -- * Lens helpers
  ,wantedCompilerVersionL
  ,actualCompilerVersionL
  ,buildOptsL
  ,globalOptsL
  ,buildOptsInstallExesL
  ,buildOptsMonoidHaddockL
  ,buildOptsMonoidTestsL
  ,buildOptsMonoidBenchmarksL
  ,buildOptsMonoidInstallExesL
  ,buildOptsHaddockL
  ,globalOptsBuildOptsMonoidL
  ,packageIndicesL
  ,packageCachesL
  ,stackRootL
  ,configUrlsL
  ,cabalVersionL
  ,whichCompilerL
  ,buildConfigL
  -- * Lens reexport
  ,view
  ,to
  ) where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Exception
import           Control.Monad (liftM, mzero, join)
import           Control.Monad.Catch (MonadThrow, MonadMask)
import           Control.Monad.Logger (LogLevel(..), MonadLoggerIO)
import           Control.Monad.Reader (MonadReader, MonadIO, liftIO)
import           Control.Monad.Trans.Control
import           Data.Aeson.Extended
                 (ToJSON, toJSON, FromJSON, parseJSON, withText, object,
                  (.=), (..:), (..:?), (..!=), Value(Bool, String),
                  withObjectWarnings, WarningParser, Object, jsonSubWarnings,
                  jsonSubWarningsT, jsonSubWarningsTT, WithJSONWarnings(..), noJSONWarnings)
import           Data.Attoparsec.Args
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Either (partitionEithers)
import           Data.HashMap.Strict (HashMap)
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
import           Data.Text.Encoding (encodeUtf8)
import           Data.Typeable
import           Data.Yaml (ParseException)
import qualified Data.Yaml as Yaml
import           Distribution.System (Platform)
import qualified Distribution.Text
import           Distribution.Version (anyVersion)
import           GHC.Generics (Generic)
import           Generics.Deriving.Monoid (memptydefault, mappenddefault)
import           Lens.Micro (Lens', lens, _1, _2, to, Getting)
import           Lens.Micro.Mtl (view)
import           Network.HTTP.Client (parseRequest)
import           Options.Applicative (ReadM)
import qualified Options.Applicative as OA
import qualified Options.Applicative.Types as OA
import           Path
import qualified Paths_stack as Meta
import           Stack.Types.BuildPlan (GitSHA1, MiniBuildPlan(..), SnapName, renderSnapName)
import           Stack.Types.Compiler
import           Stack.Types.CompilerBuild
import           Stack.Types.Docker
import           Stack.Types.FlagName
import           Stack.Types.Image
import           Stack.Types.Nix
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageIndex
import           Stack.Types.PackageName
import           Stack.Types.Resolver
import           Stack.Types.TemplateName
import           Stack.Types.Urls
import           Stack.Types.Version
import           System.FilePath (takeBaseName)
import           System.PosixCompat.Types (UserID, GroupID, FileMode)
import           System.Process.Read (EnvOverride, findExecutable)

-- Re-exports
import           Stack.Types.Config.Build as X

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
         ,configGHCBuild            :: !(Maybe CompilerBuild)
         -- ^ Override build of the compiler distribution (e.g. standard, gmp4, tinfo6)
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
         ,configPackageCaches       :: !(IORef (Maybe (Map PackageIdentifier (PackageIndex, PackageCache),
                                                       HashMap GitSHA1 (PackageIndex, OffsetSize))))
         -- ^ In memory cache of hackage index.
         ,configDumpLogs            :: !DumpLogs
         -- ^ Dump logs of local non-dependencies when doing a build.
         ,configMaybeProject        :: !(Maybe (Project, Path Abs File))
         -- ^ 'Just' when a local project can be found, 'Nothing' when stack must
         -- fall back on the implicit global project.
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

-- | Which build log files to dump
data DumpLogs
  = DumpNoLogs -- ^ don't dump any logfiles
  | DumpWarningLogs -- ^ dump logfiles containing warnings
  | DumpAllLogs -- ^ dump all logfiles
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance FromJSON DumpLogs where
  parseJSON (Bool True) = return DumpAllLogs
  parseJSON (Bool False) = return DumpNoLogs
  parseJSON v =
    withText
      "DumpLogs"
      (\t ->
          if | t == "none" -> return DumpNoLogs
             | t == "warning" -> return DumpWarningLogs
             | t == "all" -> return DumpAllLogs
             | otherwise -> fail ("Invalid DumpLogs: " ++ show t))
      v

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
    , globalTimeInLog    :: !Bool -- ^ Whether to include timings in logs.
    , globalConfigMonoid :: !ConfigMonoid -- ^ Config monoid, for passing into 'loadConfig'
    , globalResolver     :: !(Maybe AbstractResolver) -- ^ Resolver override
    , globalCompiler     :: !(Maybe CompilerVersion) -- ^ Compiler override
    , globalTerminal     :: !Bool -- ^ We're in a terminal?
    , globalColorWhen    :: !ColorWhen -- ^ When to use ansi terminal colors
    , globalStackYaml    :: !(Maybe FilePath) -- ^ Override project stack.yaml
    } deriving (Show)

-- | Parsed global command-line options monoid.
data GlobalOptsMonoid = GlobalOptsMonoid
    { globalMonoidReExecVersion :: !(First String) -- ^ Expected re-exec in container version
    , globalMonoidDockerEntrypoint :: !(First DockerEntrypoint)
      -- ^ Data used when stack is acting as a Docker entrypoint (internal use only)
    , globalMonoidLogLevel     :: !(First LogLevel) -- ^ Log level
    , globalMonoidTimeInLog    :: !(First Bool) -- ^ Whether to include timings in logs.
    , globalMonoidConfigMonoid :: !ConfigMonoid -- ^ Config monoid, for passing into 'loadConfig'
    , globalMonoidResolver     :: !(First AbstractResolver) -- ^ Resolver override
    , globalMonoidCompiler     :: !(First CompilerVersion) -- ^ Compiler override
    , globalMonoidTerminal     :: !(First Bool) -- ^ We're in a terminal?
    , globalMonoidColorWhen    :: !(First ColorWhen) -- ^ When to use ansi colors
    , globalMonoidStackYaml    :: !(First FilePath) -- ^ Override project stack.yaml
    } deriving (Show, Generic)

instance Monoid GlobalOptsMonoid where
    mempty = memptydefault
    mappend = mappenddefault

-- | Default logging level should be something useful but not crazy.
defaultLogLevel :: LogLevel
defaultLogLevel = LevelInfo

data ColorWhen = ColorNever | ColorAlways | ColorAuto
    deriving (Show, Generic)

readColorWhen :: ReadM ColorWhen
readColorWhen = do
    s <- OA.readerAsk
    case s of
        "never" -> return ColorNever
        "always" -> return ColorAlways
        "auto" -> return ColorAuto
        _ -> OA.readerError "Expected values of color option are 'never', 'always', or 'auto'."

-- | A superset of 'Config' adding information on how to build code. The reason
-- for this breakdown is because we will need some of the information from
-- 'Config' in order to determine the values here.
--
-- These are the components which know nothing about local configuration.
data BuildConfigNoLocal = BuildConfigNoLocal
    { bcConfig     :: !Config
    , bcResolver   :: !LoadedResolver
      -- ^ How we resolve which dependencies to install given a set of
      -- packages.
    , bcWantedMiniBuildPlan :: !MiniBuildPlan
      -- ^ Build plan wanted for this build
    , bcGHCVariant :: !GHCVariant
      -- ^ The variant of GHC used to select a GHC bindist.
    }

-- | The local parts of 'BuildConfigNoLocal'
data BuildConfigLocal = BuildConfigLocal
    { bcPackageEntries :: ![PackageEntry]
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
      -- different from projectRootL </> "stack.yaml"
      --
      -- FIXME MSS 2016-12-08: is the above comment still true? projectRootL
      -- is defined in terms of bcStackYaml
    , bcFlags      :: !PackageFlags
      -- ^ Per-package flag overrides
    , bcImplicitGlobal :: !Bool
      -- ^ Are we loading from the implicit global stack.yaml? This is useful
      -- for providing better error messages.
    }

data BuildConfig = BuildConfig
    { bcNoLocal :: !BuildConfigNoLocal
    , bcLocal   :: !BuildConfigLocal
    }

stackYamlL :: HasBuildConfig env => Lens' env (Path Abs File)
stackYamlL = buildConfigLocalL.lens bcStackYaml (\x y -> x { bcStackYaml = y })

-- | Directory containing the project's stack.yaml file
projectRootL :: HasBuildConfig env => Getting r env (Path Abs Dir)
projectRootL = stackYamlL.to parent

-- | Configuration after the environment has been setup.
data EnvConfigNoLocal = EnvConfigNoLocal
    {envConfigBuildConfigNoLocal :: !BuildConfigNoLocal
    ,envConfigCabalVersion :: !Version
    -- ^ This is the version of Cabal that stack will use to compile Setup.hs files
    -- in the build process.
    --
    -- Note that this is not necessarily the same version as the one that stack
    -- depends on as a library and which is displayed when running
    -- @stack list-dependencies | grep Cabal@ in the stack project.
    ,envConfigCompilerVersion :: !CompilerVersion
    -- ^ The actual version of the compiler to be used, as opposed to
    -- 'wantedCompilerL', which provides the version specified by the
    -- build plan.
    ,envConfigCompilerBuild :: !CompilerBuild
    }

data EnvConfigLocal = EnvConfigLocal
    {envConfigBuildConfigLocal :: !BuildConfigLocal
    ,envConfigPackagesRef :: !(IORef (Maybe (Map (Path Abs Dir) TreatLikeExtraDep)))
    -- ^ Cache for 'getLocalPackages'.
    }

data EnvConfig = EnvConfig
    { ecNoLocal :: !EnvConfigNoLocal
    , ecLocal :: !EnvConfigLocal
    }

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
    { peExtraDepMaybe :: !(Maybe TreatLikeExtraDep)
    , peLocation :: !PackageLocation
    , peSubdirs :: ![FilePath]
    }
    deriving Show

-- | Perform defaulting of peExtraDepMaybe
peExtraDepDef :: PackageEntry -> TreatLikeExtraDep
peExtraDepDef = fromMaybe False . peExtraDepMaybe

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
    toJSON pe | not (peExtraDepDef pe) && null (peSubdirs pe) =
        toJSON $ peLocation pe
    toJSON pe = object $
        maybe id (\e -> (("extra-dep" .= e):)) (peExtraDepMaybe pe)
        [ "location" .= peLocation pe
        , "subdirs" .= peSubdirs pe
        ]
instance FromJSON (WithJSONWarnings PackageEntry) where
    parseJSON (String t) = do
        WithJSONWarnings loc _ <- parseJSON $ String t
        return $ noJSONWarnings
            PackageEntry
                { peExtraDepMaybe = Nothing
                , peLocation = loc
                , peSubdirs = []
                }
    parseJSON v = withObjectWarnings "PackageEntry" (\o -> PackageEntry
        <$> o ..:? "extra-dep"
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

-- | Constraint synonym for constraints satisfied by a 'MiniConfig'
-- environment.
type StackMiniM r m =
    ( MonadReader r m, MonadIO m, MonadBaseControl IO m, MonadLoggerIO m, MonadMask m
    )

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
    ,configMonoidArch                :: !(First String)
    -- ^ Used for overriding the platform
    ,configMonoidGHCVariant          :: !(First GHCVariant)
    -- ^ Used for overriding the platform
    ,configMonoidGHCBuild            :: !(First CompilerBuild)
    -- ^ Used for overriding the GHC build
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
    ,configMonoidLocalProgramsBase   :: !(First (Path Abs Dir))
    -- ^ Override the default local programs dir, where e.g. GHC is installed.
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
    , configMonoidDumpLogs           :: !(First DumpLogs)
    -- ^ See 'configDumpLogs'
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
    configMonoidRequireStackVersion <- IntersectingVersionRange . unVersionRangeJSON <$> (
                                       obj ..:? configMonoidRequireStackVersionName
                                           ..!= VersionRangeJSON anyVersion)
    configMonoidArch <- First <$> obj ..:? configMonoidArchName
    configMonoidGHCVariant <- First <$> obj ..:? configMonoidGHCVariantName
    configMonoidGHCBuild <- First <$> obj ..:? configMonoidGHCBuildName
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
    configMonoidLocalProgramsBase <- First <$> obj ..:? configMonoidLocalProgramsBaseName
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
    configMonoidDumpLogs <- First <$> obj ..:? configMonoidDumpLogsName

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

configMonoidArchName :: Text
configMonoidArchName = "arch"

configMonoidGHCVariantName :: Text
configMonoidGHCVariantName = "ghc-variant"

configMonoidGHCBuildName :: Text
configMonoidGHCBuildName = "ghc-build"

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

configMonoidLocalProgramsBaseName :: Text
configMonoidLocalProgramsBaseName = "local-programs-path"

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

configMonoidDumpLogsName :: Text
configMonoidDumpLogsName = "dump-logs"

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
  | FailedToCloneRepo String
  | ManualGHCVariantSettingsAreIncompatibleWithSystemGHC
  | NixRequiresSystemGhc
  deriving Typeable
instance Show ConfigException where
    show (ParseConfigFileException configFile exception) = concat
        [ "Could not parse '"
        , toFilePath configFile
        , "':\n"
        , Yaml.prettyPrintParseException exception
        , "\nSee http://docs.haskellstack.org/en/stable/yaml_configuration/"
        ]
    show (ParseCustomSnapshotException url exception) = concat
        [ "Could not parse '"
        , T.unpack url
        , "':\n"
        , Yaml.prettyPrintParseException exception
        -- FIXME: Link to docs about custom snapshots
        -- , "\nSee http://docs.haskellstack.org/en/stable/yaml_configuration/"
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
        , showOptions whichCmd Don'tSuggestSolver
        ]
    show (ResolverMismatch whichCmd resolver errDesc) = concat
        [ "Resolver '"
        , T.unpack (resolverName resolver)
        , "' does not have a matching compiler to build some or all of your "
        , "package(s).\n"
        , errDesc
        , showOptions whichCmd Don'tSuggestSolver
        ]
    show (ResolverPartial whichCmd resolver errDesc) = concat
        [ "Resolver '"
        , T.unpack (resolverName resolver)
        , "' does not have all the packages to match your requirements.\n"
        , unlines $ fmap ("    " <>) (lines errDesc)
        , showOptions whichCmd
            (case whichCmd of
                IsSolverCmd -> Don'tSuggestSolver
                _ -> SuggestSolver)
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
    show (FailedToCloneRepo commandName) = concat
        [ "Failed to use "
        , commandName
        , " to clone the repo.  Please ensure that "
        , commandName
        , " is installed and available to stack on your PATH environment variable."
        ]
    show ManualGHCVariantSettingsAreIncompatibleWithSystemGHC = T.unpack $ T.concat
        [ "stack can only control the "
        , configMonoidGHCVariantName
        , " of its own GHC installations. Please use '--no-"
        , configMonoidSystemGHCName
        , "'."
        ]
    show NixRequiresSystemGhc = T.unpack $ T.concat
        [ "stack's Nix integration is incompatible with '--no-system-ghc'. "
        , "Please use '--"
        , configMonoidSystemGHCName
        , "' or disable the Nix integration."
        ]
instance Exception ConfigException

showOptions :: WhichSolverCmd -> SuggestSolver -> String
showOptions whichCmd suggestSolver = unlines $ "\nThis may be resolved by:" : options
  where
    options =
        (case suggestSolver of
            SuggestSolver -> [useSolver]
            Don'tSuggestSolver -> []) ++
        (case whichCmd of
            IsSolverCmd -> [useResolver]
            IsInitCmd -> both
            IsNewCmd -> both)
    both = [omitPackages, useResolver]
    useSolver    = "    - Using '--solver' to ask cabal-install to generate extra-deps, atop the chosen snapshot."
    omitPackages = "    - Using '--omit-packages to exclude mismatching package(s)."
    useResolver  = "    - Using '--resolver' to specify a matching snapshot/resolver"

data WhichSolverCmd = IsInitCmd | IsSolverCmd | IsNewCmd

data SuggestSolver = SuggestSolver | Don'tSuggestSolver

-- | Get the URL to request the information on the latest snapshots
askLatestSnapshotUrl :: (MonadReader env m, HasConfig env) => m Text
askLatestSnapshotUrl = view $ configL.to configUrls.to urlsLatestSnapshot

-- | Root for a specific package index
configPackageIndexRoot :: (MonadReader env m, HasConfig env, MonadThrow m) => IndexName -> m (Path Abs Dir)
configPackageIndexRoot (IndexName name) = do
    root <- view stackRootL
    dir <- parseRelDir $ S8.unpack name
    return (root </> $(mkRelDir "indices") </> dir)

-- | Git repo directory for a specific package index, returns 'Nothing' if not
-- a Git repo
configPackageIndexRepo :: (MonadReader env m, HasConfig env, MonadThrow m) => IndexName -> m (Maybe (Path Abs Dir))
configPackageIndexRepo name = do
    indices <- view packageIndicesL
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

-- | Location of the 01-index.cache file
configPackageIndexCache :: (MonadReader env m, HasConfig env, MonadThrow m) => IndexName -> m (Path Abs File)
configPackageIndexCache = liftM (</> $(mkRelFile "01-index.cache")) . configPackageIndexRoot

-- | Location of the 01-index.tar file
configPackageIndex :: (MonadReader env m, HasConfig env, MonadThrow m) => IndexName -> m (Path Abs File)
configPackageIndex = liftM (</> $(mkRelFile "01-index.tar")) . configPackageIndexRoot

-- | Location of the 01-index.tar.gz file
configPackageIndexGz :: (MonadReader env m, HasConfig env, MonadThrow m) => IndexName -> m (Path Abs File)
configPackageIndexGz = liftM (</> $(mkRelFile "01-index.tar.gz")) . configPackageIndexRoot

-- | Location of a package tarball
configPackageTarball :: (MonadReader env m, HasConfig env, MonadThrow m) => IndexName -> PackageIdentifier -> m (Path Abs File)
configPackageTarball iname ident = do
    root <- configPackageIndexRoot iname
    name <- parseRelDir $ packageNameString $ packageIdentifierName ident
    ver <- parseRelDir $ versionString $ packageIdentifierVersion ident
    base <- parseRelFile $ packageIdentifierString ident ++ ".tar.gz"
    return (root </> $(mkRelDir "packages") </> name </> ver </> base)

-- | @".stack-work"@
workDirL :: HasConfig env => Lens' env (Path Rel Dir)
workDirL = configL.lens configWorkDir (\x y -> x { configWorkDir = y })

-- | Per-project work dir
getProjectWorkDir :: (HasBuildConfig env, MonadReader env m) => m (Path Abs Dir)
getProjectWorkDir = do
    root    <- view projectRootL
    workDir <- view workDirL
    return (root </> workDir)

-- | File containing the installed cache, see "Stack.PackageDump"
configInstalledCache :: (HasBuildConfig env, MonadReader env m) => m (Path Abs File)
configInstalledCache = liftM (</> $(mkRelFile "installed-cache.bin")) getProjectWorkDir

-- | Relative directory for the platform identifier
platformOnlyRelDir
    :: (MonadReader env m, HasPlatform env, MonadThrow m)
    => m (Path Rel Dir)
platformOnlyRelDir = do
    platform <- view platformL
    platformVariant <- view platformVariantL
    parseRelDir (Distribution.Text.display platform ++ platformVariantSuffix platformVariant)

-- | Directory containing snapshots
snapshotsDir :: (MonadReader env m, HasEnvConfig env, MonadThrow m) => m (Path Abs Dir)
snapshotsDir = do
    root <- view stackRootL
    platform <- platformGhcRelDir
    return $ root </> $(mkRelDir "snapshots") </> platform

-- | Installation root for dependencies
installationRootDeps :: (MonadThrow m, MonadReader env m, HasEnvConfig env) => m (Path Abs Dir)
installationRootDeps = do
    root <- view stackRootL
    -- TODO: also useShaPathOnWindows here, once #1173 is resolved.
    psc <- platformSnapAndCompilerRel
    return $ root </> $(mkRelDir "snapshots") </> psc

-- | Installation root for locals
installationRootLocal :: (MonadThrow m, MonadReader env m, HasEnvConfig env) => m (Path Abs Dir)
installationRootLocal = do
    workDir <- getProjectWorkDir
    psc <- useShaPathOnWindows =<< platformSnapAndCompilerRel
    return $ workDir </> $(mkRelDir "install") </> psc

-- | Hoogle directory.
hoogleRoot :: (MonadThrow m, MonadReader env m, HasEnvConfig env) => m (Path Abs Dir)
hoogleRoot = do
    workDir <- getProjectWorkDir
    psc <- useShaPathOnWindows =<< platformSnapAndCompilerRel
    return $ workDir </> $(mkRelDir "hoogle") </> psc

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
    resolver' <- view loadedResolverL
    platform <- platformGhcRelDir
    name <- parseRelDir $ T.unpack $ resolverDirName resolver'
    ghc <- compilerVersionDir
    useShaPathOnWindows (platform </> name </> ghc)

-- | Relative directory for the platform and GHC identifier
platformGhcRelDir
    :: (MonadReader env m, HasEnvConfig env, MonadThrow m)
    => m (Path Rel Dir)
platformGhcRelDir = do
    ec <- view envConfigNoLocalL
    verOnly <- platformGhcVerOnlyRelDirStr
    parseRelDir (mconcat [ verOnly
                         , compilerBuildSuffix (envConfigCompilerBuild ec)])

-- | Relative directory for the platform and GHC identifier without GHC bindist build
platformGhcVerOnlyRelDir
    :: (MonadReader env m, HasPlatform env, HasGHCVariant env, MonadThrow m)
    => m (Path Rel Dir)
platformGhcVerOnlyRelDir =
    parseRelDir =<< platformGhcVerOnlyRelDirStr

-- | Relative directory for the platform and GHC identifier without GHC bindist build
-- (before parsing into a Path)
platformGhcVerOnlyRelDirStr
    :: (MonadReader env m, HasPlatform env, HasGHCVariant env)
    => m FilePath
platformGhcVerOnlyRelDirStr = do
    platform <- view platformL
    platformVariant <- view platformVariantL
    ghcVariant <- view ghcVariantL
    return $ mconcat [ Distribution.Text.display platform
                     , platformVariantSuffix platformVariant
                     , ghcVariantSuffix ghcVariant ]

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
    compilerVersion <- view actualCompilerVersionL
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
    bc <- view buildConfigLocalL
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
    root <- view stackRootL
    platform <- platformGhcVerOnlyRelDir
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
    config' <- view configL
    liftIO $ configEnvOverride config' minimalEnvSettings

minimalEnvSettings :: EnvSettings
minimalEnvSettings =
    EnvSettings
    { esIncludeLocals = False
    , esIncludeGhcPackagePath = False
    , esStackExe = False
    , esLocaleUtf8 = False
    }

-- | Get the path for the given compiler ignoring any local binaries.
--
-- https://github.com/commercialhaskell/stack/issues/1052
getCompilerPath
    :: (MonadIO m, MonadThrow m, MonadReader env m, HasConfig env)
    => WhichCompiler
    -> m (Path Abs File)
getCompilerPath wc = do
    config' <- view configL
    eoWithoutLocals <- liftIO $
        configEnvOverride config' minimalEnvSettings { esLocaleUtf8 = True }
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
    { peExtraDepMaybe = Nothing
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
          | s == "integersimple" -> return GHCIntegerSimple
          | otherwise -> return (GHCCustom s)

-- | Build of the compiler distribution (e.g. standard, gmp4, tinfo6)
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

data GHCDownloadInfo = GHCDownloadInfo
    { gdiConfigureOpts :: [Text]
    , gdiConfigureEnv :: Map Text Text
    , gdiDownloadInfo :: DownloadInfo
    }
    deriving Show

instance FromJSON (WithJSONWarnings GHCDownloadInfo) where
    parseJSON = withObjectWarnings "GHCDownloadInfo" $ \o -> do
        configureOpts <- o ..:? "configure-opts" ..!= mempty
        configureEnv <- o ..:? "configure-env" ..!= mempty
        downloadInfo <- parseDownloadInfoFromObject o
        return GHCDownloadInfo
            { gdiConfigureOpts = configureOpts
            , gdiConfigureEnv = configureEnv
            , gdiDownloadInfo = downloadInfo
            }

data SetupInfo = SetupInfo
    { siSevenzExe :: Maybe DownloadInfo
    , siSevenzDll :: Maybe DownloadInfo
    , siMsys2 :: Map Text VersionedDownloadInfo
    , siGHCs :: Map Text (Map Version GHCDownloadInfo)
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
    m <- view $ configL.to configExplicitSetupDeps
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

-----------------------------------
-- Lens classes
-----------------------------------

-- | Class for environment values which have a Platform
class HasPlatform env where
    platformL :: Lens' env Platform
    default platformL :: HasConfig env => Lens' env Platform
    platformL = configL.platformL
    {-# INLINE platformL #-}
    platformVariantL :: Lens' env PlatformVariant
    default platformVariantL :: HasConfig env => Lens' env PlatformVariant
    platformVariantL = configL.platformVariantL
    {-# INLINE platformVariantL #-}

-- | Class for environment values which have a GHCVariant
class HasGHCVariant env where
    ghcVariantL :: Lens' env GHCVariant
    default ghcVariantL :: HasBuildConfigNoLocal env => Lens' env GHCVariant
    ghcVariantL = buildConfigNoLocalL.ghcVariantL
    {-# INLINE ghcVariantL #-}

-- | Class for environment values that can provide a 'Config'.
class HasPlatform env => HasConfig env where
    configL :: Lens' env Config
    default configL :: HasBuildConfigNoLocal env => Lens' env Config
    configL = buildConfigNoLocalL.lens bcConfig (\x y -> x { bcConfig = y })
    {-# INLINE configL #-}

class HasConfig env => HasBuildConfigNoLocal env where
    buildConfigNoLocalL :: Lens' env BuildConfigNoLocal
    default buildConfigNoLocalL :: HasEnvConfigNoLocal env => Lens' env BuildConfigNoLocal
    buildConfigNoLocalL = envConfigNoLocalL.lens
        envConfigBuildConfigNoLocal
        (\x y -> x { envConfigBuildConfigNoLocal = y })

-- | Class for environment values that can provide a 'BuildConfig'.
class HasBuildConfigNoLocal env => HasBuildConfig env where
    buildConfigLocalL :: Lens' env BuildConfigLocal
    default buildConfigLocalL :: HasEnvConfig env => Lens' env BuildConfigLocal
    buildConfigLocalL = envConfigLocalL.lens
        envConfigBuildConfigLocal
        (\x y -> x { envConfigBuildConfigLocal = y })

class (HasBuildConfigNoLocal env, HasGHCVariant env) => HasEnvConfigNoLocal env where
    envConfigNoLocalL :: Lens' env EnvConfigNoLocal

class (HasBuildConfig env, HasEnvConfigNoLocal env) => HasEnvConfig env where
    envConfigLocalL :: Lens' env EnvConfigLocal
    envConfigLocalL = envConfigL.lens ecLocal (\x y -> x { ecLocal = y })
    {-# INLINE envConfigLocalL #-}
    envConfigL :: Lens' env EnvConfig

-----------------------------------
-- Lens instances
-----------------------------------

instance HasPlatform (Platform,PlatformVariant) where
    platformL = _1
    platformVariantL = _2
instance HasPlatform Config where
    platformL = lens configPlatform (\x y -> x { configPlatform = y })
    platformVariantL = lens configPlatformVariant (\x y -> x { configPlatformVariant = y })
instance HasPlatform BuildConfigNoLocal
instance HasPlatform BuildConfig
instance HasPlatform EnvConfigNoLocal
instance HasPlatform EnvConfig

instance HasGHCVariant GHCVariant where
    ghcVariantL = id
    {-# INLINE ghcVariantL #-}
instance HasGHCVariant BuildConfigNoLocal where
    ghcVariantL = lens bcGHCVariant (\x y -> x { bcGHCVariant = y })
instance HasGHCVariant BuildConfig
instance HasGHCVariant EnvConfigNoLocal
instance HasGHCVariant EnvConfig

instance HasConfig Config where
    configL = id
    {-# INLINE configL #-}
instance HasConfig BuildConfigNoLocal where
    configL = lens bcConfig (\x y -> x { bcConfig = y })
instance HasConfig BuildConfig
instance HasConfig EnvConfigNoLocal
instance HasConfig EnvConfig

instance HasBuildConfigNoLocal BuildConfigNoLocal where
    buildConfigNoLocalL = id
    {-# INLINE buildConfigNoLocalL #-}
instance HasBuildConfigNoLocal BuildConfig where
    buildConfigNoLocalL = lens
        bcNoLocal
        (\x y -> x { bcNoLocal = y })
instance HasBuildConfigNoLocal EnvConfigNoLocal
instance HasBuildConfigNoLocal EnvConfig

instance HasBuildConfig BuildConfig where
    buildConfigLocalL = lens bcLocal (\x y -> x { bcLocal = y})
    {-# INLINE buildConfigLocalL #-}
instance HasBuildConfig EnvConfig

instance HasEnvConfigNoLocal EnvConfigNoLocal where
    envConfigNoLocalL = id
    {-# INLINE envConfigNoLocalL #-}
instance HasEnvConfigNoLocal EnvConfig where
    envConfigNoLocalL = lens ecNoLocal (\x y -> x { ecNoLocal = y })
    {-# INLINE envConfigNoLocalL #-}

instance HasEnvConfig EnvConfig where
    envConfigL = id
    {-# INLINE envConfigL #-}

-----------------------------------
-- Helper lenses
-----------------------------------

stackRootL :: HasConfig s => Lens' s (Path Abs Dir)
stackRootL = configL.lens configStackRoot (\x y -> x { configStackRoot = y })

-- | The compiler specified by the @MiniBuildPlan@. This may be
-- different from the actual compiler used!
wantedCompilerVersionL :: HasBuildConfigNoLocal s => Lens' s CompilerVersion
wantedCompilerVersionL = miniBuildPlanL.lens
    mbpCompilerVersion
    (\x y -> x { mbpCompilerVersion = y })

-- | The version of the compiler which will actually be used. May be
-- different than that specified in the 'MiniBuildPlan' and returned
-- by 'wantedCompilerVersionL'.
actualCompilerVersionL :: HasEnvConfigNoLocal s => Lens' s CompilerVersion
actualCompilerVersionL = envConfigNoLocalL.lens
    envConfigCompilerVersion
    (\x y -> x { envConfigCompilerVersion = y })

loadedResolverL :: HasBuildConfigNoLocal s => Lens' s LoadedResolver
loadedResolverL = buildConfigNoLocalL.lens
    bcResolver
    (\x y -> x { bcResolver = y })

miniBuildPlanL :: HasBuildConfigNoLocal s => Lens' s MiniBuildPlan
miniBuildPlanL = buildConfigNoLocalL.lens
    bcWantedMiniBuildPlan
    (\x y -> x { bcWantedMiniBuildPlan = y })

packageIndicesL :: HasConfig s => Lens' s [PackageIndex]
packageIndicesL = configL.lens
    configPackageIndices
    (\x y -> x { configPackageIndices = y })

buildOptsL :: HasConfig s => Lens' s BuildOpts
buildOptsL = configL.lens
    configBuild
    (\x y -> x { configBuild = y })

buildOptsMonoidHaddockL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidHaddockL = lens (getFirst . buildMonoidHaddock)
                            (\buildMonoid t -> buildMonoid {buildMonoidHaddock = First t})

buildOptsMonoidTestsL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidTestsL = lens (getFirst . buildMonoidTests)
                            (\buildMonoid t -> buildMonoid {buildMonoidTests = First t})

buildOptsMonoidBenchmarksL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidBenchmarksL = lens (getFirst . buildMonoidBenchmarks)
                            (\buildMonoid t -> buildMonoid {buildMonoidBenchmarks = First t})

buildOptsMonoidInstallExesL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidInstallExesL =
  lens (getFirst . buildMonoidInstallExes)
       (\buildMonoid t -> buildMonoid {buildMonoidInstallExes = First t})

buildOptsInstallExesL :: Lens' BuildOpts Bool
buildOptsInstallExesL =
  lens boptsInstallExes
       (\bopts t -> bopts {boptsInstallExes = t})

buildOptsHaddockL :: Lens' BuildOpts Bool
buildOptsHaddockL =
  lens boptsHaddock
       (\bopts t -> bopts {boptsHaddock = t})

globalOptsL :: Lens' GlobalOpts ConfigMonoid
globalOptsL = lens globalConfigMonoid (\x y -> x { globalConfigMonoid = y })

globalOptsBuildOptsMonoidL :: Lens' GlobalOpts BuildOptsMonoid
globalOptsBuildOptsMonoidL = globalOptsL.lens
    configMonoidBuildOpts
    (\x y -> x { configMonoidBuildOpts = y })

packageCachesL :: HasConfig env => Lens' env
    (IORef (Maybe (Map PackageIdentifier (PackageIndex, PackageCache)
                  ,HashMap GitSHA1 (PackageIndex, OffsetSize))))
packageCachesL = configL.lens configPackageCaches (\x y -> x { configPackageCaches = y })

configUrlsL :: HasConfig env => Lens' env Urls
configUrlsL = configL.lens configUrls (\x y -> x { configUrls = y })

cabalVersionL :: HasEnvConfigNoLocal env => Lens' env Version
cabalVersionL = envConfigNoLocalL.lens
    envConfigCabalVersion
    (\x y -> x { envConfigCabalVersion = y })

whichCompilerL :: Getting r CompilerVersion WhichCompiler
whichCompilerL = to whichCompiler

buildConfigL :: HasBuildConfig env => Getting r env BuildConfig
buildConfigL = to $ \env -> BuildConfig
    (view buildConfigNoLocalL env)
    (view buildConfigLocalL env)
