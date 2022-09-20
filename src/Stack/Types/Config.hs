{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

-- | The Config type.

module Stack.Types.Config
  (
  -- * Main configuration types and classes
  -- ** HasPlatform & HasStackRoot
   HasPlatform(..)
  ,PlatformVariant(..)
  -- ** Runner
  ,HasRunner(..)
  ,Runner(..)
  ,ColorWhen(..)
  ,terminalL
  ,reExecL
  -- ** Config & HasConfig
  ,Config(..)
  ,HasConfig(..)
  ,askLatestSnapshotUrl
  ,configProjectRoot
  -- ** BuildConfig & HasBuildConfig
  ,BuildConfig(..)
  ,ProjectPackage(..)
  ,DepPackage(..)
  ,ppRoot
  ,ppVersion
  ,ppComponents
  ,ppGPD
  ,stackYamlL
  ,projectRootL
  ,HasBuildConfig(..)
  -- ** Storage databases
  ,UserStorage(..)
  ,ProjectStorage(..)
  -- ** GHCVariant & HasGHCVariant
  ,GHCVariant(..)
  ,ghcVariantName
  ,ghcVariantSuffix
  ,parseGHCVariant
  ,HasGHCVariant(..)
  ,snapshotsDir
  -- ** EnvConfig & HasEnvConfig
  ,EnvConfig(..)
  ,HasSourceMap(..)
  ,HasEnvConfig(..)
  ,getCompilerPath
  -- * Details
  -- ** ApplyGhcOptions
  ,ApplyGhcOptions(..)
  -- ** CabalConfigKey
  ,CabalConfigKey(..)
  -- ** ConfigException
  ,HpackExecutable(..)
  ,ConfigException(..)
  -- ** ConfigMonoid
  ,ConfigMonoid(..)
  ,configMonoidInstallGHCName
  ,configMonoidSystemGHCName
  ,parseConfigMonoid
  -- ** DumpLogs
  ,DumpLogs(..)
  -- ** EnvSettings
  ,EnvSettings(..)
  ,minimalEnvSettings
  ,defaultEnvSettings
  ,plainEnvSettings
  -- ** GlobalOpts & GlobalOptsMonoid
  ,GlobalOpts(..)
  ,GlobalOptsMonoid(..)
  ,rslInLogL
  ,StackYamlLoc(..)
  ,stackYamlLocL
  ,LockFileBehavior(..)
  ,readLockFileBehavior
  ,lockFileBehaviorL
  ,defaultLogLevel
  -- ** Project & ProjectAndConfigMonoid
  ,Project(..)
  ,ProjectConfig(..)
  ,Curator(..)
  ,ProjectAndConfigMonoid(..)
  ,parseProjectAndConfigMonoid
  -- ** PvpBounds
  ,PvpBounds(..)
  ,PvpBoundsType(..)
  ,parsePvpBounds
  -- ** ColorWhen
  ,readColorWhen
  -- ** Styles
  ,readStyles
  -- ** SCM
  ,SCM(..)
  -- * Paths
  ,bindirSuffix
  ,GlobalInfoSource(..)
  ,getProjectWorkDir
  ,docDirSuffix
  ,extraBinDirs
  ,hpcReportDir
  ,installationRootDeps
  ,installationRootLocal
  ,bindirCompilerTools
  ,hoogleRoot
  ,hoogleDatabasePath
  ,packageDatabaseDeps
  ,packageDatabaseExtra
  ,packageDatabaseLocal
  ,platformOnlyRelDir
  ,platformGhcRelDir
  ,platformGhcVerOnlyRelDir
  ,useShaPathOnWindows
  ,shaPath
  ,shaPathForBytes
  ,workDirL
  ,ghcInstallHook
  -- * Command-related types
  ,AddCommand
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
  -- ** Docker entrypoint
  ,DockerEntrypoint(..)
  ,DockerUser(..)
  ,module X
  -- * Lens helpers
  ,wantedCompilerVersionL
  ,actualCompilerVersionL
  ,HasCompiler(..)
  ,DumpPackage(..)
  ,CompilerPaths(..)
  ,GhcPkgExe(..)
  ,getGhcPkgExe
  ,cpWhich
  ,ExtraDirs(..)
  ,buildOptsL
  ,globalOptsL
  ,buildOptsInstallExesL
  ,buildOptsMonoidHaddockL
  ,buildOptsMonoidTestsL
  ,buildOptsMonoidBenchmarksL
  ,buildOptsMonoidInstallExesL
  ,buildOptsHaddockL
  ,globalOptsBuildOptsMonoidL
  ,stackRootL
  ,cabalVersionL
  ,whichCompilerL
  ,envOverrideSettingsL
  ,shouldForceGhcColorFlag
  ,appropriateGhcColorFlag
  -- * Helper logging functions
  ,prettyStackDevL
  -- * Lens reexport
  ,view
  ,to
  ) where

import           Control.Monad.Writer (Writer, tell)
import           Control.Monad.Trans.Except (ExceptT)
import           Crypto.Hash (hashWith, SHA1(..))
import           Stack.Prelude
import           Pantry.Internal.AesonExtended
                 (ToJSON, toJSON, FromJSON, FromJSONKey (..), parseJSON, withText, object,
                  (.=), (..:), (...:), (..:?), (..!=), Value(Bool),
                  withObjectWarnings, WarningParser, Object, jsonSubWarnings,
                  jsonSubWarningsT, jsonSubWarningsTT, WithJSONWarnings(..),
                  FromJSONKeyFunction (FromJSONKeyTextParser))
import           Data.Attoparsec.Args (parseArgs, EscapingMode (Escaping))
import qualified Data.ByteArray.Encoding as Mem (convertToBase, Base(Base16))
import qualified Data.ByteString.Char8 as S8
import           Data.Coerce (coerce)
import           Data.List (stripPrefix)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import qualified Data.Monoid as Monoid
import           Data.Monoid.Map (MonoidMap(..))
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Yaml (ParseException)
import qualified Data.Yaml as Yaml
import qualified Distribution.License as C
import           Distribution.ModuleName (ModuleName)
import           Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.PackageDescription as C
import           Distribution.System (Platform, Arch)
import qualified Distribution.Text
import           Distribution.Version (anyVersion, mkVersion', mkVersion)
import           Generics.Deriving.Monoid (memptydefault, mappenddefault)
import           Lens.Micro
import           Options.Applicative (ReadM)
import qualified Options.Applicative as OA
import qualified Options.Applicative.Types as OA
import           Pantry.Internal (Storage)
import           Path
import qualified Paths_stack as Meta
import qualified RIO.List as List
import           RIO.PrettyPrint (HasTerm (..), StyleDoc, prettyWarnL, prettyDebugL)
import           RIO.PrettyPrint.StylesUpdate (StylesUpdate,
                     parseStylesUpdateFromString, HasStylesUpdate (..))
import           Stack.Constants
import           Stack.Types.Compiler
import           Stack.Types.CompilerBuild
import           Stack.Types.Docker
import           Stack.Types.GhcPkgId
import           Stack.Types.NamedComponent
import           Stack.Types.Nix
import           Stack.Types.Resolver
import           Stack.Types.SourceMap
import           Stack.Types.TemplateName
import           Stack.Types.Version
import qualified System.FilePath as FilePath
import           System.PosixCompat.Types (UserID, GroupID, FileMode)
import           RIO.Process (ProcessContext, HasProcessContext (..))
import           Casa.Client (CasaRepoPrefix)

-- Re-exports
import           Stack.Types.Config.Build as X

-- | The base environment that almost everything in Stack runs in,
-- based off of parsing command line options in 'GlobalOpts'. Provides
-- logging and process execution.
data Runner = Runner
  { runnerGlobalOpts :: !GlobalOpts
  , runnerUseColor   :: !Bool
  , runnerLogFunc    :: !LogFunc
  , runnerTermWidth  :: !Int
  , runnerProcessContext :: !ProcessContext
  }

data ColorWhen = ColorNever | ColorAlways | ColorAuto
    deriving (Eq, Show, Generic)

instance FromJSON ColorWhen where
    parseJSON v = do
        s <- parseJSON v
        case s of
            "never"  -> pure ColorNever
            "always" -> pure ColorAlways
            "auto"   -> pure ColorAuto
            _ -> fail ("Unknown color use: " <> s <> ". Expected values of " <>
                       "option are 'never', 'always', or 'auto'.")

-- | The top-level Stackage configuration.
data Config =
  Config {configWorkDir             :: !(Path Rel Dir)
         -- ^ this allows to override .stack-work directory
         ,configUserConfigPath      :: !(Path Abs File)
         -- ^ Path to user configuration file (usually ~/.stack/config.yaml)
         ,configBuild               :: !BuildOpts
         -- ^ Build configuration
         ,configDocker              :: !DockerOpts
         -- ^ Docker configuration
         ,configNix                 :: !NixOpts
         -- ^ Execution environment (e.g nix-shell) configuration
         ,configProcessContextSettings :: !(EnvSettings -> IO ProcessContext)
         -- ^ Environment variables to be passed to external tools
         ,configLocalProgramsBase   :: !(Path Abs Dir)
         -- ^ Non-platform-specific path containing local installations
         ,configLocalPrograms       :: !(Path Abs Dir)
         -- ^ Path containing local installations (mainly GHC)
         ,configHideTHLoading       :: !Bool
         -- ^ Hide the Template Haskell "Loading package ..." messages from the
         -- console
         ,configPrefixTimestamps    :: !Bool
         -- ^ Prefix build output with timestamps for each line.
         ,configPlatform            :: !Platform
         -- ^ The platform we're building for, used in many directory names
         ,configPlatformVariant     :: !PlatformVariant
         -- ^ Variant of the platform, also used in directory names
         ,configGHCVariant          :: !(Maybe GHCVariant)
         -- ^ The variant of GHC requested by the user.
         ,configGHCBuild            :: !(Maybe CompilerBuild)
         -- ^ Override build of the compiler distribution (e.g. standard, gmp4, tinfo6)
         ,configLatestSnapshot      :: !Text
         -- ^ URL of a JSON file providing the latest LTS and Nightly snapshots.
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
         ,configCompilerRepository  :: !CompilerRepository
         -- ^ Specifies the repository containing the compiler sources
         ,configLocalBin            :: !(Path Abs Dir)
         -- ^ Directory we should install executables into
         ,configRequireStackVersion :: !VersionRange
         -- ^ Require a version of stack within this range.
         ,configJobs                :: !Int
         -- ^ How many concurrent jobs to run, defaults to number of capabilities
         ,configOverrideGccPath     :: !(Maybe (Path Abs File))
         -- ^ Optional gcc override path
         ,configExtraIncludeDirs    :: ![FilePath]
         -- ^ --extra-include-dirs arguments
         ,configExtraLibDirs        :: ![FilePath]
         -- ^ --extra-lib-dirs arguments
         ,configCustomPreprocessorExts :: ![Text]
         -- ^ List of custom preprocessors to complete the hard coded ones
         ,configConcurrentTests     :: !Bool
         -- ^ Run test suites concurrently
         ,configTemplateParams      :: !(Map Text Text)
         -- ^ Parameters for templates.
         ,configScmInit             :: !(Maybe SCM)
         -- ^ Initialize SCM (e.g. git) when creating new projects.
         ,configGhcOptionsByName    :: !(Map PackageName [Text])
         -- ^ Additional GHC options to apply to specific packages.
         ,configGhcOptionsByCat     :: !(Map ApplyGhcOptions [Text])
         -- ^ Additional GHC options to apply to categories of packages
         ,configCabalConfigOpts     :: !(Map CabalConfigKey [Text])
         -- ^ Additional options to be passed to ./Setup.hs configure
         ,configSetupInfoLocations  :: ![String]
         -- ^ URLs or paths to stack-setup.yaml files, for finding tools.
         -- If none present, the default setup-info is used.
         ,configSetupInfoInline     :: !SetupInfo
         -- ^ Additional SetupInfo to use to find tools.
         ,configPvpBounds           :: !PvpBounds
         -- ^ How PVP upper bounds should be added to packages
         ,configModifyCodePage      :: !Bool
         -- ^ Force the code page to UTF-8 on Windows
         ,configRebuildGhcOptions   :: !Bool
         -- ^ Rebuild on GHC options changes
         ,configApplyGhcOptions     :: !ApplyGhcOptions
         -- ^ Which packages to ghc-options on the command line apply to?
         ,configAllowNewer          :: !Bool
         -- ^ Ignore version ranges in .cabal files. Funny naming chosen to
         -- match cabal.
         ,configDefaultTemplate     :: !(Maybe TemplateName)
         -- ^ The default template to use when none is specified.
         -- (If Nothing, the 'default' default template is used.)
         ,configAllowDifferentUser  :: !Bool
         -- ^ Allow users other than the stack root owner to use the stack
         -- installation.
         ,configDumpLogs            :: !DumpLogs
         -- ^ Dump logs of local non-dependencies when doing a build.
         ,configProject             :: !(ProjectConfig (Project, Path Abs File))
         -- ^ Project information and stack.yaml file location
         ,configAllowLocals         :: !Bool
         -- ^ Are we allowed to build local packages? The script
         -- command disallows this.
         ,configSaveHackageCreds    :: !Bool
         -- ^ Should we save Hackage credentials to a file?
         ,configHackageBaseUrl      :: !Text
         -- ^ Hackage base URL used when uploading packages
         ,configRunner              :: !Runner
         ,configPantryConfig        :: !PantryConfig
         ,configStackRoot           :: !(Path Abs Dir)
         ,configResolver            :: !(Maybe AbstractResolver)
         -- ^ Any resolver override from the command line
         ,configUserStorage         :: !UserStorage
         -- ^ Database connection pool for user Stack database
         ,configHideSourcePaths     :: !Bool
         -- ^ Enable GHC hiding source paths?
         ,configRecommendUpgrade    :: !Bool
         -- ^ Recommend a Stack upgrade?
         ,configNoRunCompile   :: !Bool
         -- ^ Use --no-run and --compile options when using `stack script`
         ,configStackDeveloperMode  :: !Bool
         -- ^ Turn on Stack developer mode for additional messages?
         }

-- | A bit of type safety to ensure we're talking to the right database.
newtype UserStorage = UserStorage
  { unUserStorage :: Storage
  }

-- | A bit of type safety to ensure we're talking to the right database.
newtype ProjectStorage = ProjectStorage
  { unProjectStorage :: Storage
  }

-- | The project root directory, if in a project.
configProjectRoot :: Config -> Maybe (Path Abs Dir)
configProjectRoot c =
  case configProject c of
    PCProject (_, fp) -> Just $ parent fp
    PCGlobalProject -> Nothing
    PCNoProject _deps -> Nothing

-- | Which packages do configure opts apply to?
data CabalConfigKey
  = CCKTargets -- ^ See AGOTargets
  | CCKLocals -- ^ See AGOLocals
  | CCKEverything -- ^ See AGOEverything
  | CCKPackage !PackageName -- ^ A specific package
  deriving (Show, Read, Eq, Ord)
instance FromJSON CabalConfigKey where
  parseJSON = withText "CabalConfigKey" parseCabalConfigKey
instance FromJSONKey CabalConfigKey where
  fromJSONKey = FromJSONKeyTextParser parseCabalConfigKey

parseCabalConfigKey :: (Monad m, MonadFail m) => Text -> m CabalConfigKey
parseCabalConfigKey "$targets" = pure CCKTargets
parseCabalConfigKey "$locals" = pure CCKLocals
parseCabalConfigKey "$everything" = pure CCKEverything
parseCabalConfigKey name =
  case parsePackageName $ T.unpack name of
    Nothing -> fail $ "Invalid CabalConfigKey: " ++ show name
    Just x -> pure $ CCKPackage x

-- | Which packages do ghc-options on the command line apply to?
data ApplyGhcOptions = AGOTargets -- ^ all local targets
                     | AGOLocals -- ^ all local packages, even non-targets
                     | AGOEverything -- ^ every package
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance FromJSON ApplyGhcOptions where
    parseJSON = withText "ApplyGhcOptions" $ \t ->
        case t of
            "targets" -> pure AGOTargets
            "locals" -> pure AGOLocals
            "everything" -> pure AGOEverything
            _ -> fail $ "Invalid ApplyGhcOptions: " ++ show t

-- | Which build log files to dump
data DumpLogs
  = DumpNoLogs -- ^ don't dump any logfiles
  | DumpWarningLogs -- ^ dump logfiles containing warnings
  | DumpAllLogs -- ^ dump all logfiles
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance FromJSON DumpLogs where
  parseJSON (Bool True) = pure DumpAllLogs
  parseJSON (Bool False) = pure DumpNoLogs
  parseJSON v =
    withText
      "DumpLogs"
      (\t ->
          if | t == "none" -> pure DumpNoLogs
             | t == "warning" -> pure DumpWarningLogs
             | t == "all" -> pure DumpAllLogs
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
    , esKeepGhcRts :: !Bool
    -- ^ if True, keep GHCRTS variable in environment
    }
    deriving (Show, Eq, Ord)

type AddCommand =
  ExceptT (RIO Runner ())
          (Writer (OA.Mod OA.CommandFields (RIO Runner (), GlobalOptsMonoid)))
          ()

data ExecOpts = ExecOpts
    { eoCmd :: !SpecialExecCmd
    , eoArgs :: ![String]
    , eoExtra :: !ExecOptsExtra
    } deriving (Show)

data SpecialExecCmd
    = ExecCmd String
    | ExecRun
    | ExecGhc
    | ExecRunGhc
    deriving (Show, Eq)

data ExecOptsExtra = ExecOptsExtra
  { eoEnvSettings :: !EnvSettings
  , eoPackages :: ![String]
  , eoRtsOptions :: ![String]
  , eoCwd :: !(Maybe FilePath)
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
    , globalRSLInLog     :: !Bool -- ^ Whether to include raw snapshot layer (RSL) in logs.
    , globalConfigMonoid :: !ConfigMonoid -- ^ Config monoid, for passing into 'loadConfig'
    , globalResolver     :: !(Maybe AbstractResolver) -- ^ Resolver override
    , globalCompiler     :: !(Maybe WantedCompiler) -- ^ Compiler override
    , globalTerminal     :: !Bool -- ^ We're in a terminal?
    , globalStylesUpdate :: !StylesUpdate -- ^ SGR (Ansi) codes for styles
    , globalTermWidth    :: !(Maybe Int) -- ^ Terminal width override
    , globalStackYaml    :: !StackYamlLoc -- ^ Override project stack.yaml
    , globalLockFileBehavior :: !LockFileBehavior
    } deriving (Show)

rslInLogL :: HasRunner env => SimpleGetter env Bool
rslInLogL = globalOptsL.to globalRSLInLog

-- | Location for the project's stack.yaml file.
data StackYamlLoc
    = SYLDefault
    -- ^ Use the standard parent-directory-checking logic
    | SYLOverride !(Path Abs File)
    -- ^ Use a specific stack.yaml file provided
    | SYLNoProject ![PackageIdentifierRevision]
    -- ^ Do not load up a project, just user configuration. Include
    -- the given extra dependencies with the resolver.
    | SYLGlobalProject
    -- ^ Do not look for a project configuration, and use the implicit global.
    deriving Show

stackYamlLocL :: HasRunner env => Lens' env StackYamlLoc
stackYamlLocL = globalOptsL.lens globalStackYaml (\x y -> x { globalStackYaml = y })

-- | How to interact with lock files
data LockFileBehavior
  = LFBReadWrite
  -- ^ Read and write lock files
  | LFBReadOnly
  -- ^ Read lock files, but do not write them
  | LFBIgnore
  -- ^ Entirely ignore lock files
  | LFBErrorOnWrite
  -- ^ Error out on trying to write a lock file. This can be used to
  -- ensure that lock files in a repository already ensure
  -- reproducible builds.
  deriving (Show, Enum, Bounded)

lockFileBehaviorL :: HasRunner env => SimpleGetter env LockFileBehavior
lockFileBehaviorL = globalOptsL.to globalLockFileBehavior

-- | Parser for 'LockFileBehavior'
readLockFileBehavior :: ReadM LockFileBehavior
readLockFileBehavior = do
  s <- OA.readerAsk
  case Map.lookup s m of
    Just x -> pure x
    Nothing -> OA.readerError $ "Invalid lock file behavior, valid options: " ++
                                List.intercalate ", " (Map.keys m)
  where
    m = Map.fromList $ map (\x -> (render x, x)) [minBound..maxBound]
    render LFBReadWrite = "read-write"
    render LFBReadOnly = "read-only"
    render LFBIgnore = "ignore"
    render LFBErrorOnWrite = "error-on-write"

-- | Project configuration information. Not every run of Stack has a
-- true local project; see constructors below.
data ProjectConfig a
    = PCProject a
    -- ^ Normal run: we want a project, and have one. This comes from
    -- either 'SYLDefault' or 'SYLOverride'.
    | PCGlobalProject
    -- ^ No project was found when using 'SYLDefault'. Instead, use
    -- the implicit global.
    | PCNoProject ![PackageIdentifierRevision]
    -- ^ Use a no project run. This comes from 'SYLNoProject'.

-- | Parsed global command-line options monoid.
data GlobalOptsMonoid = GlobalOptsMonoid
    { globalMonoidReExecVersion :: !(First String) -- ^ Expected re-exec in container version
    , globalMonoidDockerEntrypoint :: !(First DockerEntrypoint)
      -- ^ Data used when stack is acting as a Docker entrypoint (internal use only)
    , globalMonoidLogLevel     :: !(First LogLevel) -- ^ Log level
    , globalMonoidTimeInLog    :: !FirstTrue -- ^ Whether to include timings in logs.
    , globalMonoidRSLInLog     :: !FirstFalse -- ^ Whether to include raw snapshot layer (RSL) in logs.
    , globalMonoidConfigMonoid :: !ConfigMonoid -- ^ Config monoid, for passing into 'loadConfig'
    , globalMonoidResolver     :: !(First (Unresolved AbstractResolver)) -- ^ Resolver override
    , globalMonoidResolverRoot :: !(First FilePath) -- ^ root directory for resolver relative path
    , globalMonoidCompiler     :: !(First WantedCompiler) -- ^ Compiler override
    , globalMonoidTerminal     :: !(First Bool) -- ^ We're in a terminal?
    , globalMonoidStyles       :: !StylesUpdate -- ^ Stack's output styles
    , globalMonoidTermWidth    :: !(First Int) -- ^ Terminal width override
    , globalMonoidStackYaml    :: !(First FilePath) -- ^ Override project stack.yaml
    , globalMonoidLockFileBehavior :: !(First LockFileBehavior) -- ^ See 'globalLockFileBehavior'
    } deriving Generic

instance Semigroup GlobalOptsMonoid where
    (<>) = mappenddefault

instance Monoid GlobalOptsMonoid where
    mempty = memptydefault
    mappend = (<>)

-- | Default logging level should be something useful but not crazy.
defaultLogLevel :: LogLevel
defaultLogLevel = LevelInfo

readColorWhen :: ReadM ColorWhen
readColorWhen = do
    s <- OA.readerAsk
    case s of
        "never" -> pure ColorNever
        "always" -> pure ColorAlways
        "auto" -> pure ColorAuto
        _ -> OA.readerError "Expected values of color option are 'never', 'always', or 'auto'."

readStyles :: ReadM StylesUpdate
readStyles = parseStylesUpdateFromString <$> OA.readerAsk

-- | A superset of 'Config' adding information on how to build code. The reason
-- for this breakdown is because we will need some of the information from
-- 'Config' in order to determine the values here.
--
-- These are the components which know nothing about local configuration.
data BuildConfig = BuildConfig
    { bcConfig     :: !Config
    , bcSMWanted :: !SMWanted
    , bcExtraPackageDBs :: ![Path Abs Dir]
      -- ^ Extra package databases
    , bcStackYaml  :: !(Path Abs File)
      -- ^ Location of the stack.yaml file.
      --
      -- Note: if the STACK_YAML environment variable is used, this may be
      -- different from projectRootL </> "stack.yaml" if a different file
      -- name is used.
    , bcProjectStorage :: !ProjectStorage
    -- ^ Database connection pool for project Stack database
    , bcCurator :: !(Maybe Curator)
    }

stackYamlL :: HasBuildConfig env => Lens' env (Path Abs File)
stackYamlL = buildConfigL.lens bcStackYaml (\x y -> x { bcStackYaml = y })

-- | Directory containing the project's stack.yaml file
projectRootL :: HasBuildConfig env => Getting r env (Path Abs Dir)
projectRootL = stackYamlL.to parent

-- | Configuration after the environment has been setup.
data EnvConfig = EnvConfig
    {envConfigBuildConfig :: !BuildConfig
    ,envConfigBuildOptsCLI :: !BuildOptsCLI
    ,envConfigSourceMap :: !SourceMap
    ,envConfigSourceMapHash :: !SourceMapHash
    ,envConfigCompilerPaths :: !CompilerPaths
    }

ppGPD :: MonadIO m => ProjectPackage -> m GenericPackageDescription
ppGPD = liftIO . cpGPD . ppCommon

-- | Root directory for the given 'ProjectPackage'
ppRoot :: ProjectPackage -> Path Abs Dir
ppRoot = parent . ppCabalFP

-- | All components available in the given 'ProjectPackage'
ppComponents :: MonadIO m => ProjectPackage -> m (Set NamedComponent)
ppComponents pp = do
  gpd <- ppGPD pp
  pure $ Set.fromList $ concat
    [ maybe []  (const [CLib]) (C.condLibrary gpd)
    , go CExe   (fst <$> C.condExecutables gpd)
    , go CTest  (fst <$> C.condTestSuites gpd)
    , go CBench (fst <$> C.condBenchmarks gpd)
    ]
  where
    go :: (T.Text -> NamedComponent)
       -> [C.UnqualComponentName]
       -> [NamedComponent]
    go wrapper = map (wrapper . T.pack . C.unUnqualComponentName)

-- | Version for the given 'ProjectPackage
ppVersion :: MonadIO m => ProjectPackage -> m Version
ppVersion = fmap gpdVersion . ppGPD

-- | A project is a collection of packages. We can have multiple stack.yaml
-- files, but only one of them may contain project information.
data Project = Project
    { projectUserMsg :: !(Maybe String)
    -- ^ A warning message to display to the user when the auto generated
    -- config may have issues.
    , projectPackages :: ![RelFilePath]
    -- ^ Packages which are actually part of the project (as opposed
    -- to dependencies).
    , projectDependencies :: ![RawPackageLocation]
    -- ^ Dependencies defined within the stack.yaml file, to be
    -- applied on top of the snapshot.
    , projectFlags :: !(Map PackageName (Map FlagName Bool))
    -- ^ Flags to be applied on top of the snapshot flags.
    , projectResolver :: !RawSnapshotLocation
    -- ^ How we resolve which @Snapshot@ to use
    , projectCompiler :: !(Maybe WantedCompiler)
    -- ^ Override the compiler in 'projectResolver'
    , projectExtraPackageDBs :: ![FilePath]
    , projectCurator :: !(Maybe Curator)
    -- ^ Extra configuration intended exclusively for usage by the
    -- curator tool. In other words, this is /not/ part of the
    -- documented and exposed Stack API. SUBJECT TO CHANGE.
    , projectDropPackages :: !(Set PackageName)
    -- ^ Packages to drop from the 'projectResolver'.
    }
  deriving Show

instance ToJSON Project where
    -- Expanding the constructor fully to ensure we don't miss any fields.
    toJSON (Project userMsg packages extraDeps flags resolver mcompiler extraPackageDBs mcurator drops) = object $ concat
      [ maybe [] (\cv -> ["compiler" .= cv]) mcompiler
      , maybe [] (\msg -> ["user-message" .= msg]) userMsg
      , if null extraPackageDBs then [] else ["extra-package-dbs" .= extraPackageDBs]
      , if null extraDeps then [] else ["extra-deps" .= extraDeps]
      , if Map.null flags then [] else ["flags" .= fmap toCabalStringMap (toCabalStringMap flags)]
      , ["packages" .= packages]
      , ["resolver" .= resolver]
      , maybe [] (\c -> ["curator" .= c]) mcurator
      , if Set.null drops then [] else ["drop-packages" .= Set.map CabalString drops]
      ]

-- | Extra configuration intended exclusively for usage by the
-- curator tool. In other words, this is /not/ part of the
-- documented and exposed Stack API. SUBJECT TO CHANGE.
data Curator = Curator
  { curatorSkipTest :: !(Set PackageName)
  , curatorExpectTestFailure :: !(Set PackageName)
  , curatorSkipBenchmark :: !(Set PackageName)
  , curatorExpectBenchmarkFailure :: !(Set PackageName)
  , curatorSkipHaddock :: !(Set PackageName)
  , curatorExpectHaddockFailure :: !(Set PackageName)
  }
  deriving Show
instance ToJSON Curator where
  toJSON c = object
    [ "skip-test" .= Set.map CabalString (curatorSkipTest c)
    , "expect-test-failure" .= Set.map CabalString (curatorExpectTestFailure c)
    , "skip-bench" .= Set.map CabalString (curatorSkipBenchmark c)
    , "expect-benchmark-failure" .= Set.map CabalString (curatorExpectTestFailure c)
    , "skip-haddock" .= Set.map CabalString (curatorSkipHaddock c)
    , "expect-test-failure" .= Set.map CabalString (curatorExpectHaddockFailure c)
    ]
instance FromJSON (WithJSONWarnings Curator) where
  parseJSON = withObjectWarnings "Curator" $ \o -> Curator
    <$> fmap (Set.map unCabalString) (o ..:? "skip-test" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "expect-test-failure" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "skip-bench" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "expect-benchmark-failure" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "skip-haddock" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "expect-haddock-failure" ..!= mempty)

-- An uninterpreted representation of configuration options.
-- Configurations may be "cascaded" using mappend (left-biased).
data ConfigMonoid =
  ConfigMonoid
    { configMonoidStackRoot          :: !(First (Path Abs Dir))
    -- ^ See: 'clStackRoot'
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
    , configMonoidHideTHLoading      :: !FirstTrue
    -- ^ See: 'configHideTHLoading'
    , configMonoidPrefixTimestamps   :: !(First Bool)
    -- ^ See: 'configPrefixTimestamps'
    , configMonoidLatestSnapshot     :: !(First Text)
    -- ^ See: 'configLatestSnapshot'
    , configMonoidPackageIndices     :: !(First [HackageSecurityConfig])
    -- ^ See: @picIndices@
    , configMonoidSystemGHC          :: !(First Bool)
    -- ^ See: 'configSystemGHC'
    ,configMonoidInstallGHC          :: !FirstTrue
    -- ^ See: 'configInstallGHC'
    ,configMonoidSkipGHCCheck        :: !FirstFalse
    -- ^ See: 'configSkipGHCCheck'
    ,configMonoidSkipMsys            :: !FirstFalse
    -- ^ See: 'configSkipMsys'
    ,configMonoidCompilerCheck       :: !(First VersionCheck)
    -- ^ See: 'configCompilerCheck'
    ,configMonoidCompilerRepository  :: !(First CompilerRepository)
    -- ^ See: 'configCompilerRepository'
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
    ,configMonoidExtraIncludeDirs    :: ![FilePath]
    -- ^ See: 'configExtraIncludeDirs'
    ,configMonoidExtraLibDirs        :: ![FilePath]
    -- ^ See: 'configExtraLibDirs'
    ,configMonoidCustomPreprocessorExts :: ![Text]
    -- ^ See: 'configCustomPreprocessorExts'
    , configMonoidOverrideGccPath    :: !(First (Path Abs File))
    -- ^ Allow users to override the path to gcc
    ,configMonoidOverrideHpack       :: !(First FilePath)
    -- ^ Use Hpack executable (overrides bundled Hpack)
    ,configMonoidConcurrentTests     :: !(First Bool)
    -- ^ See: 'configConcurrentTests'
    ,configMonoidLocalBinPath        :: !(First FilePath)
    -- ^ Used to override the binary installation dir
    ,configMonoidTemplateParameters  :: !(Map Text Text)
    -- ^ Template parameters.
    ,configMonoidScmInit             :: !(First SCM)
    -- ^ Initialize SCM (e.g. git init) when making new projects?
    ,configMonoidGhcOptionsByName    :: !(MonoidMap PackageName (Monoid.Dual [Text]))
    -- ^ See 'configGhcOptionsByName'. Uses 'Monoid.Dual' so that
    -- options from the configs on the right come first, so that they
    -- can be overridden.
    ,configMonoidGhcOptionsByCat     :: !(MonoidMap ApplyGhcOptions (Monoid.Dual [Text]))
    -- ^ See 'configGhcOptionsAll'. Uses 'Monoid.Dual' so that options
    -- from the configs on the right come first, so that they can be
    -- overridden.
    ,configMonoidCabalConfigOpts     :: !(MonoidMap CabalConfigKey (Monoid.Dual [Text]))
    -- ^ See 'configCabalConfigOpts'.
    ,configMonoidExtraPath           :: ![Path Abs Dir]
    -- ^ Additional paths to search for executables in
    ,configMonoidSetupInfoLocations  :: ![String]
    -- ^ See 'configSetupInfoLocations'
    ,configMonoidSetupInfoInline     :: !SetupInfo
    -- ^ See 'configSetupInfoInline'
    ,configMonoidLocalProgramsBase   :: !(First (Path Abs Dir))
    -- ^ Override the default local programs dir, where e.g. GHC is installed.
    ,configMonoidPvpBounds           :: !(First PvpBounds)
    -- ^ See 'configPvpBounds'
    ,configMonoidModifyCodePage      :: !FirstTrue
    -- ^ See 'configModifyCodePage'
    ,configMonoidRebuildGhcOptions   :: !FirstFalse
    -- ^ See 'configMonoidRebuildGhcOptions'
    ,configMonoidApplyGhcOptions     :: !(First ApplyGhcOptions)
    -- ^ See 'configApplyGhcOptions'
    ,configMonoidAllowNewer          :: !(First Bool)
    -- ^ See 'configMonoidAllowNewer'
    ,configMonoidDefaultTemplate     :: !(First TemplateName)
    -- ^ The default template to use when none is specified.
    -- (If Nothing, the 'default' default template is used.)
    , configMonoidAllowDifferentUser :: !(First Bool)
    -- ^ Allow users other than the stack root owner to use the stack
    -- installation.
    , configMonoidDumpLogs           :: !(First DumpLogs)
    -- ^ See 'configDumpLogs'
    , configMonoidSaveHackageCreds   :: !(First Bool)
    -- ^ See 'configSaveHackageCreds'
    , configMonoidHackageBaseUrl     :: !(First Text)
    -- ^ See 'configHackageBaseUrl'
    , configMonoidColorWhen          :: !(First ColorWhen)
    -- ^ When to use 'ANSI' colors
    , configMonoidStyles             :: !StylesUpdate
    , configMonoidHideSourcePaths    :: !FirstTrue
    -- ^ See 'configHideSourcePaths'
    , configMonoidRecommendUpgrade   :: !FirstTrue
    -- ^ See 'configRecommendUpgrade'
    , configMonoidCasaRepoPrefix     :: !(First CasaRepoPrefix)
    , configMonoidSnapshotLocation :: !(First Text)
    -- ^ Custom location of LTS/Nightly snapshots
    , configMonoidNoRunCompile  :: !FirstFalse
    -- ^ See: 'configNoRunCompile'
    , configMonoidStackDeveloperMode :: !(First Bool)
    -- ^ See 'configStackDeveloperMode'
    }
  deriving (Show, Generic)

instance Semigroup ConfigMonoid where
    (<>) = mappenddefault

instance Monoid ConfigMonoid where
    mempty = memptydefault
    mappend = (<>)

parseConfigMonoid :: Path Abs Dir -> Value -> Yaml.Parser (WithJSONWarnings ConfigMonoid)
parseConfigMonoid = withObjectWarnings "ConfigMonoid" . parseConfigMonoidObject

-- | Parse a partial configuration.  Used both to parse both a standalone config
-- file and a project file, so that a sub-parser is not required, which would interfere with
-- warnings for missing fields.
parseConfigMonoidObject :: Path Abs Dir -> Object -> WarningParser ConfigMonoid
parseConfigMonoidObject rootDir obj = do
    -- Parsing 'stackRoot' from 'stackRoot'/config.yaml would be nonsensical
    let configMonoidStackRoot = First Nothing
    configMonoidWorkDir <- First <$> obj ..:? configMonoidWorkDirName
    configMonoidBuildOpts <- jsonSubWarnings (obj ..:? configMonoidBuildOptsName ..!= mempty)
    configMonoidDockerOpts <- jsonSubWarnings (obj ..:? configMonoidDockerOptsName ..!= mempty)
    configMonoidNixOpts <- jsonSubWarnings (obj ..:? configMonoidNixOptsName ..!= mempty)
    configMonoidConnectionCount <- First <$> obj ..:? configMonoidConnectionCountName
    configMonoidHideTHLoading <- FirstTrue <$> obj ..:? configMonoidHideTHLoadingName
    configMonoidPrefixTimestamps <- First <$> obj ..:? configMonoidPrefixTimestampsName

    murls :: Maybe Value <- obj ..:? configMonoidUrlsName
    configMonoidLatestSnapshot <-
      case murls of
        Nothing -> pure $ First Nothing
        Just urls -> jsonSubWarnings $ lift $ withObjectWarnings
          "urls"
          (\o -> First <$> o ..:? "latest-snapshot" :: WarningParser (First Text))
          urls

    configMonoidPackageIndices <- First <$> jsonSubWarningsTT (obj ..:?  configMonoidPackageIndicesName)
    configMonoidSystemGHC <- First <$> obj ..:? configMonoidSystemGHCName
    configMonoidInstallGHC <- FirstTrue <$> obj ..:? configMonoidInstallGHCName
    configMonoidSkipGHCCheck <- FirstFalse <$> obj ..:? configMonoidSkipGHCCheckName
    configMonoidSkipMsys <- FirstFalse <$> obj ..:? configMonoidSkipMsysName
    configMonoidRequireStackVersion <- IntersectingVersionRange . unVersionRangeJSON <$> (
                                       obj ..:? configMonoidRequireStackVersionName
                                           ..!= VersionRangeJSON anyVersion)
    configMonoidArch <- First <$> obj ..:? configMonoidArchName
    configMonoidGHCVariant <- First <$> obj ..:? configMonoidGHCVariantName
    configMonoidGHCBuild <- First <$> obj ..:? configMonoidGHCBuildName
    configMonoidJobs <- First <$> obj ..:? configMonoidJobsName
    configMonoidExtraIncludeDirs <- map (toFilePath rootDir FilePath.</>) <$>
        obj ..:?  configMonoidExtraIncludeDirsName ..!= []
    configMonoidExtraLibDirs <- map (toFilePath rootDir FilePath.</>) <$>
        obj ..:?  configMonoidExtraLibDirsName ..!= []
    configMonoidCustomPreprocessorExts <- obj ..:?  configMonoidCustomPreprocessorExtsName ..!= []
    configMonoidOverrideGccPath <- First <$> obj ..:? configMonoidOverrideGccPathName
    configMonoidOverrideHpack <- First <$> obj ..:? configMonoidOverrideHpackName
    configMonoidConcurrentTests <- First <$> obj ..:? configMonoidConcurrentTestsName
    configMonoidLocalBinPath <- First <$> obj ..:? configMonoidLocalBinPathName
    templates <- obj ..:? "templates"
    (configMonoidScmInit,configMonoidTemplateParameters) <-
      case templates of
        Nothing -> pure (First Nothing,M.empty)
        Just tobj -> do
          scmInit <- tobj ..:? configMonoidScmInitName
          params <- tobj ..:? configMonoidTemplateParametersName
          pure (First scmInit,fromMaybe M.empty params)
    configMonoidCompilerCheck <- First <$> obj ..:? configMonoidCompilerCheckName
    configMonoidCompilerRepository <- First <$> (obj ..:? configMonoidCompilerRepositoryName)

    options <- Map.map unGhcOptions <$> obj ..:? configMonoidGhcOptionsName ..!= mempty

    optionsEverything <-
      case (Map.lookup GOKOldEverything options, Map.lookup GOKEverything options) of
        (Just _, Just _) -> fail "Cannot specify both `*` and `$everything` GHC options"
        (Nothing, Just x) -> pure x
        (Just x, Nothing) -> do
          tell "The `*` ghc-options key is not recommended. Consider using $locals, or if really needed, $everything"
          pure x
        (Nothing, Nothing) -> pure []

    let configMonoidGhcOptionsByCat = coerce $ Map.fromList
          [ (AGOEverything, optionsEverything)
          , (AGOLocals, Map.findWithDefault [] GOKLocals options)
          , (AGOTargets, Map.findWithDefault [] GOKTargets options)
          ]

        configMonoidGhcOptionsByName = coerce $ Map.fromList
            [(name, opts) | (GOKPackage name, opts) <- Map.toList options]

    configMonoidCabalConfigOpts' <- obj ..:? "configure-options" ..!= mempty
    let configMonoidCabalConfigOpts = coerce (configMonoidCabalConfigOpts' :: Map CabalConfigKey [Text])

    configMonoidExtraPath <- obj ..:? configMonoidExtraPathName ..!= []
    configMonoidSetupInfoLocations <- obj ..:? configMonoidSetupInfoLocationsName ..!= []
    configMonoidSetupInfoInline <- jsonSubWarningsT (obj ..:? configMonoidSetupInfoInlineName) ..!= mempty
    configMonoidLocalProgramsBase <- First <$> obj ..:? configMonoidLocalProgramsBaseName
    configMonoidPvpBounds <- First <$> obj ..:? configMonoidPvpBoundsName
    configMonoidModifyCodePage <- FirstTrue <$> obj ..:? configMonoidModifyCodePageName
    configMonoidRebuildGhcOptions <- FirstFalse <$> obj ..:? configMonoidRebuildGhcOptionsName
    configMonoidApplyGhcOptions <- First <$> obj ..:? configMonoidApplyGhcOptionsName
    configMonoidAllowNewer <- First <$> obj ..:? configMonoidAllowNewerName
    configMonoidDefaultTemplate <- First <$> obj ..:? configMonoidDefaultTemplateName
    configMonoidAllowDifferentUser <- First <$> obj ..:? configMonoidAllowDifferentUserName
    configMonoidDumpLogs <- First <$> obj ..:? configMonoidDumpLogsName
    configMonoidSaveHackageCreds <- First <$> obj ..:? configMonoidSaveHackageCredsName
    configMonoidHackageBaseUrl <- First <$> obj ..:? configMonoidHackageBaseUrlName

    configMonoidColorWhenUS <- obj ..:? configMonoidColorWhenUSName
    configMonoidColorWhenGB <- obj ..:? configMonoidColorWhenGBName
    let configMonoidColorWhen =  First $   configMonoidColorWhenUS
                                       <|> configMonoidColorWhenGB

    configMonoidStylesUS <- obj ..:? configMonoidStylesUSName
    configMonoidStylesGB <- obj ..:? configMonoidStylesGBName
    let configMonoidStyles = fromMaybe mempty $   configMonoidStylesUS
                                              <|> configMonoidStylesGB

    configMonoidHideSourcePaths <- FirstTrue <$> obj ..:? configMonoidHideSourcePathsName
    configMonoidRecommendUpgrade <- FirstTrue <$> obj ..:? configMonoidRecommendUpgradeName

    configMonoidCasaRepoPrefix <- First <$> obj ..:? configMonoidCasaRepoPrefixName
    configMonoidSnapshotLocation <- First <$> obj ..:? configMonoidSnapshotLocationName
    configMonoidNoRunCompile <- FirstFalse <$> obj ..:? configMonoidNoRunCompileName

    configMonoidStackDeveloperMode <- First <$> obj ..:? configMonoidStackDeveloperModeName

    pure ConfigMonoid {..}

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

configMonoidPrefixTimestampsName :: Text
configMonoidPrefixTimestampsName = "build-output-timestamps"

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

configMonoidCustomPreprocessorExtsName  :: Text
configMonoidCustomPreprocessorExtsName  = "custom-preprocessor-extensions"

configMonoidOverrideGccPathName :: Text
configMonoidOverrideGccPathName = "with-gcc"

configMonoidOverrideHpackName :: Text
configMonoidOverrideHpackName = "with-hpack"

configMonoidConcurrentTestsName :: Text
configMonoidConcurrentTestsName = "concurrent-tests"

configMonoidLocalBinPathName :: Text
configMonoidLocalBinPathName = "local-bin-path"

configMonoidScmInitName :: Text
configMonoidScmInitName = "scm-init"

configMonoidTemplateParametersName :: Text
configMonoidTemplateParametersName = "params"

configMonoidCompilerCheckName :: Text
configMonoidCompilerCheckName = "compiler-check"

configMonoidCompilerRepositoryName :: Text
configMonoidCompilerRepositoryName = "compiler-repository"

configMonoidGhcOptionsName :: Text
configMonoidGhcOptionsName = "ghc-options"

configMonoidExtraPathName :: Text
configMonoidExtraPathName = "extra-path"

configMonoidSetupInfoLocationsName :: Text
configMonoidSetupInfoLocationsName = "setup-info-locations"

configMonoidSetupInfoInlineName :: Text
configMonoidSetupInfoInlineName = "setup-info"

configMonoidLocalProgramsBaseName :: Text
configMonoidLocalProgramsBaseName = "local-programs-path"

configMonoidPvpBoundsName :: Text
configMonoidPvpBoundsName = "pvp-bounds"

configMonoidModifyCodePageName :: Text
configMonoidModifyCodePageName = "modify-code-page"

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

configMonoidSaveHackageCredsName :: Text
configMonoidSaveHackageCredsName = "save-hackage-creds"

configMonoidHackageBaseUrlName :: Text
configMonoidHackageBaseUrlName = "hackage-base-url"

configMonoidColorWhenUSName :: Text
configMonoidColorWhenUSName = "color"

configMonoidColorWhenGBName :: Text
configMonoidColorWhenGBName = "colour"

configMonoidStylesUSName :: Text
configMonoidStylesUSName = "stack-colors"

configMonoidStylesGBName :: Text
configMonoidStylesGBName = "stack-colours"

configMonoidHideSourcePathsName :: Text
configMonoidHideSourcePathsName = "hide-source-paths"

configMonoidRecommendUpgradeName :: Text
configMonoidRecommendUpgradeName = "recommend-stack-upgrade"

configMonoidCasaRepoPrefixName :: Text
configMonoidCasaRepoPrefixName = "casa-repo-prefix"

configMonoidSnapshotLocationName :: Text
configMonoidSnapshotLocationName = "snapshot-location-base"

configMonoidNoRunCompileName :: Text
configMonoidNoRunCompileName = "script-no-run-compile"

configMonoidStackDeveloperModeName :: Text
configMonoidStackDeveloperModeName = "stack-developer-mode"

data ConfigException
  = ParseConfigFileException (Path Abs File) ParseException
  | ParseCustomSnapshotException Text ParseException
  | NoProjectConfigFound (Path Abs Dir) (Maybe Text)
  | UnexpectedArchiveContents [Path Abs Dir] [Path Abs File]
  | UnableToExtractArchive Text (Path Abs File)
  | BadStackVersionException VersionRange
  | NoMatchingSnapshot (NonEmpty SnapName)
  | ResolverMismatch !RawSnapshotLocation String
  | ResolverPartial !RawSnapshotLocation String
  | NoSuchDirectory FilePath
  | ParseGHCVariantException String
  | BadStackRoot (Path Abs Dir)
  | Won'tCreateStackRootInDirectoryOwnedByDifferentUser (Path Abs Dir) (Path Abs Dir) -- ^ @$STACK_ROOT@, parent dir
  | UserDoesn'tOwnDirectory (Path Abs Dir)
  | ManualGHCVariantSettingsAreIncompatibleWithSystemGHC
  | NixRequiresSystemGhc
  | NoResolverWhenUsingNoProject
  | DuplicateLocalPackageNames ![(PackageName, [PackageLocation])]
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
        , "\nSee https://docs.haskellstack.org/en/stable/custom_snapshot/"
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
        [ "Archive extraction failed. Tarballs and zip archives are supported, couldn't handle the following URL, "
        , T.unpack url, " downloaded to the file ", toFilePath $ filename file
        ]
    show (BadStackVersionException requiredRange) = concat
        [ "The version of stack you are using ("
        , show (mkVersion' Meta.version)
        , ") is outside the required\n"
        ,"version range specified in stack.yaml ("
        , T.unpack (versionRangeText requiredRange)
        , ").\n"
        , "You can upgrade stack by running:\n\n"
        , "stack upgrade"
        ]
    show (NoMatchingSnapshot names) = concat
        [ "None of the following snapshots provides a compiler matching "
        , "your package(s):\n"
        , unlines $ map (\name -> "    - " <> show name)
                        (NonEmpty.toList names)
        , resolveOptions
        ]
    show (ResolverMismatch resolver errDesc) = concat
        [ "Resolver '"
        , T.unpack $ utf8BuilderToText $ display resolver
        , "' does not have a matching compiler to build some or all of your "
        , "package(s).\n"
        , errDesc
        , resolveOptions
        ]
    show (ResolverPartial resolver errDesc) = concat
        [ "Resolver '"
        , T.unpack $ utf8BuilderToText $ display resolver
        , "' does not have all the packages to match your requirements.\n"
        , unlines $ fmap ("    " <>) (lines errDesc)
        , resolveOptions
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
    show NoResolverWhenUsingNoProject = "When using the script command, you must provide a resolver argument"
    show (DuplicateLocalPackageNames pairs) = concat
        $ "The same package name is used in multiple local packages\n"
        : map go pairs
      where
        go (name, dirs) = unlines
            $ ""
            : (packageNameString name ++ " used in:")
            : map goLoc dirs
        goLoc loc = "- " ++ show loc
instance Exception ConfigException

resolveOptions :: String
resolveOptions =
  unlines [ "\nThis may be resolved by:"
          , "    - Using '--omit-packages' to exclude mismatching package(s)."
          , "    - Using '--resolver' to specify a matching snapshot/resolver"
          ]

-- | Get the URL to request the information on the latest snapshots
askLatestSnapshotUrl :: (MonadReader env m, HasConfig env) => m Text
askLatestSnapshotUrl = view $ configL.to configLatestSnapshot

-- | @".stack-work"@
workDirL :: HasConfig env => Lens' env (Path Rel Dir)
workDirL = configL.lens configWorkDir (\x y -> x { configWorkDir = y })

-- | @STACK_ROOT\/hooks\/@
hooksDir :: HasConfig env => RIO env (Path Abs Dir)
hooksDir = do
  sr <- view $ configL.to configStackRoot
  pure (sr </> [reldir|hooks|])

-- | @STACK_ROOT\/hooks\/ghc-install.sh@
ghcInstallHook :: HasConfig env => RIO env (Path Abs File)
ghcInstallHook = do
  hd <- hooksDir
  pure (hd </> [relfile|ghc-install.sh|])

-- | Per-project work dir
getProjectWorkDir :: (HasBuildConfig env, MonadReader env m) => m (Path Abs Dir)
getProjectWorkDir = do
    root    <- view projectRootL
    workDir <- view workDirL
    pure (root </> workDir)

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
    pure $ root </> relDirSnapshots </> platform

-- | Installation root for dependencies
installationRootDeps :: (HasEnvConfig env) => RIO env (Path Abs Dir)
installationRootDeps = do
    root <- view stackRootL
    -- TODO: also useShaPathOnWindows here, once #1173 is resolved.
    psc <- platformSnapAndCompilerRel
    pure $ root </> relDirSnapshots </> psc

-- | Installation root for locals
installationRootLocal :: (HasEnvConfig env) => RIO env (Path Abs Dir)
installationRootLocal = do
    workDir <- getProjectWorkDir
    psc <- useShaPathOnWindows =<< platformSnapAndCompilerRel
    pure $ workDir </> relDirInstall </> psc

-- | Installation root for compiler tools
bindirCompilerTools :: (MonadThrow m, MonadReader env m, HasEnvConfig env) => m (Path Abs Dir)
bindirCompilerTools = do
    config <- view configL
    platform <- platformGhcRelDir
    compilerVersion <- view actualCompilerVersionL
    compiler <- parseRelDir $ compilerVersionString compilerVersion
    pure $
        view stackRootL config </>
        relDirCompilerTools </>
        platform </>
        compiler </>
        bindirSuffix

-- | Hoogle directory.
hoogleRoot :: (HasEnvConfig env) => RIO env (Path Abs Dir)
hoogleRoot = do
    workDir <- getProjectWorkDir
    psc <- useShaPathOnWindows =<< platformSnapAndCompilerRel
    pure $ workDir </> relDirHoogle </> psc

-- | Get the hoogle database path.
hoogleDatabasePath :: (HasEnvConfig env) => RIO env (Path Abs File)
hoogleDatabasePath = do
    dir <- hoogleRoot
    pure (dir </> relFileDatabaseHoo)

-- | Path for platform followed by snapshot name followed by compiler
-- name.
platformSnapAndCompilerRel
    :: (HasEnvConfig env)
    => RIO env (Path Rel Dir)
platformSnapAndCompilerRel = do
    platform <- platformGhcRelDir
    smh <- view $ envConfigL.to envConfigSourceMapHash
    name <- smRelDir smh
    ghc <- compilerVersionDir
    useShaPathOnWindows (platform </> name </> ghc)

-- | Relative directory for the platform and GHC identifier
platformGhcRelDir
    :: (MonadReader env m, HasEnvConfig env, MonadThrow m)
    => m (Path Rel Dir)
platformGhcRelDir = do
    cp <- view compilerPathsL
    let cbSuffix = compilerBuildSuffix $ cpBuild cp
    verOnly <- platformGhcVerOnlyRelDirStr
    parseRelDir (mconcat [ verOnly, cbSuffix ])

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
    pure $ mconcat [ Distribution.Text.display platform
                     , platformVariantSuffix platformVariant
                     , ghcVariantSuffix ghcVariant ]

-- | This is an attempt to shorten stack paths on Windows to decrease our
-- chances of hitting 260 symbol path limit. The idea is to calculate
-- SHA1 hash of the path used on other architectures, encode with base
-- 16 and take first 8 symbols of it.
useShaPathOnWindows :: MonadThrow m => Path Rel Dir -> m (Path Rel Dir)
useShaPathOnWindows
  | osIsWindows = shaPath
  | otherwise = pure

shaPath :: (IsPath Rel t, MonadThrow m) => Path Rel t -> m (Path Rel t)
shaPath = shaPathForBytes . encodeUtf8 . T.pack . toFilePath

shaPathForBytes :: (IsPath Rel t, MonadThrow m) => ByteString -> m (Path Rel t)
shaPathForBytes
    = parsePath . S8.unpack . S8.take 8
    . Mem.convertToBase Mem.Base16 . hashWith SHA1

-- TODO: Move something like this into the path package. Consider
-- subsuming path-io's 'AnyPath'?
class IsPath b t where
  parsePath :: MonadThrow m => FilePath -> m (Path b t)

instance IsPath Abs Dir where parsePath = parseAbsDir
instance IsPath Rel Dir where parsePath = parseRelDir
instance IsPath Abs File where parsePath = parseAbsFile
instance IsPath Rel File where parsePath = parseRelFile

compilerVersionDir :: (MonadThrow m, MonadReader env m, HasEnvConfig env) => m (Path Rel Dir)
compilerVersionDir = do
    compilerVersion <- view actualCompilerVersionL
    parseRelDir $ case compilerVersion of
        ACGhc version -> versionString version
        ACGhcGit {} -> compilerVersionString compilerVersion

-- | Package database for installing dependencies into
packageDatabaseDeps :: (HasEnvConfig env) => RIO env (Path Abs Dir)
packageDatabaseDeps = do
    root <- installationRootDeps
    pure $ root </> relDirPkgdb

-- | Package database for installing local packages into
packageDatabaseLocal :: (HasEnvConfig env) => RIO env (Path Abs Dir)
packageDatabaseLocal = do
    root <- installationRootLocal
    pure $ root </> relDirPkgdb

-- | Extra package databases
packageDatabaseExtra :: (MonadReader env m, HasEnvConfig env) => m [Path Abs Dir]
packageDatabaseExtra = view $ buildConfigL.to bcExtraPackageDBs

-- | Where do we get information on global packages for loading up a
-- 'LoadedSnapshot'?
data GlobalInfoSource
  = GISSnapshotHints
  -- ^ Accept the hints in the snapshot definition
  | GISCompiler ActualCompiler
  -- ^ Look up the actual information in the installed compiler

-- | Where HPC reports and tix files get stored.
hpcReportDir :: (HasEnvConfig env)
             => RIO env (Path Abs Dir)
hpcReportDir = do
   root <- installationRootLocal
   pure $ root </> relDirHpc

-- | Get the extra bin directories (for the PATH). Puts more local first
--
-- Bool indicates whether or not to include the locals
extraBinDirs :: (HasEnvConfig env)
             => RIO env (Bool -> [Path Abs Dir])
extraBinDirs = do
    deps <- installationRootDeps
    local' <- installationRootLocal
    tools <- bindirCompilerTools
    pure $ \locals -> if locals
        then [local' </> bindirSuffix, deps </> bindirSuffix, tools]
        else [deps </> bindirSuffix, tools]

minimalEnvSettings :: EnvSettings
minimalEnvSettings =
    EnvSettings
    { esIncludeLocals = False
    , esIncludeGhcPackagePath = False
    , esStackExe = False
    , esLocaleUtf8 = False
    , esKeepGhcRts = False
    }

-- | Default @EnvSettings@ which includes locals and GHC_PACKAGE_PATH.
--
-- Note that this also passes through the GHCRTS environment variable.
-- See https://github.com/commercialhaskell/stack/issues/3444
defaultEnvSettings :: EnvSettings
defaultEnvSettings = EnvSettings
    { esIncludeLocals = True
    , esIncludeGhcPackagePath = True
    , esStackExe = True
    , esLocaleUtf8 = False
    , esKeepGhcRts = True
    }

-- | Environment settings which do not embellish the environment
--
-- Note that this also passes through the GHCRTS environment variable.
-- See https://github.com/commercialhaskell/stack/issues/3444
plainEnvSettings :: EnvSettings
plainEnvSettings = EnvSettings
    { esIncludeLocals = False
    , esIncludeGhcPackagePath = False
    , esStackExe = False
    , esLocaleUtf8 = False
    , esKeepGhcRts = True
    }

-- | Get the path for the given compiler ignoring any local binaries.
--
-- https://github.com/commercialhaskell/stack/issues/1052
getCompilerPath :: HasCompiler env => RIO env (Path Abs File)
getCompilerPath = view $ compilerPathsL.to cpCompiler

data ProjectAndConfigMonoid
  = ProjectAndConfigMonoid !Project !ConfigMonoid

parseProjectAndConfigMonoid :: Path Abs Dir -> Value -> Yaml.Parser (WithJSONWarnings (IO ProjectAndConfigMonoid))
parseProjectAndConfigMonoid rootDir =
    withObjectWarnings "ProjectAndConfigMonoid" $ \o -> do
        packages <- o ..:? "packages" ..!= [RelFilePath "."]
        deps <- jsonSubWarningsTT (o ..:? "extra-deps") ..!= []
        flags' <- o ..:? "flags" ..!= mempty
        let flags = unCabalStringMap <$> unCabalStringMap
                    (flags' :: Map (CabalString PackageName) (Map (CabalString FlagName) Bool))

        resolver <- jsonSubWarnings $ o ...: ["snapshot", "resolver"]
        mcompiler <- o ..:? "compiler"
        msg <- o ..:? "user-message"
        config <- parseConfigMonoidObject rootDir o
        extraPackageDBs <- o ..:? "extra-package-dbs" ..!= []
        mcurator <- jsonSubWarningsT (o ..:? "curator")
        drops <- o ..:? "drop-packages" ..!= mempty
        pure $ do
          deps' <- mapM (resolvePaths (Just rootDir)) deps
          resolver' <- resolvePaths (Just rootDir) resolver
          let project = Project
                  { projectUserMsg = msg
                  , projectResolver = resolver'
                  , projectCompiler = mcompiler -- FIXME make sure resolver' isn't SLCompiler
                  , projectExtraPackageDBs = extraPackageDBs
                  , projectPackages = packages
                  , projectDependencies = concatMap toList (deps' :: [NonEmpty RawPackageLocation])
                  , projectFlags = flags
                  , projectCurator = mcurator
                  , projectDropPackages = Set.map unCabalString drops
                  }
          pure $ ProjectAndConfigMonoid project config

-- | A software control system.
data SCM = Git
  deriving (Show)

instance FromJSON SCM where
    parseJSON v = do
        s <- parseJSON v
        case s of
            "git" -> pure Git
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

-- | Specialized variant of GHC (e.g. libgmp4 or integer-simple)
data GHCVariant
    = GHCStandard
    -- ^ Standard bindist
    | GHCIntegerSimple
    -- ^ Bindist that uses integer-simple
    | GHCNativeBignum
    -- ^ Bindist that uses the Haskell-native big-integer backend
    | GHCCustom String
    -- ^ Other bindists
    deriving (Show)

instance FromJSON GHCVariant where
    -- Strange structuring is to give consistent error messages
    parseJSON =
        withText
            "GHCVariant"
            (either (fail . show) pure . parseGHCVariant . T.unpack)

-- | Render a GHC variant to a String.
ghcVariantName :: GHCVariant -> String
ghcVariantName GHCStandard = "standard"
ghcVariantName GHCIntegerSimple = "integersimple"
ghcVariantName GHCNativeBignum = "int-native"
ghcVariantName (GHCCustom name) = "custom-" ++ name

-- | Render a GHC variant to a String suffix.
ghcVariantSuffix :: GHCVariant -> String
ghcVariantSuffix GHCStandard = ""
ghcVariantSuffix v = "-" ++ ghcVariantName v

-- | Parse GHC variant from a String.
parseGHCVariant :: (MonadThrow m) => String -> m GHCVariant
parseGHCVariant s =
    case stripPrefix "custom-" s of
        Just name -> pure (GHCCustom name)
        Nothing
          | s == "" -> pure GHCStandard
          | s == "standard" -> pure GHCStandard
          | s == "integersimple" -> pure GHCIntegerSimple
          | s == "int-native" -> pure GHCNativeBignum
          | otherwise -> pure (GHCCustom s)

-- | Build of the compiler distribution (e.g. standard, gmp4, tinfo6)
-- | Information for a file to download.
data DownloadInfo = DownloadInfo
    { downloadInfoUrl :: Text
      -- ^ URL or absolute file path
    , downloadInfoContentLength :: Maybe Int
    , downloadInfoSha1 :: Maybe ByteString
    , downloadInfoSha256 :: Maybe ByteString
    } deriving (Show)

instance FromJSON (WithJSONWarnings DownloadInfo) where
    parseJSON = withObjectWarnings "DownloadInfo" parseDownloadInfoFromObject

-- | Parse JSON in existing object for 'DownloadInfo'
parseDownloadInfoFromObject :: Object -> WarningParser DownloadInfo
parseDownloadInfoFromObject o = do
    url <- o ..: "url"
    contentLength <- o ..:? "content-length"
    sha1TextMay <- o ..:? "sha1"
    sha256TextMay <- o ..:? "sha256"
    pure
        DownloadInfo
        { downloadInfoUrl = url
        , downloadInfoContentLength = contentLength
        , downloadInfoSha1 = fmap encodeUtf8 sha1TextMay
        , downloadInfoSha256 = fmap encodeUtf8 sha256TextMay
        }

data VersionedDownloadInfo = VersionedDownloadInfo
    { vdiVersion :: Version
    , vdiDownloadInfo :: DownloadInfo
    }
    deriving Show

instance FromJSON (WithJSONWarnings VersionedDownloadInfo) where
    parseJSON = withObjectWarnings "VersionedDownloadInfo" $ \o -> do
        CabalString version <- o ..: "version"
        downloadInfo <- parseDownloadInfoFromObject o
        pure VersionedDownloadInfo
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
        pure GHCDownloadInfo
            { gdiConfigureOpts = configureOpts
            , gdiConfigureEnv = configureEnv
            , gdiDownloadInfo = downloadInfo
            }

data SetupInfo = SetupInfo
    { siSevenzExe :: Maybe DownloadInfo
    , siSevenzDll :: Maybe DownloadInfo
    , siMsys2 :: Map Text VersionedDownloadInfo
    , siGHCs :: Map Text (Map Version GHCDownloadInfo)
    , siStack :: Map Text (Map Version DownloadInfo)
    }
    deriving Show

instance FromJSON (WithJSONWarnings SetupInfo) where
    parseJSON = withObjectWarnings "SetupInfo" $ \o -> do
        siSevenzExe <- jsonSubWarningsT (o ..:? "sevenzexe-info")
        siSevenzDll <- jsonSubWarningsT (o ..:? "sevenzdll-info")
        siMsys2 <- jsonSubWarningsT (o ..:? "msys2" ..!= mempty)
        (fmap unCabalStringMap -> siGHCs) <- jsonSubWarningsTT (o ..:? "ghc" ..!= mempty)
        (fmap unCabalStringMap -> siStack) <- jsonSubWarningsTT (o ..:? "stack" ..!= mempty)
        pure SetupInfo {..}

-- | For the @siGHCs@ field maps are deeply merged.
-- For all fields the values from the first @SetupInfo@ win.
instance Semigroup SetupInfo where
    l <> r =
        SetupInfo
        { siSevenzExe = siSevenzExe l <|> siSevenzExe r
        , siSevenzDll = siSevenzDll l <|> siSevenzDll r
        , siMsys2 = siMsys2 l <> siMsys2 r
        , siGHCs = Map.unionWith (<>) (siGHCs l) (siGHCs r)
        , siStack = Map.unionWith (<>) (siStack l) (siStack r) }

instance Monoid SetupInfo where
    mempty =
        SetupInfo
        { siSevenzExe = Nothing
        , siSevenzDll = Nothing
        , siMsys2 = Map.empty
        , siGHCs = Map.empty
        , siStack = Map.empty
        }
    mappend = (<>)

-- | How PVP bounds should be added to .cabal files
data PvpBoundsType
  = PvpBoundsNone
  | PvpBoundsUpper
  | PvpBoundsLower
  | PvpBoundsBoth
  deriving (Show, Read, Eq, Typeable, Ord, Enum, Bounded)

data PvpBounds = PvpBounds
  { pbType :: !PvpBoundsType
  , pbAsRevision :: !Bool
  }
  deriving (Show, Read, Eq, Typeable, Ord)

pvpBoundsText :: PvpBoundsType -> Text
pvpBoundsText PvpBoundsNone = "none"
pvpBoundsText PvpBoundsUpper = "upper"
pvpBoundsText PvpBoundsLower = "lower"
pvpBoundsText PvpBoundsBoth = "both"

parsePvpBounds :: Text -> Either String PvpBounds
parsePvpBounds t = maybe err Right $ do
    (t', asRevision) <-
      case T.break (== '-') t of
        (x, "") -> Just (x, False)
        (x, "-revision") -> Just (x, True)
        _ -> Nothing
    x <- Map.lookup t' m
    Just PvpBounds
      { pbType = x
      , pbAsRevision = asRevision
      }
  where
    m = Map.fromList $ map (pvpBoundsText &&& id) [minBound..maxBound]
    err = Left $ "Invalid PVP bounds: " ++ T.unpack t

instance ToJSON PvpBounds where
  toJSON (PvpBounds typ asRevision) =
    toJSON (pvpBoundsText typ <> (if asRevision then "-revision" else ""))
instance FromJSON PvpBounds where
  parseJSON = withText "PvpBounds" (either fail pure . parsePvpBounds)

-- | Data passed into Docker container for the Docker entrypoint's use
newtype DockerEntrypoint = DockerEntrypoint
    { deUser :: Maybe DockerUser
      -- ^ UID/GID/etc of host user, if we wish to perform UID/GID switch in container
    } deriving (Read,Show)

-- | Docker host user info
data DockerUser = DockerUser
    { duUid :: UserID -- ^ uid
    , duGid :: GroupID -- ^ gid
    , duGroups :: [GroupID] -- ^ Supplemental groups
    , duUmask :: FileMode -- ^ File creation mask }
    } deriving (Read,Show)

data GhcOptionKey
  = GOKOldEverything
  | GOKEverything
  | GOKLocals
  | GOKTargets
  | GOKPackage !PackageName
  deriving (Eq, Ord)

instance FromJSONKey GhcOptionKey where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    case t of
      "*" -> pure GOKOldEverything
      "$everything" -> pure GOKEverything
      "$locals" -> pure GOKLocals
      "$targets" -> pure GOKTargets
      _ ->
        case parsePackageName $ T.unpack t of
          Nothing -> fail $ "Invalid package name: " ++ show t
          Just x -> pure $ GOKPackage x
  fromJSONKeyList = FromJSONKeyTextParser $ \_ -> fail "GhcOptionKey.fromJSONKeyList"

newtype GhcOptions = GhcOptions { unGhcOptions :: [Text] }

instance FromJSON GhcOptions where
  parseJSON = withText "GhcOptions" $ \t ->
    case parseArgs Escaping t of
      Left e -> fail e
      Right opts -> pure $ GhcOptions $ map T.pack opts

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
    ghcVariantL :: SimpleGetter env GHCVariant
    default ghcVariantL :: HasConfig env => SimpleGetter env GHCVariant
    ghcVariantL = configL.ghcVariantL
    {-# INLINE ghcVariantL #-}

-- | Class for environment values which have a 'Runner'.
class (HasProcessContext env, HasLogFunc env) => HasRunner env where
  runnerL :: Lens' env Runner
instance HasLogFunc Runner where
  logFuncL = lens runnerLogFunc (\x y -> x { runnerLogFunc = y })
instance HasProcessContext Runner where
  processContextL = lens runnerProcessContext (\x y -> x { runnerProcessContext = y })
instance HasRunner Runner where
  runnerL = id
instance HasStylesUpdate Runner where
  stylesUpdateL = globalOptsL.
                  lens globalStylesUpdate (\x y -> x { globalStylesUpdate = y })
instance HasTerm Runner where
  useColorL = lens runnerUseColor (\x y -> x { runnerUseColor = y })
  termWidthL = lens runnerTermWidth (\x y -> x { runnerTermWidth = y })

globalOptsL :: HasRunner env => Lens' env GlobalOpts
globalOptsL = runnerL.lens runnerGlobalOpts (\x y -> x { runnerGlobalOpts = y })

-- | Class for environment values that can provide a 'Config'.
class ( HasPlatform env, HasGHCVariant env, HasProcessContext env, HasPantryConfig env, HasTerm env, HasRunner env) => HasConfig env where
    configL :: Lens' env Config
    default configL :: HasBuildConfig env => Lens' env Config
    configL = buildConfigL.lens bcConfig (\x y -> x { bcConfig = y })
    {-# INLINE configL #-}

class HasConfig env => HasBuildConfig env where
    buildConfigL :: Lens' env BuildConfig
    default buildConfigL :: HasEnvConfig env => Lens' env BuildConfig
    buildConfigL = envConfigL.lens
        envConfigBuildConfig
        (\x y -> x { envConfigBuildConfig = y })

class (HasBuildConfig env, HasSourceMap env, HasCompiler env) => HasEnvConfig env where
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
instance HasPlatform BuildConfig
instance HasPlatform EnvConfig

instance HasGHCVariant GHCVariant where
    ghcVariantL = id
    {-# INLINE ghcVariantL #-}
instance HasGHCVariant Config where
    ghcVariantL = to $ fromMaybe GHCStandard . configGHCVariant
instance HasGHCVariant BuildConfig
instance HasGHCVariant EnvConfig

instance HasProcessContext Config where
    processContextL = runnerL.processContextL
instance HasProcessContext BuildConfig where
    processContextL = configL.processContextL
instance HasProcessContext EnvConfig where
    processContextL = configL.processContextL

instance HasPantryConfig Config where
    pantryConfigL = lens configPantryConfig (\x y -> x { configPantryConfig = y })
instance HasPantryConfig BuildConfig where
    pantryConfigL = configL.pantryConfigL
instance HasPantryConfig EnvConfig where
    pantryConfigL = configL.pantryConfigL

instance HasConfig Config where
    configL = id
    {-# INLINE configL #-}
instance HasConfig BuildConfig where
    configL = lens bcConfig (\x y -> x { bcConfig = y })
instance HasConfig EnvConfig

instance HasBuildConfig BuildConfig where
    buildConfigL = id
    {-# INLINE buildConfigL #-}
instance HasBuildConfig EnvConfig

instance HasCompiler EnvConfig where
    compilerPathsL = to envConfigCompilerPaths
instance HasEnvConfig EnvConfig where
    envConfigL = id
    {-# INLINE envConfigL #-}

instance HasRunner Config where
  runnerL = lens configRunner (\x y -> x { configRunner = y })
instance HasRunner BuildConfig where
  runnerL = configL.runnerL
instance HasRunner EnvConfig where
  runnerL = configL.runnerL

instance HasLogFunc Config where
  logFuncL = runnerL.logFuncL
instance HasLogFunc BuildConfig where
  logFuncL = runnerL.logFuncL
instance HasLogFunc EnvConfig where
  logFuncL = runnerL.logFuncL

instance HasStylesUpdate Config where
  stylesUpdateL = runnerL.stylesUpdateL
instance HasStylesUpdate BuildConfig where
  stylesUpdateL = runnerL.stylesUpdateL
instance HasStylesUpdate EnvConfig where
  stylesUpdateL = runnerL.stylesUpdateL

instance HasTerm Config where
  useColorL = runnerL.useColorL
  termWidthL = runnerL.termWidthL
instance HasTerm BuildConfig where
  useColorL = runnerL.useColorL
  termWidthL = runnerL.termWidthL
instance HasTerm EnvConfig where
  useColorL = runnerL.useColorL
  termWidthL = runnerL.termWidthL

-----------------------------------
-- Helper lenses
-----------------------------------

stackRootL :: HasConfig s => Lens' s (Path Abs Dir)
stackRootL = configL.lens configStackRoot (\x y -> x { configStackRoot = y })

-- | The compiler specified by the @SnapshotDef@. This may be
-- different from the actual compiler used!
wantedCompilerVersionL :: HasBuildConfig s => Getting r s WantedCompiler
wantedCompilerVersionL = buildConfigL.to (smwCompiler . bcSMWanted)

-- | Location of the ghc-pkg executable
newtype GhcPkgExe = GhcPkgExe (Path Abs File)
  deriving Show

-- | Get the 'GhcPkgExe' from a 'HasCompiler' environment
getGhcPkgExe :: HasCompiler env => RIO env GhcPkgExe
getGhcPkgExe = view $ compilerPathsL.to cpPkg

-- | Dump information for a single package
data DumpPackage = DumpPackage
    { dpGhcPkgId :: !GhcPkgId
    , dpPackageIdent :: !PackageIdentifier
    , dpParentLibIdent :: !(Maybe PackageIdentifier)
    , dpLicense :: !(Maybe C.License)
    , dpLibDirs :: ![FilePath]
    , dpLibraries :: ![Text]
    , dpHasExposedModules :: !Bool
    , dpExposedModules :: !(Set ModuleName)
    , dpDepends :: ![GhcPkgId]
    , dpHaddockInterfaces :: ![FilePath]
    , dpHaddockHtml :: !(Maybe FilePath)
    , dpIsExposed :: !Bool
    }
    deriving (Show, Read, Eq)

-- | Paths on the filesystem for the compiler we're using
data CompilerPaths = CompilerPaths
  { cpCompilerVersion :: !ActualCompiler
  , cpArch :: !Arch
  , cpBuild :: !CompilerBuild
  , cpCompiler :: !(Path Abs File)
  -- | ghc-pkg or equivalent
  , cpPkg :: !GhcPkgExe
  -- | runghc
  , cpInterpreter :: !(Path Abs File)
  -- | haddock, in 'IO' to allow deferring the lookup
  , cpHaddock :: !(Path Abs File)
  -- | Is this a Stack-sandboxed installation?
  , cpSandboxed :: !Bool
  , cpCabalVersion :: !Version
  -- ^ This is the version of Cabal that stack will use to compile Setup.hs files
  -- in the build process.
  --
  -- Note that this is not necessarily the same version as the one that stack
  -- depends on as a library and which is displayed when running
  -- @stack ls dependencies | grep Cabal@ in the stack project.
  , cpGlobalDB :: !(Path Abs Dir)
  -- ^ Global package database
  , cpGhcInfo :: !ByteString
  -- ^ Output of @ghc --info@
  , cpGlobalDump :: !(Map PackageName DumpPackage)
  }
  deriving Show

cpWhich :: (MonadReader env m, HasCompiler env) => m WhichCompiler
cpWhich = view $ compilerPathsL.to (whichCompiler.cpCompilerVersion)

data ExtraDirs = ExtraDirs
    { edBins :: ![Path Abs Dir]
    , edInclude :: ![Path Abs Dir]
    , edLib :: ![Path Abs Dir]
    } deriving (Show, Generic)
instance Semigroup ExtraDirs where
    (<>) = mappenddefault
instance Monoid ExtraDirs where
    mempty = memptydefault
    mappend = (<>)

-- | An environment which ensures that the given compiler is available
-- on the PATH
class HasCompiler env where
  compilerPathsL :: SimpleGetter env CompilerPaths
instance HasCompiler CompilerPaths where
  compilerPathsL = id

class HasSourceMap env where
  sourceMapL :: Lens' env SourceMap
instance HasSourceMap EnvConfig where
  sourceMapL = lens envConfigSourceMap (\x y -> x { envConfigSourceMap = y })

-- | The version of the compiler which will actually be used. May be
-- different than that specified in the 'SnapshotDef' and returned
-- by 'wantedCompilerVersionL'.
actualCompilerVersionL :: HasSourceMap env => SimpleGetter env ActualCompiler
actualCompilerVersionL = sourceMapL.to smCompiler

buildOptsL :: HasConfig s => Lens' s BuildOpts
buildOptsL = configL.lens
    configBuild
    (\x y -> x { configBuild = y })

buildOptsMonoidHaddockL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidHaddockL = lens (getFirstFalse . buildMonoidHaddock)
                            (\buildMonoid t -> buildMonoid {buildMonoidHaddock = FirstFalse t})

buildOptsMonoidTestsL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidTestsL = lens (getFirstFalse . buildMonoidTests)
                            (\buildMonoid t -> buildMonoid {buildMonoidTests = FirstFalse t})

buildOptsMonoidBenchmarksL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidBenchmarksL = lens (getFirstFalse . buildMonoidBenchmarks)
                            (\buildMonoid t -> buildMonoid {buildMonoidBenchmarks = FirstFalse t})

buildOptsMonoidInstallExesL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidInstallExesL =
  lens (getFirstFalse . buildMonoidInstallExes)
       (\buildMonoid t -> buildMonoid {buildMonoidInstallExes = FirstFalse t})

buildOptsInstallExesL :: Lens' BuildOpts Bool
buildOptsInstallExesL =
  lens boptsInstallExes
       (\bopts t -> bopts {boptsInstallExes = t})

buildOptsHaddockL :: Lens' BuildOpts Bool
buildOptsHaddockL =
  lens boptsHaddock
       (\bopts t -> bopts {boptsHaddock = t})

globalOptsBuildOptsMonoidL :: Lens' GlobalOpts BuildOptsMonoid
globalOptsBuildOptsMonoidL =
  lens
    globalConfigMonoid
    (\x y -> x { globalConfigMonoid = y })
  .
  lens
    configMonoidBuildOpts
    (\x y -> x { configMonoidBuildOpts = y })

cabalVersionL :: HasCompiler env => SimpleGetter env Version
cabalVersionL = compilerPathsL.to cpCabalVersion

whichCompilerL :: Getting r ActualCompiler WhichCompiler
whichCompilerL = to whichCompiler

envOverrideSettingsL :: HasConfig env => Lens' env (EnvSettings -> IO ProcessContext)
envOverrideSettingsL = configL.lens
    configProcessContextSettings
    (\x y -> x { configProcessContextSettings = y })

shouldForceGhcColorFlag :: (HasRunner env, HasEnvConfig env)
                        => RIO env Bool
shouldForceGhcColorFlag = do
    canDoColor <- (>= mkVersion [8, 2, 1]) . getGhcVersion
              <$> view actualCompilerVersionL
    shouldDoColor <- view useColorL
    pure $ canDoColor && shouldDoColor

appropriateGhcColorFlag :: (HasRunner env, HasEnvConfig env)
                        => RIO env (Maybe String)
appropriateGhcColorFlag = f <$> shouldForceGhcColorFlag
  where f True = Just ghcColorForceFlag
        f False = Nothing

-- | See 'globalTerminal'
terminalL :: HasRunner env => Lens' env Bool
terminalL = globalOptsL.lens globalTerminal (\x y -> x { globalTerminal = y })

-- | See 'globalReExecVersion'
reExecL :: HasRunner env => SimpleGetter env Bool
reExecL = globalOptsL.to (isJust . globalReExecVersion)

-- | In dev mode, print as a warning, otherwise as debug
prettyStackDevL :: HasConfig env => [StyleDoc] -> RIO env ()
prettyStackDevL docs = do
  config <- view configL
  if configStackDeveloperMode config
    then prettyWarnL docs
    else prettyDebugL docs
