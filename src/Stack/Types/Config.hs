{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

-- | The Config type.

module Stack.Types.Config
  (
  -- * Main configuration types and classes
  -- ** HasPlatform & HasStackRoot
    HasPlatform (..)
  , PlatformVariant (..)
  -- ** Config & HasConfig
  , Config (..)
  , HasConfig (..)
  , askLatestSnapshotUrl
  , configProjectRoot
  -- ** BuildConfig & HasBuildConfig
  , BuildConfig (..)
  , ProjectPackage (..)
  , DepPackage (..)
  , ppRoot
  , ppVersion
  , ppComponents
  , ppGPD
  , stackYamlL
  , projectRootL
  , HasBuildConfig (..)
  -- ** Storage databases
  , UserStorage (..)
  , ProjectStorage (..)
  -- ** GHCVariant & HasGHCVariant
  , HasGHCVariant (..)
  , snapshotsDir
  -- ** EnvConfig & HasEnvConfig
  , EnvConfig (..)
  , HasSourceMap (..)
  , HasEnvConfig (..)
  , getCompilerPath
  -- * Details
  -- ** ConfigException
  , HpackExecutable (..)
  , ConfigException (..)
  , ConfigPrettyException (..)
  , ParseAbsolutePathException (..)
  , packageIndicesWarning
  -- ** ConfigMonoid
  , configMonoidInstallGHCName
  , configMonoidSystemGHCName
  , parseConfigMonoid
  -- ** EnvSettings
  , EnvSettings (..)
  , minimalEnvSettings
  , defaultEnvSettings
  , plainEnvSettings
  -- ** GlobalOpts & GlobalOptsMonoid
  , GlobalOptsMonoid (..)
  , defaultLogLevel
  -- ** Project & ProjectAndConfigMonoid
  , Project (..)
  , ProjectConfig (..)
  , Curator (..)
  , ProjectAndConfigMonoid (..)
  , parseProjectAndConfigMonoid
  -- ** Styles
  , readStyles
  -- * Paths
  , bindirSuffix
  , GlobalInfoSource (..)
  , getProjectWorkDir
  , docDirSuffix
  , extraBinDirs
  , hpcReportDir
  , installationRootDeps
  , installationRootLocal
  , bindirCompilerTools
  , hoogleRoot
  , hoogleDatabasePath
  , packageDatabaseDeps
  , packageDatabaseExtra
  , packageDatabaseLocal
  , platformOnlyRelDir
  , platformGhcRelDir
  , platformGhcVerOnlyRelDir
  , useShaPathOnWindows
  , shaPath
  , shaPathForBytes
  , workDirL
  , ghcInstallHook
  -- * Command-related types
  , AddCommand
  , module X
  -- * Lens helpers
  , wantedCompilerVersionL
  , actualCompilerVersionL
  , HasCompiler (..)
  , DumpPackage (..)
  , CompilerPaths (..)
  , GhcPkgExe (..)
  , getGhcPkgExe
  , cpWhich
  , ExtraDirs (..)
  , buildOptsL
  , globalOptsL
  , buildOptsInstallExesL
  , buildOptsMonoidHaddockL
  , buildOptsMonoidTestsL
  , buildOptsMonoidBenchmarksL
  , buildOptsMonoidInstallExesL
  , buildOptsHaddockL
  , globalOptsBuildOptsMonoidL
  , stackRootL
  , stackGlobalConfigL
  , cabalVersionL
  , whichCompilerL
  , envOverrideSettingsL
  , shouldForceGhcColorFlag
  , appropriateGhcColorFlag
  -- * Helper logging functions
  , prettyStackDevL
  -- * Lens reexport
  , view
  , to
  ) where

import           Control.Monad.Writer ( Writer, tell )
import           Control.Monad.Trans.Except ( ExceptT )
import           Crypto.Hash ( SHA1 (..), hashWith )
import           Pantry.Internal.AesonExtended
                   ( FromJSON, FromJSONKey (..)
                   , FromJSONKeyFunction (FromJSONKeyTextParser), Object, ToJSON
                   , Value, WarningParser, WithJSONWarnings (..), (.=)
                   ,  (...:), (..:?), (..!=), jsonSubWarnings
                   , jsonSubWarningsT, jsonSubWarningsTT, object, parseJSON
                   , toJSON, withObjectWarnings, withText
                   )
import           Data.Attoparsec.Args ( EscapingMode (Escaping), parseArgs )
import qualified Data.ByteArray.Encoding as Mem ( Base(Base16), convertToBase )
import qualified Data.ByteString.Char8 as S8
import           Data.Coerce ( coerce )
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import qualified Data.Monoid as Monoid
import           Data.Monoid.Map ( MonoidMap (..) )
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Yaml ( ParseException )
import qualified Data.Yaml as Yaml
import qualified Distribution.License as C
import           Distribution.ModuleName ( ModuleName )
import           Distribution.PackageDescription ( GenericPackageDescription )
import qualified Distribution.PackageDescription as C
import           Distribution.System ( Platform, Arch )
import qualified Distribution.Text ( display )
import           Distribution.Version ( anyVersion, mkVersion )
import           Generics.Deriving.Monoid ( mappenddefault, memptydefault )
import           Lens.Micro ( _1, _2 )
import           Options.Applicative ( ReadM )
import qualified Options.Applicative as OA
import qualified Options.Applicative.Types as OA
import           Pantry.Internal ( Storage )
import           Path
                   ( (</>), dirname, filename, parent, parseAbsDir, parseAbsFile
                   , parseRelDir, parseRelFile, reldir, relfile
                   )
import           Stack.Constants
                   ( bindirSuffix, docDirSuffix, ghcColorForceFlag, osIsWindows
                   , relDirCompilerTools, relDirHoogle, relDirHpc, relDirInstall
                   , relDirPkgdb, relDirSnapshots, relFileDatabaseHoo
                   )
import           Stack.Prelude
import           Stack.Types.ApplyGhcOptions ( ApplyGhcOptions (..) )
import           Stack.Types.CabalConfigKey ( CabalConfigKey )
import           Stack.Types.Compiler
                   ( ActualCompiler (..), CompilerRepository, WhichCompiler
                   , compilerVersionString, getGhcVersion, whichCompiler
                   )
import           Stack.Types.CompilerBuild
                   ( CompilerBuild, compilerBuildSuffix )
import           Stack.Types.ConfigMonoid ( ConfigMonoid (..) )
import           Stack.Types.Docker
                   ( DockerOpts, VersionRangeJSON (..) )
import           Stack.Types.DockerEntrypoint ( DockerEntrypoint )
import           Stack.Types.DumpLogs ( DumpLogs )
import           Stack.Types.GHCVariant ( GHCVariant (..), ghcVariantSuffix )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.LockFileBehavior ( LockFileBehavior )
import           Stack.Types.NamedComponent ( NamedComponent (..) )
import           Stack.Types.Nix ( NixOpts )
import           Stack.Types.PvpBounds ( PvpBounds )
import           Stack.Types.Resolver ( AbstractResolver )
import           Stack.Types.Runner ( HasRunner (..), Runner, globalOptsL )
import           Stack.Types.SCM ( SCM )
import           Stack.Types.SetupInfo ( SetupInfo )
import           Stack.Types.SourceMap
                   ( CommonPackage (..), DepPackage (..), ProjectPackage (..)
                   , SMWanted (..), SourceMap (..), SourceMapHash, smRelDir
                   )
import           Stack.Types.TemplateName ( TemplateName )
import           Stack.Types.Version
                   ( IntersectingVersionRange (..), VersionCheck (..)
                   , VersionRange, stackVersion, versionRangeText
                   )
import qualified System.FilePath as FilePath
import           RIO.Process ( HasProcessContext (..), ProcessContext )

-- Re-exports
import           Stack.Types.Config.Build as X

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Config" module.
data ConfigException
  = ParseCustomSnapshotException Text ParseException
  | NoProjectConfigFound (Path Abs Dir) (Maybe Text)
  | UnexpectedArchiveContents [Path Abs Dir] [Path Abs File]
  | UnableToExtractArchive Text (Path Abs File)
  | BadStackVersionException VersionRange
  | NoSuchDirectory FilePath
  | ParseGHCVariantException String
  | BadStackRoot (Path Abs Dir)
  | Won'tCreateStackRootInDirectoryOwnedByDifferentUser
      (Path Abs Dir)
      (Path Abs Dir)
    -- ^ @$STACK_ROOT@, parent dir
  | UserDoesn'tOwnDirectory (Path Abs Dir)
  | ManualGHCVariantSettingsAreIncompatibleWithSystemGHC
  | NixRequiresSystemGhc
  | NoResolverWhenUsingNoProject
  | NoLTSWithMajorVersion Int
  | NoLTSFound
  deriving (Show, Typeable)

instance Exception ConfigException where
  displayException (ParseCustomSnapshotException url exception) = concat
    [ "Error: [S-8981]\n"
    , "Could not parse '"
    , T.unpack url
    , "':\n"
    , Yaml.prettyPrintParseException exception
    , "\nSee https://docs.haskellstack.org/en/stable/custom_snapshot/"
    ]
  displayException (NoProjectConfigFound dir mcmd) = concat
    [ "Error: [S-2206]\n"
    , "Unable to find a stack.yaml file in the current directory ("
    , toFilePath dir
    , ") or its ancestors"
    , case mcmd of
        Nothing -> ""
        Just cmd -> "\nRecommended action: stack " ++ T.unpack cmd
    ]
  displayException (UnexpectedArchiveContents dirs files) = concat
    [ "Error: [S-4964]\n"
    , "When unpacking an archive specified in your stack.yaml file, "
    , "did not find expected contents. Expected: a single directory. Found: "
    , show ( map (toFilePath . dirname) dirs
           , map (toFilePath . filename) files
           )
    ]
  displayException (UnableToExtractArchive url file) = concat
    [ "Error: [S-2040]\n"
    , "Archive extraction failed. Tarballs and zip archives are supported, \
      \couldn't handle the following URL, "
    , T.unpack url
    , " downloaded to the file "
    , toFilePath $ filename file
    ]
  displayException (BadStackVersionException requiredRange) = concat
    [ "Error: [S-1641]\n"
    , "The version of Stack you are using ("
    , show stackVersion
    , ") is outside the required\n"
    ,"version range specified in stack.yaml ("
    , T.unpack (versionRangeText requiredRange)
    , ").\n"
    , "You can upgrade Stack by running:\n\n"
    , "stack upgrade"
    ]
  displayException (NoSuchDirectory dir) = concat
    [ "Error: [S-8773]\n"
    , "No directory could be located matching the supplied path: "
    , dir
    ]
  displayException (ParseGHCVariantException v) = concat
    [ "Error: [S-3938]\n"
    , "Invalid ghc-variant value: "
    , v
    ]
  displayException (BadStackRoot stackRoot) = concat
    [ "Error: [S-8530]\n"
    , "Invalid Stack root: '"
    , toFilePath stackRoot
    , "'. Please provide a valid absolute path."
    ]
  displayException (Won'tCreateStackRootInDirectoryOwnedByDifferentUser envStackRoot parentDir) = concat
    [ "Error: [S-7613]\n"
    , "Preventing creation of Stack root '"
    , toFilePath envStackRoot
    , "'. Parent directory '"
    , toFilePath parentDir
    , "' is owned by someone else."
    ]
  displayException (UserDoesn'tOwnDirectory dir) = concat
    [ "Error: [S-8707]\n"
    , "You are not the owner of '"
    , toFilePath dir
    , "'. Aborting to protect file permissions."
    , "\nRetry with '--"
    , T.unpack configMonoidAllowDifferentUserName
    , "' to disable this precaution."
    ]
  displayException ManualGHCVariantSettingsAreIncompatibleWithSystemGHC = T.unpack $ T.concat
    [ "Error: [S-3605]\n"
    , "Stack can only control the "
    , configMonoidGHCVariantName
    , " of its own GHC installations. Please use '--no-"
    , configMonoidSystemGHCName
    , "'."
    ]
  displayException NixRequiresSystemGhc = T.unpack $ T.concat
    [ "Error: [S-6816]\n"
    , "Stack's Nix integration is incompatible with '--no-system-ghc'. "
    , "Please use '--"
    , configMonoidSystemGHCName
    , "' or disable the Nix integration."
    ]
  displayException NoResolverWhenUsingNoProject =
    "Error: [S-5027]\n"
    ++ "When using the script command, you must provide a resolver argument"
  displayException (NoLTSWithMajorVersion n) = concat
    [ "Error: [S-3803]\n"
    , "No LTS release found with major version "
    , show n
    , "."
    ]
  displayException NoLTSFound =
    "Error: [S-5472]\n"
    ++ "No LTS releases found."

-- | Type representing \'pretty\' exceptions thrown by functions exported by the
-- "Stack.Config" module.
data ConfigPrettyException
  = ParseConfigFileException !(Path Abs File) !ParseException
  | StackWorkEnvNotRelativeDir !String
  | NoMatchingSnapshot !(NonEmpty SnapName)
  | ResolverMismatch !RawSnapshotLocation String
  | ResolverPartial !RawSnapshotLocation !String
  | MultiplePackageIndices [PackageIndexConfig]
  | DuplicateLocalPackageNames ![(PackageName, [PackageLocation])]
  deriving (Show, Typeable)

instance Pretty ConfigPrettyException where
  pretty (ParseConfigFileException configFile exception) =
    "[S-6602]"
    <> line
    <> fillSep
         [ flow "Stack could not load and parse"
         , pretty configFile
         , flow "as a YAML configuraton file."
         ]
    <> blankLine
    <> flow "While loading and parsing, Stack encountered the following \
            \error:"
    <> blankLine
    <> string (Yaml.prettyPrintParseException exception)
    <> blankLine
    <> fillSep
         [ flow "For help about the content of Stack's YAML configuration \
                \files, see (for the most recent release of Stack)"
         ,    style
                Url
                "http://docs.haskellstack.org/en/stable/yaml_configuration/"
           <> "."
         ]
  pretty (StackWorkEnvNotRelativeDir x) =
    "[S-7462]"
    <> line
    <> flow "Stack failed to interpret the value of the STACK_WORK \
            \environment variable as a valid relative path to a directory. \
            \Stack will not accept an absolute path. A path containing a \
            \.. (parent directory) component is not valid."
    <> blankLine
    <> fillSep
         [ flow "If set, Stack expects the value to identify the location \
                \of Stack's work directory, relative to the root directory \
                \of the project or package. Stack encountered the value:"
         , style Error (fromString x) <> "."
         ]
  pretty (NoMatchingSnapshot names) =
    "[S-1833]"
    <> line
    <> flow "None of the following snapshots provides a compiler matching \
            \your package(s):"
    <> line
    <> bulletedList (map (fromString . show) (NonEmpty.toList names))
    <> blankLine
    <> resolveOptions
  pretty (ResolverMismatch resolver errDesc) =
    "[S-6395]"
    <> line
    <> fillSep
         [ "Snapshot"
         , style Url (pretty $ PrettyRawSnapshotLocation resolver)
         , flow "does not have a matching compiler to build some or all of \
                \your package(s)."
         ]
    <> blankLine
    <> indent 4 (string errDesc)
    <> line
    <> resolveOptions
  pretty (ResolverPartial resolver errDesc) =
    "[S-2422]"
    <> line
    <> fillSep
         [ "Snapshot"
         , style Url (pretty $ PrettyRawSnapshotLocation resolver)
         , flow "does not have all the packages to match your requirements."
         ]
    <> blankLine
    <> indent 4 (string errDesc)
    <> line
    <> resolveOptions
  pretty (MultiplePackageIndices pics) =
    "[S-3251]"
    <> line
    <> fillSep
         [ flow "When using the"
         , style Shell "package-indices"
         , flow "key to override the default package index, you must \
                \provide exactly one value, received:"
         , bulletedList (map (string . show) pics)
         ]
    <> blankLine
    <> packageIndicesWarning
  pretty (DuplicateLocalPackageNames pairs) =
    "[S-5470]"
    <> line
    <> fillSep
        [ flow "The same package name is used in more than one local package or"
        , style Shell "extra-deps" <> "."
        ]
    <> mconcat (map go pairs)
   where
    go (name, dirs) =
         blankLine
      <> fillSep
           [ style Error (fromString $ packageNameString name)
           , flow "used in:"
           ]
      <> line
      <> bulletedList (map (fromString . T.unpack . textDisplay) dirs)

instance Exception ConfigPrettyException

data ParseAbsolutePathException
  = ParseAbsolutePathException String String
  deriving (Show, Typeable)

instance Exception ParseAbsolutePathException where
  displayException (ParseAbsolutePathException envVar dir) = concat
    [ "Error: [S-9437]\n"
    , "Failed to parse "
    , envVar
    , " environment variable (expected absolute directory): "
    , dir
    ]

-- | The top-level Stackage configuration.
data Config = Config
  { configWorkDir             :: !(Path Rel Dir)
    -- ^ this allows to override .stack-work directory
  , configUserConfigPath      :: !(Path Abs File)
    -- ^ Path to user configuration file (usually ~/.stack/config.yaml)
  , configBuild               :: !BuildOpts
    -- ^ Build configuration
  , configDocker              :: !DockerOpts
    -- ^ Docker configuration
  , configNix                 :: !NixOpts
    -- ^ Execution environment (e.g nix-shell) configuration
  , configProcessContextSettings :: !(EnvSettings -> IO ProcessContext)
    -- ^ Environment variables to be passed to external tools
  , configLocalProgramsBase   :: !(Path Abs Dir)
    -- ^ Non-platform-specific path containing local installations
  , configLocalPrograms       :: !(Path Abs Dir)
    -- ^ Path containing local installations (mainly GHC)
  , configHideTHLoading       :: !Bool
    -- ^ Hide the Template Haskell "Loading package ..." messages from the
    -- console
  , configPrefixTimestamps    :: !Bool
    -- ^ Prefix build output with timestamps for each line.
  , configPlatform            :: !Platform
    -- ^ The platform we're building for, used in many directory names
  , configPlatformVariant     :: !PlatformVariant
    -- ^ Variant of the platform, also used in directory names
  , configGHCVariant          :: !(Maybe GHCVariant)
    -- ^ The variant of GHC requested by the user.
  , configGHCBuild            :: !(Maybe CompilerBuild)
    -- ^ Override build of the compiler distribution (e.g. standard, gmp4,
    -- tinfo6)
  , configLatestSnapshot      :: !Text
    -- ^ URL of a JSON file providing the latest LTS and Nightly snapshots.
  , configSystemGHC           :: !Bool
    -- ^ Should we use the system-installed GHC (on the PATH) if
    -- available? Can be overridden by command line options.
  , configInstallGHC          :: !Bool
    -- ^ Should we automatically install GHC if missing or the wrong
    -- version is available? Can be overridden by command line options.
  , configSkipGHCCheck        :: !Bool
    -- ^ Don't bother checking the GHC version or architecture.
  , configSkipMsys            :: !Bool
    -- ^ On Windows: don't use a sandboxed MSYS
  , configCompilerCheck       :: !VersionCheck
    -- ^ Specifies which versions of the compiler are acceptable.
  , configCompilerRepository  :: !CompilerRepository
    -- ^ Specifies the repository containing the compiler sources
  , configLocalBin            :: !(Path Abs Dir)
    -- ^ Directory we should install executables into
  , configRequireStackVersion :: !VersionRange
    -- ^ Require a version of Stack within this range.
  , configJobs                :: !Int
    -- ^ How many concurrent jobs to run, defaults to number of capabilities
  , configOverrideGccPath     :: !(Maybe (Path Abs File))
    -- ^ Optional gcc override path
  , configExtraIncludeDirs    :: ![FilePath]
    -- ^ --extra-include-dirs arguments
  , configExtraLibDirs        :: ![FilePath]
    -- ^ --extra-lib-dirs arguments
  , configCustomPreprocessorExts :: ![Text]
    -- ^ List of custom preprocessors to complete the hard coded ones
  , configConcurrentTests     :: !Bool
    -- ^ Run test suites concurrently
  , configTemplateParams      :: !(Map Text Text)
    -- ^ Parameters for templates.
  , configScmInit             :: !(Maybe SCM)
    -- ^ Initialize SCM (e.g. git) when creating new projects.
  , configGhcOptionsByName    :: !(Map PackageName [Text])
    -- ^ Additional GHC options to apply to specific packages.
  , configGhcOptionsByCat     :: !(Map ApplyGhcOptions [Text])
    -- ^ Additional GHC options to apply to categories of packages
  , configCabalConfigOpts     :: !(Map CabalConfigKey [Text])
    -- ^ Additional options to be passed to ./Setup.hs configure
  , configSetupInfoLocations  :: ![String]
    -- ^ URLs or paths to stack-setup.yaml files, for finding tools.
    -- If none present, the default setup-info is used.
  , configSetupInfoInline     :: !SetupInfo
    -- ^ Additional SetupInfo to use to find tools.
  , configPvpBounds           :: !PvpBounds
    -- ^ How PVP upper bounds should be added to packages
  , configModifyCodePage      :: !Bool
    -- ^ Force the code page to UTF-8 on Windows
  , configRebuildGhcOptions   :: !Bool
    -- ^ Rebuild on GHC options changes
  , configApplyGhcOptions     :: !ApplyGhcOptions
    -- ^ Which packages to ghc-options on the command line apply to?
  , configAllowNewer          :: !Bool
    -- ^ Ignore version ranges in .cabal files. Funny naming chosen to
    -- match cabal.
  , configAllowNewerDeps      :: !(Maybe [PackageName])
    -- ^ Ignore dependency upper and lower bounds only for specified
    -- packages. No effect unless allow-newer is enabled.
  , configDefaultTemplate     :: !(Maybe TemplateName)
    -- ^ The default template to use when none is specified.
    -- (If Nothing, the 'default' default template is used.)
  , configAllowDifferentUser  :: !Bool
    -- ^ Allow users other than the Stack root owner to use the Stack
    -- installation.
  , configDumpLogs            :: !DumpLogs
    -- ^ Dump logs of local non-dependencies when doing a build.
  , configProject             :: !(ProjectConfig (Project, Path Abs File))
    -- ^ Project information and stack.yaml file location
  , configAllowLocals         :: !Bool
    -- ^ Are we allowed to build local packages? The script
    -- command disallows this.
  , configSaveHackageCreds    :: !Bool
    -- ^ Should we save Hackage credentials to a file?
  , configHackageBaseUrl      :: !Text
    -- ^ Hackage base URL used when uploading packages
  , configRunner              :: !Runner
  , configPantryConfig        :: !PantryConfig
  , configStackRoot           :: !(Path Abs Dir)
  , configResolver            :: !(Maybe AbstractResolver)
    -- ^ Any resolver override from the command line
  , configUserStorage         :: !UserStorage
    -- ^ Database connection pool for user Stack database
  , configHideSourcePaths     :: !Bool
    -- ^ Enable GHC hiding source paths?
  , configRecommendUpgrade    :: !Bool
    -- ^ Recommend a Stack upgrade?
  , configNoRunCompile   :: !Bool
    -- ^ Use --no-run and --compile options when using `stack script`
  , configStackDeveloperMode  :: !Bool
    -- ^ Turn on Stack developer mode for additional messages?
  }

-- | Class for environment values which have a GHCVariant
class HasGHCVariant env where
  ghcVariantL :: SimpleGetter env GHCVariant
  default ghcVariantL :: HasConfig env => SimpleGetter env GHCVariant
  ghcVariantL = configL.ghcVariantL
  {-# INLINE ghcVariantL #-}

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
  deriving (Eq, Ord, Show)

type AddCommand =
  ExceptT (RIO Runner ())
          (Writer (OA.Mod OA.CommandFields (RIO Runner (), GlobalOptsMonoid)))
          ()


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
  { globalMonoidReExecVersion :: !(First String)
    -- ^ Expected re-exec in container version
  , globalMonoidDockerEntrypoint :: !(First DockerEntrypoint)
    -- ^ Data used when Stack is acting as a Docker entrypoint (internal use
    -- only)
  , globalMonoidLogLevel     :: !(First LogLevel)
    -- ^ Log level
  , globalMonoidTimeInLog    :: !FirstTrue
    -- ^ Whether to include timings in logs.
  , globalMonoidRSLInLog     :: !FirstFalse
    -- ^ Whether to include raw snapshot layer (RSL) in logs.
  , globalMonoidConfigMonoid :: !ConfigMonoid
    -- ^ Config monoid, for passing into 'loadConfig'
  , globalMonoidResolver     :: !(First (Unresolved AbstractResolver))
    -- ^ Resolver override
  , globalMonoidResolverRoot :: !(First FilePath)
    -- ^ root directory for resolver relative path
  , globalMonoidCompiler     :: !(First WantedCompiler)
    -- ^ Compiler override
  , globalMonoidTerminal     :: !(First Bool)
    -- ^ We're in a terminal?
  , globalMonoidStyles       :: !StylesUpdate
    -- ^ Stack's output styles
  , globalMonoidTermWidth    :: !(First Int)
    -- ^ Terminal width override
  , globalMonoidStackYaml    :: !(First FilePath)
    -- ^ Override project stack.yaml
  , globalMonoidLockFileBehavior :: !(First LockFileBehavior)
    -- ^ See 'globalLockFileBehavior'
  }
  deriving Generic

instance Semigroup GlobalOptsMonoid where
  (<>) = mappenddefault

instance Monoid GlobalOptsMonoid where
  mempty = memptydefault
  mappend = (<>)

-- | Default logging level should be something useful but not crazy.
defaultLogLevel :: LogLevel
defaultLogLevel = LevelInfo

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
    -- ^ Dependencies defined within the stack.yaml file, to be applied on top
    -- of the snapshot.
  , projectFlags :: !(Map PackageName (Map FlagName Bool))
    -- ^ Flags to be applied on top of the snapshot flags.
  , projectResolver :: !RawSnapshotLocation
    -- ^ How we resolve which @Snapshot@ to use
  , projectCompiler :: !(Maybe WantedCompiler)
    -- ^ Override the compiler in 'projectResolver'
  , projectExtraPackageDBs :: ![FilePath]
  , projectCurator :: !(Maybe Curator)
    -- ^ Extra configuration intended exclusively for usage by the curator tool.
    -- In other words, this is /not/ part of the documented and exposed Stack
    -- API. SUBJECT TO CHANGE.
  , projectDropPackages :: !(Set PackageName)
    -- ^ Packages to drop from the 'projectResolver'.
  }
  deriving Show

instance ToJSON Project where
  -- Expanding the constructor fully to ensure we don't miss any fields.
  toJSON (Project userMsg packages extraDeps flags resolver mcompiler extraPackageDBs mcurator drops) = object $ concat
    [ maybe [] (\cv -> ["compiler" .= cv]) mcompiler
    , maybe [] (\msg -> ["user-message" .= msg]) userMsg
    , [ "extra-package-dbs" .= extraPackageDBs | not (null extraPackageDBs) ]
    , [ "extra-deps" .= extraDeps | not (null extraDeps) ]
    , [ "flags" .= fmap toCabalStringMap (toCabalStringMap flags)
      | not (Map.null flags)
      ]
    , ["packages" .= packages]
    , ["resolver" .= resolver]
    , maybe [] (\c -> ["curator" .= c]) mcurator
    , [ "drop-packages" .= Set.map CabalString drops | not (Set.null drops) ]
    ]

-- | Extra configuration intended exclusively for usage by the curator tool. In
-- other words, this is /not/ part of the documented and exposed Stack API.
-- SUBJECT TO CHANGE.
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
    , "expect-benchmark-failure" .=
        Set.map CabalString (curatorExpectTestFailure c)
    , "skip-haddock" .= Set.map CabalString (curatorSkipHaddock c)
    , "expect-test-failure" .=
        Set.map CabalString (curatorExpectHaddockFailure c)
    ]

instance FromJSON (WithJSONWarnings Curator) where
  parseJSON = withObjectWarnings "Curator" $ \o -> Curator
    <$> fmap (Set.map unCabalString) (o ..:? "skip-test" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "expect-test-failure" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "skip-bench" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "expect-benchmark-failure" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "skip-haddock" ..!= mempty)
    <*> fmap (Set.map unCabalString) (o ..:? "expect-haddock-failure" ..!= mempty)

parseConfigMonoid ::
     Path Abs Dir
  -> Value
  -> Yaml.Parser (WithJSONWarnings ConfigMonoid)
parseConfigMonoid = withObjectWarnings "ConfigMonoid" . parseConfigMonoidObject

-- | Parse a partial configuration.  Used both to parse both a standalone config
-- file and a project file, so that a sub-parser is not required, which would
-- interfere with warnings for missing fields.
parseConfigMonoidObject :: Path Abs Dir -> Object -> WarningParser ConfigMonoid
parseConfigMonoidObject rootDir obj = do
  -- Parsing 'stackRoot' from 'stackRoot'/config.yaml would be nonsensical
  let configMonoidStackRoot = First Nothing
  configMonoidWorkDir <- First <$> obj ..:? configMonoidWorkDirName
  configMonoidBuildOpts <-
    jsonSubWarnings (obj ..:? configMonoidBuildOptsName ..!= mempty)
  configMonoidDockerOpts <-
    jsonSubWarnings (obj ..:? configMonoidDockerOptsName ..!= mempty)
  configMonoidNixOpts <-
    jsonSubWarnings (obj ..:? configMonoidNixOptsName ..!= mempty)
  configMonoidConnectionCount <-
    First <$> obj ..:? configMonoidConnectionCountName
  configMonoidHideTHLoading <-
    FirstTrue <$> obj ..:? configMonoidHideTHLoadingName
  configMonoidPrefixTimestamps <-
    First <$> obj ..:? configMonoidPrefixTimestampsName

  murls :: Maybe Value <- obj ..:? configMonoidUrlsName
  configMonoidLatestSnapshot <-
    case murls of
      Nothing -> pure $ First Nothing
      Just urls -> jsonSubWarnings $ lift $ withObjectWarnings
        "urls"
        (\o -> First <$> o ..:? "latest-snapshot" :: WarningParser (First Text))
        urls

  configMonoidPackageIndex <-
    First <$> jsonSubWarningsT (obj ..:?  configMonoidPackageIndexName)
  configMonoidPackageIndices <-
    First <$> jsonSubWarningsTT (obj ..:?  configMonoidPackageIndicesName)
  configMonoidSystemGHC <- First <$> obj ..:? configMonoidSystemGHCName
  configMonoidInstallGHC <- FirstTrue <$> obj ..:? configMonoidInstallGHCName
  configMonoidSkipGHCCheck <-
    FirstFalse <$> obj ..:? configMonoidSkipGHCCheckName
  configMonoidSkipMsys <- FirstFalse <$> obj ..:? configMonoidSkipMsysName
  configMonoidRequireStackVersion <-
    IntersectingVersionRange . unVersionRangeJSON <$>
      ( obj ..:? configMonoidRequireStackVersionName
          ..!= VersionRangeJSON anyVersion
      )
  configMonoidArch <- First <$> obj ..:? configMonoidArchName
  configMonoidGHCVariant <- First <$> obj ..:? configMonoidGHCVariantName
  configMonoidGHCBuild <- First <$> obj ..:? configMonoidGHCBuildName
  configMonoidJobs <- First <$> obj ..:? configMonoidJobsName
  configMonoidExtraIncludeDirs <- map (toFilePath rootDir FilePath.</>) <$>
    obj ..:?  configMonoidExtraIncludeDirsName ..!= []
  configMonoidExtraLibDirs <- map (toFilePath rootDir FilePath.</>) <$>
    obj ..:?  configMonoidExtraLibDirsName ..!= []
  configMonoidCustomPreprocessorExts <-
    obj ..:?  configMonoidCustomPreprocessorExtsName ..!= []
  configMonoidOverrideGccPath <-
    First <$> obj ..:? configMonoidOverrideGccPathName
  configMonoidOverrideHpack <-
    First <$> obj ..:? configMonoidOverrideHpackName
  configMonoidConcurrentTests <-
    First <$> obj ..:? configMonoidConcurrentTestsName
  configMonoidLocalBinPath <- First <$> obj ..:? configMonoidLocalBinPathName
  templates <- obj ..:? "templates"
  (configMonoidScmInit,configMonoidTemplateParameters) <-
    case templates of
      Nothing -> pure (First Nothing,M.empty)
      Just tobj -> do
        scmInit <- tobj ..:? configMonoidScmInitName
        params <- tobj ..:? configMonoidTemplateParametersName
        pure (First scmInit,fromMaybe M.empty params)
  configMonoidCompilerCheck <-
    First <$> obj ..:? configMonoidCompilerCheckName
  configMonoidCompilerRepository <-
    First <$> (obj ..:? configMonoidCompilerRepositoryName)

  options <-
    Map.map unGhcOptions <$> obj ..:? configMonoidGhcOptionsName ..!= mempty

  optionsEverything <-
    case (Map.lookup GOKOldEverything options, Map.lookup GOKEverything options) of
      (Just _, Just _) ->
        fail "Cannot specify both `*` and `$everything` GHC options"
      (Nothing, Just x) -> pure x
      (Just x, Nothing) -> do
        tell "The `*` ghc-options key is not recommended. Consider using \
             \$locals, or if really needed, $everything"
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
  let configMonoidCabalConfigOpts =
        coerce (configMonoidCabalConfigOpts' :: Map CabalConfigKey [Text])

  configMonoidExtraPath <- obj ..:? configMonoidExtraPathName ..!= []
  configMonoidSetupInfoLocations <-
    obj ..:? configMonoidSetupInfoLocationsName ..!= []
  configMonoidSetupInfoInline <-
    jsonSubWarningsT (obj ..:? configMonoidSetupInfoInlineName) ..!= mempty
  configMonoidLocalProgramsBase <-
    First <$> obj ..:? configMonoidLocalProgramsBaseName
  configMonoidPvpBounds <- First <$> obj ..:? configMonoidPvpBoundsName
  configMonoidModifyCodePage <-
    FirstTrue <$> obj ..:? configMonoidModifyCodePageName
  configMonoidRebuildGhcOptions <-
    FirstFalse <$> obj ..:? configMonoidRebuildGhcOptionsName
  configMonoidApplyGhcOptions <-
    First <$> obj ..:? configMonoidApplyGhcOptionsName
  configMonoidAllowNewer <- First <$> obj ..:? configMonoidAllowNewerName
  configMonoidAllowNewerDeps <- obj ..:? configMonoidAllowNewerDepsName
  configMonoidDefaultTemplate <-
    First <$> obj ..:? configMonoidDefaultTemplateName
  configMonoidAllowDifferentUser <-
    First <$> obj ..:? configMonoidAllowDifferentUserName
  configMonoidDumpLogs <- First <$> obj ..:? configMonoidDumpLogsName
  configMonoidSaveHackageCreds <-
    First <$> obj ..:? configMonoidSaveHackageCredsName
  configMonoidHackageBaseUrl <-
    First <$> obj ..:? configMonoidHackageBaseUrlName

  configMonoidColorWhenUS <- obj ..:? configMonoidColorWhenUSName
  configMonoidColorWhenGB <- obj ..:? configMonoidColorWhenGBName
  let configMonoidColorWhen =  First $   configMonoidColorWhenUS
                                     <|> configMonoidColorWhenGB

  configMonoidStylesUS <- obj ..:? configMonoidStylesUSName
  configMonoidStylesGB <- obj ..:? configMonoidStylesGBName
  let configMonoidStyles = fromMaybe mempty $   configMonoidStylesUS
                                            <|> configMonoidStylesGB

  configMonoidHideSourcePaths <-
    FirstTrue <$> obj ..:? configMonoidHideSourcePathsName
  configMonoidRecommendUpgrade <-
    FirstTrue <$> obj ..:? configMonoidRecommendUpgradeName

  configMonoidCasaRepoPrefix <-
    First <$> obj ..:? configMonoidCasaRepoPrefixName
  configMonoidSnapshotLocation <-
    First <$> obj ..:? configMonoidSnapshotLocationName
  configMonoidNoRunCompile <-
    FirstFalse <$> obj ..:? configMonoidNoRunCompileName

  configMonoidStackDeveloperMode <-
    First <$> obj ..:? configMonoidStackDeveloperModeName

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

configMonoidPackageIndexName :: Text
configMonoidPackageIndexName = "package-index"

-- Deprecated in favour of package-index
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

configMonoidAllowNewerDepsName :: Text
configMonoidAllowNewerDepsName = "allow-newer-deps"

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

packageIndicesWarning :: StyleDoc
packageIndicesWarning =
  fillSep
    [ "The"
    , style Shell "package-indices"
    , flow "key is deprecated in favour of"
    , style Shell "package-index" <> "."
    ]

resolveOptions :: StyleDoc
resolveOptions =
     flow "This may be resolved by:"
  <> line
  <> bulletedList
       [ fillSep
           [ "Using"
           , style Shell "--omit-packages"
           , "to exclude mismatching package(s)."
           ]
       , fillSep
           [ "Using"
           , style Shell "--resolver"
           , "to specify a matching snapshot/resolver."
           ]
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
platformOnlyRelDir ::
     (MonadReader env m, HasPlatform env, MonadThrow m)
  => m (Path Rel Dir)
platformOnlyRelDir = do
  platform <- view platformL
  platformVariant <- view platformVariantL
  parseRelDir
    (  Distribution.Text.display platform
    ++ platformVariantSuffix platformVariant
    )

-- | Directory containing snapshots
snapshotsDir ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => m (Path Abs Dir)
snapshotsDir = do
  root <- view stackRootL
  platform <- platformGhcRelDir
  pure $ root </> relDirSnapshots </> platform

-- | Installation root for dependencies
installationRootDeps :: HasEnvConfig env => RIO env (Path Abs Dir)
installationRootDeps = do
  root <- view stackRootL
  -- TODO: also useShaPathOnWindows here, once #1173 is resolved.
  psc <- platformSnapAndCompilerRel
  pure $ root </> relDirSnapshots </> psc

-- | Installation root for locals
installationRootLocal :: HasEnvConfig env => RIO env (Path Abs Dir)
installationRootLocal = do
  workDir <- getProjectWorkDir
  psc <- useShaPathOnWindows =<< platformSnapAndCompilerRel
  pure $ workDir </> relDirInstall </> psc

-- | Installation root for compiler tools
bindirCompilerTools ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => m (Path Abs Dir)
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
hoogleRoot :: HasEnvConfig env => RIO env (Path Abs Dir)
hoogleRoot = do
  workDir <- getProjectWorkDir
  psc <- useShaPathOnWindows =<< platformSnapAndCompilerRel
  pure $ workDir </> relDirHoogle </> psc

-- | Get the hoogle database path.
hoogleDatabasePath :: HasEnvConfig env => RIO env (Path Abs File)
hoogleDatabasePath = do
  dir <- hoogleRoot
  pure (dir </> relFileDatabaseHoo)

-- | Path for platform followed by snapshot name followed by compiler
-- name.
platformSnapAndCompilerRel :: HasEnvConfig env => RIO env (Path Rel Dir)
platformSnapAndCompilerRel = do
  platform <- platformGhcRelDir
  smh <- view $ envConfigL.to envConfigSourceMapHash
  name <- smRelDir smh
  ghc <- compilerVersionDir
  useShaPathOnWindows (platform </> name </> ghc)

-- | Relative directory for the platform and GHC identifier
platformGhcRelDir ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => m (Path Rel Dir)
platformGhcRelDir = do
  cp <- view compilerPathsL
  let cbSuffix = compilerBuildSuffix $ cpBuild cp
  verOnly <- platformGhcVerOnlyRelDirStr
  parseRelDir (mconcat [ verOnly, cbSuffix ])

-- | Relative directory for the platform and GHC identifier without GHC bindist
-- build
platformGhcVerOnlyRelDir ::
     (HasGHCVariant env, HasPlatform env, MonadReader env m, MonadThrow m)
  => m (Path Rel Dir)
platformGhcVerOnlyRelDir =
  parseRelDir =<< platformGhcVerOnlyRelDirStr

-- | Relative directory for the platform and GHC identifier without GHC bindist
-- build (before parsing into a Path)
platformGhcVerOnlyRelDirStr ::
     (HasGHCVariant env, HasPlatform env, MonadReader env m)
  => m FilePath
platformGhcVerOnlyRelDirStr = do
  platform <- view platformL
  platformVariant <- view platformVariantL
  ghcVariant <- view ghcVariantL
  pure $ mconcat [ Distribution.Text.display platform
                   , platformVariantSuffix platformVariant
                   , ghcVariantSuffix ghcVariant ]

-- | This is an attempt to shorten Stack paths on Windows to decrease our
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

instance IsPath Abs Dir where
  parsePath = parseAbsDir

instance IsPath Rel Dir where
  parsePath = parseRelDir

instance IsPath Abs File where
  parsePath = parseAbsFile

instance IsPath Rel File where
  parsePath = parseRelFile

compilerVersionDir ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => m (Path Rel Dir)
compilerVersionDir = do
  compilerVersion <- view actualCompilerVersionL
  parseRelDir $ case compilerVersion of
    ACGhc version -> versionString version
    ACGhcGit {} -> compilerVersionString compilerVersion

-- | Package database for installing dependencies into
packageDatabaseDeps :: HasEnvConfig env => RIO env (Path Abs Dir)
packageDatabaseDeps = do
  root <- installationRootDeps
  pure $ root </> relDirPkgdb

-- | Package database for installing local packages into
packageDatabaseLocal :: HasEnvConfig env => RIO env (Path Abs Dir)
packageDatabaseLocal = do
  root <- installationRootLocal
  pure $ root </> relDirPkgdb

-- | Extra package databases
packageDatabaseExtra ::
     (HasEnvConfig env, MonadReader env m)
  => m [Path Abs Dir]
packageDatabaseExtra = view $ buildConfigL.to bcExtraPackageDBs

-- | Where do we get information on global packages for loading up a
-- 'LoadedSnapshot'?
data GlobalInfoSource
  = GISSnapshotHints
    -- ^ Accept the hints in the snapshot definition
  | GISCompiler ActualCompiler
    -- ^ Look up the actual information in the installed compiler

-- | Where HPC reports and tix files get stored.
hpcReportDir :: HasEnvConfig env => RIO env (Path Abs Dir)
hpcReportDir = do
  root <- installationRootLocal
  pure $ root </> relDirHpc

-- | Get the extra bin directories (for the PATH). Puts more local first
--
-- Bool indicates whether or not to include the locals
extraBinDirs :: HasEnvConfig env => RIO env (Bool -> [Path Abs Dir])
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

parseProjectAndConfigMonoid ::
     Path Abs Dir
  -> Value
  -> Yaml.Parser (WithJSONWarnings (IO ProjectAndConfigMonoid))
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
            , projectDependencies =
                concatMap toList (deps' :: [NonEmpty RawPackageLocation])
            , projectFlags = flags
            , projectCurator = mcurator
            , projectDropPackages = Set.map unCabalString drops
            }
      pure $ ProjectAndConfigMonoid project config

-- | A variant of the platform, used to differentiate Docker builds from host
data PlatformVariant
  = PlatformVariantNone
  | PlatformVariant String

-- | Render a platform variant to a String suffix.
platformVariantSuffix :: PlatformVariant -> String
platformVariantSuffix PlatformVariantNone = ""
platformVariantSuffix (PlatformVariant v) = "-" ++ v

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
  fromJSONKeyList =
    FromJSONKeyTextParser $ \_ -> fail "GhcOptionKey.fromJSONKeyList"

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

-- | Class for environment values that can provide a 'Config'.
class ( HasPlatform env
      , HasGHCVariant env
      , HasProcessContext env
      , HasPantryConfig env
      , HasTerm env
      , HasRunner env
      ) => HasConfig env where
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

instance HasPlatform (Platform, PlatformVariant) where
  platformL = _1
  platformVariantL = _2

instance HasPlatform Config where
  platformL = lens configPlatform (\x y -> x { configPlatform = y })
  platformVariantL =
    lens configPlatformVariant (\x y -> x { configPlatformVariant = y })

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

stackGlobalConfigL :: HasConfig s => Lens' s (Path Abs File)
stackGlobalConfigL =
  configL.lens configUserConfigPath (\x y -> x { configUserConfigPath = y })

-- | The compiler specified by the @SnapshotDef@. This may be different from the
-- actual compiler used!
wantedCompilerVersionL :: HasBuildConfig s => Getting r s WantedCompiler
wantedCompilerVersionL = buildConfigL.to (smwCompiler . bcSMWanted)

-- | Location of the ghc-pkg executable
newtype GhcPkgExe
  = GhcPkgExe (Path Abs File)
  deriving Show

-- | Get the 'GhcPkgExe' from a 'HasCompiler' environment
getGhcPkgExe :: HasCompiler env => RIO env GhcPkgExe
getGhcPkgExe = view $ compilerPathsL.to cpPkg

-- | Type representing dump information for a single package, as output by the
-- @ghc-pkg describe@ command.
data DumpPackage = DumpPackage
  { dpGhcPkgId :: !GhcPkgId
    -- ^ The @id@ field.
  , dpPackageIdent :: !PackageIdentifier
    -- ^ The @name@ and @version@ fields. The @name@ field is the munged package
    -- name. If the package is not for a sub library, its munged name is its
    -- name.
  , dpParentLibIdent :: !(Maybe PackageIdentifier)
    -- ^ The @package-name@ and @version@ fields, if @package-name@ is present.
    -- That field is present if the package is for a sub library.
  , dpLicense :: !(Maybe C.License)
  , dpLibDirs :: ![FilePath]
    -- ^ The @library-dirs@ field.
  , dpLibraries :: ![Text]
    -- ^ The @hs-libraries@ field.
  , dpHasExposedModules :: !Bool
  , dpExposedModules :: !(Set ModuleName)
  , dpDepends :: ![GhcPkgId]
    -- ^ The @depends@ field (packages on which this package depends).
  , dpHaddockInterfaces :: ![FilePath]
  , dpHaddockHtml :: !(Maybe FilePath)
  , dpIsExposed :: !Bool
  }
  deriving (Eq, Read, Show)

-- | Paths on the filesystem for the compiler we're using
data CompilerPaths = CompilerPaths
  { cpCompilerVersion :: !ActualCompiler
  , cpArch :: !Arch
  , cpBuild :: !CompilerBuild
  , cpCompiler :: !(Path Abs File)
  , cpPkg :: !GhcPkgExe
    -- ^ ghc-pkg or equivalent
  , cpInterpreter :: !(Path Abs File)
    -- ^ runghc
  , cpHaddock :: !(Path Abs File)
    -- ^ haddock, in 'IO' to allow deferring the lookup
  , cpSandboxed :: !Bool
    -- ^ Is this a Stack-sandboxed installation?
  , cpCabalVersion :: !Version
    -- ^ This is the version of Cabal that Stack will use to compile Setup.hs
    -- files in the build process.
    --
    -- Note that this is not necessarily the same version as the one that Stack
    -- depends on as a library and which is displayed when running
    -- @stack ls dependencies | grep Cabal@ in the Stack project.
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
  }
  deriving (Show, Generic)

instance Semigroup ExtraDirs where
  (<>) = mappenddefault

instance Monoid ExtraDirs where
  mempty = memptydefault
  mappend = (<>)

-- | An environment which ensures that the given compiler is available on the
-- PATH
class HasCompiler env where
  compilerPathsL :: SimpleGetter env CompilerPaths

instance HasCompiler CompilerPaths where
  compilerPathsL = id

class HasSourceMap env where
  sourceMapL :: Lens' env SourceMap

instance HasSourceMap EnvConfig where
  sourceMapL = lens envConfigSourceMap (\x y -> x { envConfigSourceMap = y })

-- | The version of the compiler which will actually be used. May be different
-- than that specified in the 'SnapshotDef' and returned by
-- 'wantedCompilerVersionL'.
actualCompilerVersionL :: HasSourceMap env => SimpleGetter env ActualCompiler
actualCompilerVersionL = sourceMapL.to smCompiler

buildOptsL :: HasConfig s => Lens' s BuildOpts
buildOptsL = configL.lens
  configBuild
  (\x y -> x { configBuild = y })

buildOptsMonoidHaddockL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidHaddockL =
  lens (getFirstFalse . buildMonoidHaddock)
    (\buildMonoid t -> buildMonoid {buildMonoidHaddock = FirstFalse t})

buildOptsMonoidTestsL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidTestsL =
  lens (getFirstFalse . buildMonoidTests)
    (\buildMonoid t -> buildMonoid {buildMonoidTests = FirstFalse t})

buildOptsMonoidBenchmarksL :: Lens' BuildOptsMonoid (Maybe Bool)
buildOptsMonoidBenchmarksL =
  lens (getFirstFalse . buildMonoidBenchmarks)
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

envOverrideSettingsL ::
     HasConfig env
  => Lens' env (EnvSettings -> IO ProcessContext)
envOverrideSettingsL = configL.lens
  configProcessContextSettings
  (\x y -> x { configProcessContextSettings = y })

shouldForceGhcColorFlag ::
     (HasEnvConfig env, HasRunner env)
  => RIO env Bool
shouldForceGhcColorFlag = do
  canDoColor <- (>= mkVersion [8, 2, 1]) . getGhcVersion
            <$> view actualCompilerVersionL
  shouldDoColor <- view useColorL
  pure $ canDoColor && shouldDoColor

appropriateGhcColorFlag ::
     (HasEnvConfig env, HasRunner env)
  => RIO env (Maybe String)
appropriateGhcColorFlag = f <$> shouldForceGhcColorFlag
 where
  f True = Just ghcColorForceFlag
  f False = Nothing

-- | In dev mode, print as a warning, otherwise as debug
prettyStackDevL :: HasConfig env => [StyleDoc] -> RIO env ()
prettyStackDevL docs = do
  config <- view configL
  if configStackDeveloperMode config
    then prettyWarnL docs
    else prettyDebugL docs
