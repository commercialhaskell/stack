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

module Stack.Types.Config
  (
    Config (..)
  , HasConfig (..)
  , askLatestSnapshotUrl
  , configProjectRoot
  , ghcInstallHook
  -- * Lens helpers
  , buildOptsL
  , envOverrideSettingsL
  , globalOptsL
  , stackGlobalConfigL
  , stackRootL
  , workDirL
  -- * Helper logging functions
  , prettyStackDevL
  ) where

import           Distribution.System ( Platform )
import           Path ( (</>), parent, reldir, relfile )
import           RIO.Process ( HasProcessContext (..), ProcessContext )
import           Stack.Prelude
import           Stack.Types.ApplyGhcOptions ( ApplyGhcOptions (..) )
import           Stack.Types.BuildOpts ( BuildOpts )
import           Stack.Types.CabalConfigKey ( CabalConfigKey )
import           Stack.Types.Compiler ( CompilerRepository )
import           Stack.Types.CompilerBuild ( CompilerBuild )
import           Stack.Types.Docker ( DockerOpts )
import           Stack.Types.DumpLogs ( DumpLogs )
import           Stack.Types.EnvSettings ( EnvSettings )
import           Stack.Types.GHCVariant ( GHCVariant (..), HasGHCVariant (..) )
import           Stack.Types.Nix ( NixOpts )
import           Stack.Types.Platform ( HasPlatform (..), PlatformVariant )
import           Stack.Types.Project ( Project (..) )
import           Stack.Types.ProjectConfig ( ProjectConfig (..) )
import           Stack.Types.PvpBounds ( PvpBounds )
import           Stack.Types.Resolver ( AbstractResolver )
import           Stack.Types.Runner ( HasRunner (..), Runner, globalOptsL )
import           Stack.Types.SCM ( SCM )
import           Stack.Types.SetupInfo ( SetupInfo )
import           Stack.Types.Storage ( UserStorage )
import           Stack.Types.TemplateName ( TemplateName )
import           Stack.Types.Version ( VersionCheck (..), VersionRange )

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

-- | The project root directory, if in a project.
configProjectRoot :: Config -> Maybe (Path Abs Dir)
configProjectRoot c =
  case configProject c of
    PCProject (_, fp) -> Just $ parent fp
    PCGlobalProject -> Nothing
    PCNoProject _deps -> Nothing

-- | Get the URL to request the information on the latest snapshots
askLatestSnapshotUrl :: (MonadReader env m, HasConfig env) => m Text
askLatestSnapshotUrl = view $ configL.to configLatestSnapshot

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

-----------------------------------
-- Lens classes
-----------------------------------

-- | Class for environment values that can provide a 'Config'.
class ( HasPlatform env
      , HasGHCVariant env
      , HasProcessContext env
      , HasPantryConfig env
      , HasTerm env
      , HasRunner env
      ) => HasConfig env where
  configL :: Lens' env Config

-----------------------------------
-- Lens instances
-----------------------------------

instance HasPlatform Config where
  platformL = lens configPlatform (\x y -> x { configPlatform = y })
  platformVariantL =
    lens configPlatformVariant (\x y -> x { configPlatformVariant = y })

instance HasGHCVariant Config where
  ghcVariantL = to $ fromMaybe GHCStandard . configGHCVariant

instance HasProcessContext Config where
  processContextL = runnerL.processContextL

instance HasPantryConfig Config where
  pantryConfigL = lens configPantryConfig (\x y -> x { configPantryConfig = y })

instance HasConfig Config where
  configL = id
  {-# INLINE configL #-}

instance HasRunner Config where
  runnerL = lens configRunner (\x y -> x { configRunner = y })

instance HasLogFunc Config where
  logFuncL = runnerL.logFuncL

instance HasStylesUpdate Config where
  stylesUpdateL = runnerL.stylesUpdateL

instance HasTerm Config where
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

buildOptsL :: HasConfig s => Lens' s BuildOpts
buildOptsL = configL.lens
  configBuild
  (\x y -> x { configBuild = y })

envOverrideSettingsL ::
     HasConfig env
  => Lens' env (EnvSettings -> IO ProcessContext)
envOverrideSettingsL = configL.lens
  configProcessContextSettings
  (\x y -> x { configProcessContextSettings = y })

-- | @".stack-work"@
workDirL :: HasConfig env => Lens' env (Path Rel Dir)
workDirL = configL.lens configWorkDir (\x y -> x { configWorkDir = y })

-- | In dev mode, print as a warning, otherwise as debug
prettyStackDevL :: HasConfig env => [StyleDoc] -> RIO env ()
prettyStackDevL docs = do
  config <- view configL
  if configStackDeveloperMode config
    then prettyWarnL docs
    else prettyDebugL docs
