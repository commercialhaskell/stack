{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

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
  , userGlobalConfigFileL
  , stackRootL
  , workDirL
  -- * Helper logging functions
  , prettyStackDevL
  ) where

import           Casa.Client ( CasaRepoPrefix )
import           Distribution.System ( Platform )
import           Path ( (</>), parent, reldir, relfile )
import           RIO.Process ( HasProcessContext (..), ProcessContext )
import           Stack.Prelude
import           Stack.Types.ApplyGhcOptions ( ApplyGhcOptions (..) )
import           Stack.Types.ApplyProgOptions ( ApplyProgOptions (..) )
import           Stack.Types.BuildOpts ( BuildOpts )
import           Stack.Types.CabalConfigKey ( CabalConfigKey )
import           Stack.Types.Compiler ( CompilerRepository )
import           Stack.Types.CompilerBuild ( CompilerBuild )
import           Stack.Types.Docker ( DockerOpts )
import           Stack.Types.DumpLogs ( DumpLogs )
import           Stack.Types.EnvSettings ( EnvSettings )
import           Stack.Types.GHCVariant ( GHCVariant (..), HasGHCVariant (..) )
import           Stack.Types.MsysEnvironment ( MsysEnvironment )
import           Stack.Types.Nix ( NixOpts )
import           Stack.Types.Platform ( HasPlatform (..), PlatformVariant )
import           Stack.Types.Project ( Project (..) )
import           Stack.Types.ProjectConfig ( ProjectConfig (..) )
import           Stack.Types.PvpBounds ( PvpBounds )
import           Stack.Types.Runner ( HasRunner (..), Runner, globalOptsL )
import           Stack.Types.SCM ( SCM )
import           Stack.Types.SetupInfo ( SetupInfo )
import           Stack.Types.Snapshot ( AbstractSnapshot )
import           Stack.Types.Storage ( UserStorage )
import           Stack.Types.TemplateName ( TemplateName )
import           Stack.Types.Version ( VersionCheck (..), VersionRange )

-- | The top-level Stackage configuration.
data Config = Config
  { workDir                :: !(Path Rel Dir)
    -- ^ this allows to override .stack-work directory
  , userGlobalConfigFile   :: !(Path Abs File)
    -- ^ The user-specific global configuration file.
  , build                  :: !BuildOpts
    -- ^ Build configuration
  , docker                 :: !DockerOpts
    -- ^ Docker configuration
  , nix                    :: !NixOpts
    -- ^ Execution environment (e.g nix-shell) configuration
  , processContextSettings :: !(EnvSettings -> IO ProcessContext)
    -- ^ Environment variables to be passed to external tools
  , localProgramsBase      :: !(Path Abs Dir)
    -- ^ Non-platform-specific path containing local installations
  , localPrograms          :: !(Path Abs Dir)
    -- ^ Path containing local installations (mainly GHC)
  , hideTHLoading          :: !Bool
    -- ^ Hide the Template Haskell "Loading package ..." messages from the
    -- console
  , prefixTimestamps       :: !Bool
    -- ^ Prefix build output with timestamps for each line.
  , platform               :: !Platform
    -- ^ The platform we're building for, used in many directory names
  , platformVariant        :: !PlatformVariant
    -- ^ Variant of the platform, also used in directory names
  , ghcVariant             :: !(Maybe GHCVariant)
    -- ^ The variant of GHC requested by the user.
  , ghcBuild               :: !(Maybe CompilerBuild)
    -- ^ Override build of the compiler distribution (e.g. standard, gmp4,
    -- tinfo6)
  , latestSnapshot         :: !Text
    -- ^ URL of a JSON file providing the latest LTS and Nightly snapshots.
  , systemGHC              :: !Bool
    -- ^ Should we use the system-installed GHC (on the PATH) if
    -- available? Can be overridden by command line options.
  , installGHC             :: !Bool
    -- ^ Should we automatically install GHC if missing or the wrong
    -- version is available? Can be overridden by command line options.
  , skipGHCCheck           :: !Bool
    -- ^ Don't bother checking the GHC version or architecture.
  , skipMsys               :: !Bool
    -- ^ On Windows: don't use a sandboxed MSYS
  , msysEnvironment        :: !(Maybe MsysEnvironment)
    -- ^ On Windows: what MSYS2 environment to apply. Nothing on other operating
    -- systems.
  , compilerCheck          :: !VersionCheck
    -- ^ Specifies which versions of the compiler are acceptable.
  , compilerRepository     :: !CompilerRepository
    -- ^ Specifies the repository containing the compiler sources
  , localBin               :: !(Path Abs Dir)
    -- ^ Directory we should install executables into
  , fileWatchHook          :: !(Maybe (Path Abs File))
    -- ^ Optional path of executable used to override --file-watch
    -- post-processing.
  , requireStackVersion    :: !VersionRange
    -- ^ Require a version of Stack within this range.
  , jobs                   :: !Int
    -- ^ How many concurrent jobs to run, defaults to number of capabilities
  , overrideGccPath        :: !(Maybe (Path Abs File))
    -- ^ Optional gcc override path
  , extraIncludeDirs       :: ![FilePath]
    -- ^ --extra-include-dirs arguments
  , extraLibDirs           :: ![FilePath]
    -- ^ --extra-lib-dirs arguments
  , customPreprocessorExts :: ![Text]
    -- ^ List of custom preprocessors to complete the hard coded ones
  , concurrentTests        :: !Bool
    -- ^ Run test suites concurrently
  , templateParams         :: !(Map Text Text)
    -- ^ Parameters for templates.
  , scmInit                :: !(Maybe SCM)
    -- ^ Initialize SCM (e.g. git) when creating new projects.
  , ghcOptionsByName       :: !(Map PackageName [Text])
    -- ^ Additional GHC options to apply to specific packages.
  , ghcOptionsByCat        :: !(Map ApplyGhcOptions [Text])
    -- ^ Additional GHC options to apply to categories of packages
  , cabalConfigOpts        :: !(Map CabalConfigKey [Text])
    -- ^ Additional options to be passed to ./Setup.hs configure
  , setupInfoLocations     :: ![String]
    -- ^ URLs or paths to stack-setup.yaml files, for finding tools.
    -- If none present, the default setup-info is used.
  , setupInfoInline        :: !SetupInfo
    -- ^ Additional SetupInfo to use to find tools.
  , pvpBounds              :: !PvpBounds
    -- ^ How PVP upper bounds should be added to packages
  , modifyCodePage         :: !Bool
    -- ^ Force the code page to UTF-8 on Windows
  , rebuildGhcOptions      :: !Bool
    -- ^ Rebuild on GHC options changes
  , applyGhcOptions        :: !ApplyGhcOptions
    -- ^ Which packages do --ghc-options on the command line apply to?
  , applyProgOptions       :: !ApplyProgOptions
    -- ^ Which packages do all and any --PROG-option options on the command line
    -- apply to?
  , allowNewer             :: !Bool
    -- ^ Ignore version ranges in .cabal files. Funny naming chosen to
    -- match cabal.
  , allowNewerDeps         :: !(Maybe [PackageName])
    -- ^ Ignore dependency upper and lower bounds only for specified
    -- packages. No effect unless allow-newer is enabled.
  , defaultInitSnapshot    :: !(First AbstractSnapshot)
    -- ^ An optional default snapshot to use with @stack init@ when none is
    -- specified at the command line.
  , defaultTemplate        :: !(Maybe TemplateName)
    -- ^ The default template to use when none is specified.
    -- (If Nothing, the 'default' default template is used.)
  , allowDifferentUser     :: !Bool
    -- ^ Allow users other than the Stack root owner to use the Stack
    -- installation.
  , dumpLogs               :: !DumpLogs
    -- ^ Dump logs of local non-dependencies when doing a build.
  , project                :: !(ProjectConfig (Project, Path Abs File))
    -- ^ Project information and stack.yaml file location
  , allowLocals            :: !Bool
    -- ^ Are we allowed to build local packages? The script
    -- command disallows this.
  , saveHackageCreds       :: !FirstTrue
    -- ^ Should we save Hackage credentials to a file?
  , hackageBaseUrl         :: !Text
    -- ^ Hackage base URL used when uploading packages
  , runner                 :: !Runner
  , pantryConfig           :: !PantryConfig
  , stackRoot              :: !(Path Abs Dir)
  , snapshot               :: !(Maybe AbstractSnapshot)
    -- ^ Any snapshot override from the command line
  , userStorage            :: !UserStorage
    -- ^ Database connection pool for user Stack database
  , hideSourcePaths        :: !Bool
    -- ^ Enable GHC hiding source paths?
  , recommendStackUpgrade  :: !Bool
    -- ^ Recommend a Stack upgrade?
  , notifyIfNixOnPath      :: !Bool
    -- ^ Notify if the Nix package manager (nix) is on the PATH, but
    -- Stack's Nix integration is not enabled?
  , notifyIfGhcUntested    :: !Bool
    -- ^ Notify if Stack has not been tested with the GHC version?
  , notifyIfCabalUntested  :: !Bool
    -- ^ Notify if Stack has not been tested with the Cabal version?
  , notifyIfArchUnknown    :: !Bool
    -- ^ Notify if the specified machine architecture is unknown to Cabal (the
    -- library)?
  , noRunCompile           :: !Bool
    -- ^ Use --no-run and --compile options when using `stack script`
  , stackDeveloperMode     :: !Bool
    -- ^ Turn on Stack developer mode for additional messages?
  , casa                   :: !(Maybe (CasaRepoPrefix, Int))
    -- ^ Optional Casa configuration
  }

-- | The project root directory, if in a project.
configProjectRoot :: Config -> Maybe (Path Abs Dir)
configProjectRoot c =
  case c.project of
    PCProject (_, fp) -> Just $ parent fp
    PCGlobalProject -> Nothing
    PCNoProject _deps -> Nothing

-- | Get the URL to request the information on the latest snapshots
askLatestSnapshotUrl :: (MonadReader env m, HasConfig env) => m Text
askLatestSnapshotUrl = view $ configL . to (.latestSnapshot)

-- | @STACK_ROOT\/hooks\/@
hooksDir :: HasConfig env => RIO env (Path Abs Dir)
hooksDir = do
  sr <- view $ configL . to (.stackRoot)
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
  platformL = lens (.platform) (\x y -> x { platform = y })
  platformVariantL =
    lens (.platformVariant) (\x y -> x { platformVariant = y })

instance HasGHCVariant Config where
  ghcVariantL = to $ fromMaybe GHCStandard . (.ghcVariant)

instance HasProcessContext Config where
  processContextL = runnerL . processContextL

instance HasPantryConfig Config where
  pantryConfigL = lens
    (.pantryConfig)
    (\x y -> x { pantryConfig = y })

instance HasConfig Config where
  configL = id
  {-# INLINE configL #-}

instance HasRunner Config where
  runnerL = lens (.runner) (\x y -> x { runner = y })

instance HasLogFunc Config where
  logFuncL = runnerL . logFuncL

instance HasStylesUpdate Config where
  stylesUpdateL = runnerL . stylesUpdateL

instance HasTerm Config where
  useColorL = runnerL . useColorL
  termWidthL = runnerL . termWidthL

-----------------------------------
-- Helper lenses
-----------------------------------

stackRootL :: HasConfig s => Lens' s (Path Abs Dir)
stackRootL =
  configL . lens (.stackRoot) (\x y -> x { stackRoot = y })

userGlobalConfigFileL :: HasConfig s => Lens' s (Path Abs File)
userGlobalConfigFileL = configL . lens
  (.userGlobalConfigFile)
  (\x y -> x { userGlobalConfigFile = y })

buildOptsL :: HasConfig s => Lens' s BuildOpts
buildOptsL = configL . lens (.build) (\x y -> x { build = y })

envOverrideSettingsL ::
     HasConfig env
  => Lens' env (EnvSettings -> IO ProcessContext)
envOverrideSettingsL = configL . lens
  (.processContextSettings)
  (\x y -> x { processContextSettings = y })

-- | @".stack-work"@
workDirL :: HasConfig env => Lens' env (Path Rel Dir)
workDirL = configL . lens (.workDir) (\x y -> x { workDir = y })

-- | In dev mode, print as a warning, otherwise as debug
prettyStackDevL :: HasConfig env => [StyleDoc] -> RIO env ()
prettyStackDevL docs = do
  config <- view configL
  if config.stackDeveloperMode
    then prettyWarnL docs
    else prettyDebugL docs
