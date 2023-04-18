{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.ConfigMonoid
  ( ConfigMonoid (..)
  ) where

import qualified Data.Monoid as Monoid
import           Data.Monoid.Map ( MonoidMap (..) )
import           Casa.Client ( CasaRepoPrefix )
import           Generics.Deriving.Monoid ( mappenddefault, memptydefault )
import           Stack.Prelude
import           Stack.Types.AllowNewerDeps ( AllowNewerDeps )
import           Stack.Types.ApplyGhcOptions ( ApplyGhcOptions )
import           Stack.Types.CabalConfigKey ( CabalConfigKey )
import           Stack.Types.ColorWhen ( ColorWhen )
import           Stack.Types.Compiler ( CompilerRepository )
import           Stack.Types.CompilerBuild ( CompilerBuild )
import           Stack.Types.Config.Build ( BuildOptsMonoid )
import           Stack.Types.Docker ( DockerOptsMonoid )
import           Stack.Types.DumpLogs ( DumpLogs )
import           Stack.Types.GHCVariant ( GHCVariant )
import           Stack.Types.Nix ( NixOptsMonoid )
import           Stack.Types.PvpBounds ( PvpBounds )
import           Stack.Types.SCM ( SCM )
import           Stack.Types.SetupInfo ( SetupInfo )
import           Stack.Types.TemplateName ( TemplateName )
import           Stack.Types.Version ( IntersectingVersionRange, VersionCheck )

-- | An uninterpreted representation of configuration options.
-- Configurations may be "cascaded" using mappend (left-biased).
data ConfigMonoid = ConfigMonoid
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
  , configMonoidPackageIndex     :: !(First PackageIndexConfig)
    -- ^ See: 'withPantryConfig'
  , configMonoidPackageIndices     :: !(First [PackageIndexConfig])
    -- ^ Deprecated in favour of package-index
  , configMonoidSystemGHC          :: !(First Bool)
    -- ^ See: 'configSystemGHC'
  , configMonoidInstallGHC          :: !FirstTrue
    -- ^ See: 'configInstallGHC'
  , configMonoidSkipGHCCheck        :: !FirstFalse
    -- ^ See: 'configSkipGHCCheck'
  , configMonoidSkipMsys            :: !FirstFalse
    -- ^ See: 'configSkipMsys'
  , configMonoidCompilerCheck       :: !(First VersionCheck)
    -- ^ See: 'configCompilerCheck'
  , configMonoidCompilerRepository  :: !(First CompilerRepository)
    -- ^ See: 'configCompilerRepository'
  , configMonoidRequireStackVersion :: !IntersectingVersionRange
    -- ^ See: 'configRequireStackVersion'
  , configMonoidArch                :: !(First String)
    -- ^ Used for overriding the platform
  , configMonoidGHCVariant          :: !(First GHCVariant)
    -- ^ Used for overriding the platform
  , configMonoidGHCBuild            :: !(First CompilerBuild)
    -- ^ Used for overriding the GHC build
  , configMonoidJobs                :: !(First Int)
    -- ^ See: 'configJobs'
  , configMonoidExtraIncludeDirs    :: ![FilePath]
    -- ^ See: 'configExtraIncludeDirs'
  , configMonoidExtraLibDirs        :: ![FilePath]
    -- ^ See: 'configExtraLibDirs'
  , configMonoidCustomPreprocessorExts :: ![Text]
    -- ^ See: 'configCustomPreprocessorExts'
  , configMonoidOverrideGccPath    :: !(First (Path Abs File))
    -- ^ Allow users to override the path to gcc
  , configMonoidOverrideHpack       :: !(First FilePath)
    -- ^ Use Hpack executable (overrides bundled Hpack)
  , configMonoidConcurrentTests     :: !(First Bool)
    -- ^ See: 'configConcurrentTests'
  , configMonoidLocalBinPath        :: !(First FilePath)
    -- ^ Used to override the binary installation dir
  , configMonoidTemplateParameters  :: !(Map Text Text)
    -- ^ Template parameters.
  , configMonoidScmInit             :: !(First SCM)
    -- ^ Initialize SCM (e.g. git init) when making new projects?
  , configMonoidGhcOptionsByName    :: !(MonoidMap PackageName (Monoid.Dual [Text]))
    -- ^ See 'configGhcOptionsByName'. Uses 'Monoid.Dual' so that
    -- options from the configs on the right come first, so that they
    -- can be overridden.
  , configMonoidGhcOptionsByCat     :: !(MonoidMap ApplyGhcOptions (Monoid.Dual [Text]))
    -- ^ See 'configGhcOptionsAll'. Uses 'Monoid.Dual' so that options
    -- from the configs on the right come first, so that they can be
    -- overridden.
  , configMonoidCabalConfigOpts     :: !(MonoidMap CabalConfigKey (Monoid.Dual [Text]))
    -- ^ See 'configCabalConfigOpts'.
  , configMonoidExtraPath           :: ![Path Abs Dir]
    -- ^ Additional paths to search for executables in
  , configMonoidSetupInfoLocations  :: ![String]
    -- ^ See 'configSetupInfoLocations'
  , configMonoidSetupInfoInline     :: !SetupInfo
    -- ^ See 'configSetupInfoInline'
  , configMonoidLocalProgramsBase   :: !(First (Path Abs Dir))
    -- ^ Override the default local programs dir, where e.g. GHC is installed.
  , configMonoidPvpBounds           :: !(First PvpBounds)
    -- ^ See 'configPvpBounds'
  , configMonoidModifyCodePage      :: !FirstTrue
    -- ^ See 'configModifyCodePage'
  , configMonoidRebuildGhcOptions   :: !FirstFalse
    -- ^ See 'configMonoidRebuildGhcOptions'
  , configMonoidApplyGhcOptions     :: !(First ApplyGhcOptions)
    -- ^ See 'configApplyGhcOptions'
  , configMonoidAllowNewer          :: !(First Bool)
    -- ^ See 'configMonoidAllowNewer'
  , configMonoidAllowNewerDeps      :: !(Maybe AllowNewerDeps)
    -- ^ See 'configMonoidAllowNewerDeps'
  , configMonoidDefaultTemplate     :: !(First TemplateName)
   -- ^ The default template to use when none is specified.
   -- (If Nothing, the 'default' default template is used.)
  , configMonoidAllowDifferentUser :: !(First Bool)
   -- ^ Allow users other than the Stack root owner to use the Stack
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
  deriving (Generic, Show)

instance Semigroup ConfigMonoid where
  (<>) = mappenddefault

instance Monoid ConfigMonoid where
  mempty = memptydefault
  mappend = (<>)
