{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Stack.Types.ConfigMonoid
License     : BSD-3-Clause
-}

module Stack.Types.ConfigMonoid
  ( ConfigMonoid (..)
  , parseConfigMonoid
  , parseConfigMonoidObject
  , configMonoidAllowDifferentUserName
  , configMonoidGHCVariantName
  , configMonoidInstallGHCName
  , configMonoidInstallMsysName
  , configMonoidRecommendStackUpgradeName
  , configMonoidSystemGHCName
  ) where

import           Data.Aeson.Types ( Object, Value )
import           Data.Aeson.WarningParser
                   ( WarningParser, WithJSONWarnings, (..:?), (..!=)
                   , jsonSubWarnings, jsonSubWarningsT, withObjectWarnings
                   )
import           Casa.Client ( CasaRepoPrefix )
import           Control.Monad.Writer ( tell )
import           Data.Coerce ( coerce )
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import qualified Data.Monoid as Monoid
import           Data.Monoid.Map ( MonoidMap (..) )
import qualified Data.Yaml as Yaml
import           Distribution.Version ( anyVersion )
import           Generics.Deriving.Monoid ( mappenddefault, memptydefault )
import           Stack.Prelude hiding ( snapshotLocation )
import           Stack.Types.AllowNewerDeps ( AllowNewerDeps )
import           Stack.Types.ApplyGhcOptions ( ApplyGhcOptions (..) )
import           Stack.Types.ApplyProgOptions ( ApplyProgOptions (..) )
import           Stack.Types.BuildOptsMonoid ( BuildOptsMonoid )
import           Stack.Types.Casa ( CasaOptsMonoid )
import           Stack.Types.CabalConfigKey ( CabalConfigKey )
import           Stack.Types.ColorWhen ( ColorWhen )
import           Stack.Types.Compiler ( CompilerRepository )
import           Stack.Types.CompilerBuild ( CompilerBuild )
import           Stack.Types.Docker ( DockerOptsMonoid, VersionRangeJSON (..) )
import           Stack.Types.DumpLogs ( DumpLogs )
import           Stack.Types.GhcOptionKey ( GhcOptionKey (..) )
import           Stack.Types.GhcOptions ( GhcOptions (..) )
import           Stack.Types.GHCVariant ( GHCVariant )
import           Stack.Types.MsysEnvironment ( MsysEnvironment )
import           Stack.Types.Nix ( NixOptsMonoid )
import           Stack.Types.PvpBounds ( PvpBounds )
import           Stack.Types.SCM ( SCM )
import           Stack.Types.SetupInfo ( SetupInfo )
import           Stack.Types.TemplateName ( TemplateName )
import           Stack.Types.Version
                   ( IntersectingVersionRange (..), VersionCheck )
import qualified System.FilePath as FilePath
import Stack.Types.Snapshot (AbstractSnapshot)

-- | An uninterpreted representation of configuration options. Configurations
-- may be "cascaded" using mappend (left-biased).
data ConfigMonoid = ConfigMonoid
  { stackRoot               :: !(First (Path Abs Dir))
    -- ^ See: 'Stack.Types.Config.stackRoot'
  , workDir                 :: !(First (Path Rel Dir))
    -- ^ See: 'Stack.Types.Config.workDir'.
  , buildOpts               :: !BuildOptsMonoid
    -- ^ build options.
  , dockerOpts              :: !DockerOptsMonoid
    -- ^ Docker options.
  , nixOpts                 :: !NixOptsMonoid
    -- ^ Options for the execution environment (nix-shell or container)
  , connectionCount         :: !(First Int)
    -- ^ See: 'Stack.Types.Config.connectionCount'
  , hideTHLoading           :: !FirstTrue
    -- ^ See: 'Stack.Types.Config.hideTHLoading'
  , prefixTimestamps        :: !(First Bool)
    -- ^ See: 'Stack.Types.Config.prefixTimestamps'
  , latestSnapshot          :: !(First Text)
    -- ^ See: 'Stack.Types.Config.latestSnapshot'
  , packageIndex            :: !(First PackageIndexConfig)
    -- ^ See: 'withPantryConfig'
  , systemGHC               :: !(First Bool)
    -- ^ See: 'Stack.Types.Config.systemGHC'
  , installGHC              :: !FirstTrue
    -- ^ See: 'Stack.Types.Config.installGHC'
  , installMsys             :: !(First Bool)
    -- ^ See: 'Stack.Types.Config.installMsys'
  , skipGHCCheck            :: !FirstFalse
    -- ^ See: 'Stack.Types.Config.skipGHCCheck'
  , skipMsys                :: !FirstFalse
    -- ^ See: 'Stack.Types.Config.skipMsys'
  , msysEnvironment         :: !(First MsysEnvironment)
    -- ^ See: 'Stack.Types.Config.msysEnvironment'
  , compilerCheck           :: !(First VersionCheck)
    -- ^ See: 'Stack.Types.Config.compilerCheck'
  , compilerRepository      :: !(First CompilerRepository)
    -- ^ See: 'Stack.Types.Config.compilerRepository'
  , requireStackVersion     :: !IntersectingVersionRange
    -- ^ See: 'Stack.Types.Config.requireStackVersion'
  , arch                    :: !(First String)
    -- ^ Used for overriding the platform
  , ghcVariant              :: !(First GHCVariant)
    -- ^ Used for overriding the platform
  , ghcBuild                :: !(First CompilerBuild)
    -- ^ Used for overriding the GHC build
  , jobs                    :: !(First Int)
    -- ^ See: 'Stack.Types.Config.jobs'
  , extraIncludeDirs        :: ![FilePath]
    -- ^ See: 'Stack.Types.Config.extraIncludeDirs'
  , extraLibDirs            :: ![FilePath]
    -- ^ See: 'Stack.Types.Config.extraLibDirs'
  , customPreprocessorExts  :: ![Text]
    -- ^ See: 'Stack.Types.Config.customPreprocessorExts'
  , overrideGccPath         :: !(First (Path Abs File))
    -- ^ Allow users to override the path to gcc
  , overrideHpack           :: !(First FilePath)
    -- ^ Use Hpack executable (overrides bundled Hpack)
  , hpackForce              :: !FirstFalse
    -- ^ Pass --force to Hpack to always overwrite Cabal file
  , concurrentTests         :: !(First Bool)
    -- ^ See: 'Stack.Types.Config.concurrentTests'
  , localBinPath            :: !(First FilePath)
    -- ^ Used to override the binary installation dir
  , fileWatchHook           :: !(First FilePath)
    -- ^ Path to executable used to override --file-watch post-processing.
  , templateParameters      :: !(Map Text Text)
    -- ^ Template parameters.
  , scmInit                 :: !(First SCM)
    -- ^ Initialize SCM (e.g. git init) when making new projects?
  , ghcOptionsByName        :: !(MonoidMap PackageName (Monoid.Dual [Text]))
    -- ^ See 'Stack.Types.Config.ghcOptionsByName'. Uses 'Monoid.Dual' so that
    -- options from the configs on the right come first, so that they
    -- can be overridden.
  , ghcOptionsByCat         :: !(MonoidMap ApplyGhcOptions (Monoid.Dual [Text]))
    -- ^ See 'Stack.Types.Config.ghcOptionsAll'. Uses 'Monoid.Dual' so that options
    -- from the configs on the right come first, so that they can be
    -- overridden.
  , cabalConfigOpts         :: !(MonoidMap CabalConfigKey (Monoid.Dual [Text]))
    -- ^ See 'Stack.Types.Config.cabalConfigOpts'.
  , extraPath               :: ![Path Abs Dir]
    -- ^ Additional paths to search for executables in
  , setupInfoLocations      :: ![String]
    -- ^ See 'Stack.Types.Config.setupInfoLocations'
  , setupInfoInline         :: !SetupInfo
    -- ^ See 'Stack.Types.Config.setupInfoInline'
  , localProgramsBase       :: !(First (Path Abs Dir))
    -- ^ Override the default local programs dir, where e.g. GHC is installed.
  , pvpBounds               :: !(First PvpBounds)
    -- ^ See 'Stack.Types.Config.pvpBounds'
  , modifyCodePage          :: !FirstTrue
    -- ^ See 'Stack.Types.Config.modifyCodePage'
  , rebuildGhcOptions       :: !FirstFalse
    -- ^ See 'Stack.Types.Config.monoidRebuildGhcOptions'
  , applyGhcOptions         :: !(First ApplyGhcOptions)
    -- ^ See 'Stack.Types.Config.applyGhcOptions'
  , applyProgOptions        :: !(First ApplyProgOptions)
    -- ^ See 'Stack.Types.Config.applyProgOptions'
  , allowNewer              :: !(First Bool)
    -- ^ See 'Stack.Types.Config.monoidAllowNewer'
  , allowNewerDeps          :: !(Maybe AllowNewerDeps)
    -- ^ See 'Stack.Types.Config.monoidAllowNewerDeps'
  , defaultInitSnapshot     :: !(First (Unresolved AbstractSnapshot))
   -- ^ An optional default snapshot to use with @stack init@ when none is
   -- specified.
  , defaultTemplate         :: !(First TemplateName)
   -- ^ The default template to use when none is specified.
   -- (If Nothing, the 'default' default template is used.)
  , allowDifferentUser      :: !(First Bool)
   -- ^ Allow users other than the Stack root owner to use the Stack
   -- installation.
  , dumpLogs                :: !(First DumpLogs)
    -- ^ See 'Stack.Types.Config.dumpLogs'
  , saveHackageCreds        :: !FirstTrue
    -- ^ See 'Stack.Types.Config.saveHackageCreds'
  , hackageBaseUrl          :: !(First Text)
    -- ^ See 'Stack.Types.Config.hackageBaseUrl'
  , colorWhen               :: !(First ColorWhen)
    -- ^ When to use \'ANSI\' colors
  , styles                  :: !StylesUpdate
  , hideSourcePaths         :: !FirstTrue
    -- ^ See 'Stack.Types.Config.hideSourcePaths'
  , recommendStackUpgrade   :: !FirstTrue
    -- ^ See 'Stack.Types.Config.recommendStackUpgrade'
  , notifyIfNixOnPath       :: !FirstTrue
    -- ^ See 'Stack.Types.Config.notifyIfNixOnPath'
  , notifyIfGhcUntested     :: !FirstTrue
    -- ^ See 'Stack.Types.Config.notifyIfGhcUntested'
  , notifyIfCabalUntested   :: !FirstTrue
    -- ^ See 'Stack.Types.Config.notifyIfCabalUntested'
  , notifyIfArchUnknown     :: !FirstTrue
    -- ^ See 'Stack.Types.Config.notifyIfArchUnknown'
  , notifyIfNoRunTests      :: !FirstTrue
    -- ^ See 'Stack.Types.Config.notifyIfNoRunTests'
  , notifyIfNoRunBenchmarks :: !FirstTrue
    -- ^ See 'Stack.Types.Config.notifyIfNoRunBenchmarks'
  , casaOpts                :: !CasaOptsMonoid
    -- ^ Casa configuration options.
  , casaRepoPrefix          :: !(First CasaRepoPrefix)
    -- ^ Casa repository prefix (deprecated).
  , snapshotLocation        :: !(First Text)
    -- ^ Custom location of LTS/Nightly snapshots
  , globalHintsLocation     :: !(First (Unresolved GlobalHintsLocation))
    -- ^ Custom location of global hints
  , noRunCompile            :: !FirstFalse
    -- ^ See: 'Stack.Types.Config.noRunCompile'
  , stackDeveloperMode      :: !(First Bool)
    -- ^ See 'Stack.Types.Config.stackDeveloperMode'
  }
  deriving Generic

instance Semigroup ConfigMonoid where
  (<>) = mappenddefault

instance Monoid ConfigMonoid where
  mempty = memptydefault
  mappend = (<>)

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
  let stackRoot = First Nothing
  workDir <- First <$> obj ..:? configMonoidWorkDirName
  buildOpts <- jsonSubWarnings (obj ..:? configMonoidBuildOptsName ..!= mempty)
  dockerOpts <-
    jsonSubWarnings (obj ..:? configMonoidDockerOptsName ..!= mempty)
  nixOpts <- jsonSubWarnings (obj ..:? configMonoidNixOptsName ..!= mempty)
  connectionCount <- First <$> obj ..:? configMonoidConnectionCountName
  hideTHLoading <- FirstTrue <$> obj ..:? configMonoidHideTHLoadingName
  prefixTimestamps <- First <$> obj ..:? configMonoidPrefixTimestampsName

  latestSnapshot <- obj ..:? configMonoidUrlsName >>= \case
    Nothing -> pure $ First Nothing
    Just urls -> jsonSubWarnings $ lift $ withObjectWarnings
      "urls"
      (\o -> First <$> o ..:? "latest-snapshot" :: WarningParser (First Text))
      (urls :: Value)

  packageIndex <-
    First <$> jsonSubWarningsT (obj ..:?  configMonoidPackageIndexName)
  systemGHC <- First <$> obj ..:? configMonoidSystemGHCName
  installGHC <- FirstTrue <$> obj ..:? configMonoidInstallGHCName
  installMsys <- First <$> obj ..:? configMonoidInstallMsysName
  skipGHCCheck <- FirstFalse <$> obj ..:? configMonoidSkipGHCCheckName
  skipMsys <- FirstFalse <$> obj ..:? configMonoidSkipMsysName
  msysEnvironment <- First <$> obj ..:? configMonoidMsysEnvironmentName
  requireStackVersion <-
    IntersectingVersionRange . (.versionRangeJSON) <$>
      ( obj ..:? configMonoidRequireStackVersionName
          ..!= VersionRangeJSON anyVersion
      )
  arch <- First <$> obj ..:? configMonoidArchName
  ghcVariant <- First <$> obj ..:? configMonoidGHCVariantName
  ghcBuild <- First <$> obj ..:? configMonoidGHCBuildName
  jobs <- First <$> obj ..:? configMonoidJobsName
  extraIncludeDirs <- map (toFilePath rootDir FilePath.</>) <$>
    obj ..:?  configMonoidExtraIncludeDirsName ..!= []
  extraLibDirs <- map (toFilePath rootDir FilePath.</>) <$>
    obj ..:?  configMonoidExtraLibDirsName ..!= []
  customPreprocessorExts <-
    obj ..:?  configMonoidCustomPreprocessorExtsName ..!= []
  overrideGccPath <- First <$> obj ..:? configMonoidOverrideGccPathName
  overrideHpack <- First <$> obj ..:? configMonoidOverrideHpackName
  hpackForce <- FirstFalse <$> obj ..:? configMonoidHpackForceName
  concurrentTests <- First <$> obj ..:? configMonoidConcurrentTestsName
  localBinPath <- First <$> obj ..:? configMonoidLocalBinPathName
  fileWatchHook <- First <$> obj ..:? configMonoidFileWatchHookName
  (scmInit, templateParameters) <- obj ..:? "templates" >>= \case
    Nothing -> pure (First Nothing,M.empty)
    Just tobj -> do
      scmInit <- tobj ..:? configMonoidScmInitName
      params <- tobj ..:? configMonoidTemplateParametersName
      pure (First scmInit,fromMaybe M.empty params)
  compilerCheck <- First <$> obj ..:? configMonoidCompilerCheckName
  compilerRepository <- First <$> (obj ..:? configMonoidCompilerRepositoryName)

  options <- Map.map (.ghcOptions) <$>
    obj ..:? configMonoidGhcOptionsName ..!= (mempty :: Map GhcOptionKey GhcOptions)

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

  let ghcOptionsByCat = coerce $ Map.fromList
        [ (AGOEverything, optionsEverything)
        , (AGOLocals, Map.findWithDefault [] GOKLocals options)
        , (AGOTargets, Map.findWithDefault [] GOKTargets options)
        ]

      ghcOptionsByName = coerce $ Map.fromList
          [(name, opts) | (GOKPackage name, opts) <- Map.toList options]

  cabalConfigOpts' <- obj ..:? configMonoidConfigureOptionsName ..!= mempty
  let cabalConfigOpts = coerce (cabalConfigOpts' :: Map CabalConfigKey [Text])
  extraPath <- obj ..:? configMonoidExtraPathName ..!= []
  setupInfoLocations <- obj ..:? configMonoidSetupInfoLocationsName ..!= []
  setupInfoInline <-
    jsonSubWarningsT (obj ..:? configMonoidSetupInfoInlineName) ..!= mempty
  localProgramsBase <- First <$> obj ..:? configMonoidLocalProgramsBaseName
  pvpBounds <- First <$> obj ..:? configMonoidPvpBoundsName
  modifyCodePage <- FirstTrue <$> obj ..:? configMonoidModifyCodePageName
  rebuildGhcOptions <- FirstFalse <$> obj ..:? configMonoidRebuildGhcOptionsName
  applyGhcOptions <- First <$> obj ..:? configMonoidApplyGhcOptionsName
  applyProgOptions <- First <$> obj ..:? configMonoidApplyProgOptionsName
  allowNewer <- First <$> obj ..:? configMonoidAllowNewerName
  allowNewerDeps <- obj ..:? configMonoidAllowNewerDepsName
  defaultInitSnapshot <- First <$> obj ..:? configMonoidDefaultInitSnapshotName
  defaultTemplate <- First <$> obj ..:? configMonoidDefaultTemplateName
  allowDifferentUser <- First <$> obj ..:? configMonoidAllowDifferentUserName
  dumpLogs <- First <$> obj ..:? configMonoidDumpLogsName
  saveHackageCreds <- FirstTrue <$> obj ..:? configMonoidSaveHackageCredsName
  hackageBaseUrl <- First <$> obj ..:? configMonoidHackageBaseUrlName
  configMonoidColorWhenUS <- obj ..:? configMonoidColorWhenUSName
  configMonoidColorWhenGB <- obj ..:? configMonoidColorWhenGBName
  let colorWhen = First $ configMonoidColorWhenUS <|> configMonoidColorWhenGB
  configMonoidStylesUS <- obj ..:? configMonoidStylesUSName
  configMonoidStylesGB <- obj ..:? configMonoidStylesGBName
  let styles = fromMaybe mempty $ configMonoidStylesUS <|> configMonoidStylesGB
  hideSourcePaths <- FirstTrue <$> obj ..:? configMonoidHideSourcePathsName
  recommendStackUpgrade <-
    FirstTrue <$> obj ..:? configMonoidRecommendStackUpgradeName
  notifyIfNixOnPath <- FirstTrue <$> obj ..:? configMonoidNotifyIfNixOnPathName
  notifyIfGhcUntested <-
    FirstTrue <$> obj ..:? configMonoidNotifyIfGhcUntestedName
  notifyIfCabalUntested <-
    FirstTrue <$> obj ..:? configMonoidNotifyIfCabalUntestedName
  notifyIfArchUnknown <-
    FirstTrue <$> obj ..:? configMonoidNotifyIfArchUnknownName
  notifyIfNoRunTests <-
    FirstTrue <$> obj ..:? configMonoidNotifyIfNoRunTestsName
  notifyIfNoRunBenchmarks <-
    FirstTrue <$> obj ..:? configMonoidNotifyIfNoRunBenchmarksName
  casaOpts <- jsonSubWarnings (obj ..:? configMonoidCasaOptsName ..!= mempty)
  casaRepoPrefix <- First <$> obj ..:? configMonoidCasaRepoPrefixName
  snapshotLocation <- First <$> obj ..:? configMonoidSnapshotLocationName
  globalHintsLocation <-
    First <$> jsonSubWarningsT (obj ..:? configMonoidGlobalHintsLocationName)
  noRunCompile <- FirstFalse <$> obj ..:? configMonoidNoRunCompileName
  stackDeveloperMode <- First <$> obj ..:? configMonoidStackDeveloperModeName
  pure ConfigMonoid
    { stackRoot
    , workDir
    , buildOpts
    , dockerOpts
    , nixOpts
    , connectionCount
    , hideTHLoading
    , prefixTimestamps
    , latestSnapshot
    , packageIndex
    , systemGHC
    , installGHC
    , installMsys
    , skipGHCCheck
    , skipMsys
    , msysEnvironment
    , compilerCheck
    , compilerRepository
    , requireStackVersion
    , arch
    , ghcVariant
    , ghcBuild
    , jobs
    , extraIncludeDirs
    , extraLibDirs
    , customPreprocessorExts
    , overrideGccPath
    , overrideHpack
    , hpackForce
    , concurrentTests
    , localBinPath
    , fileWatchHook
    , templateParameters
    , scmInit
    , ghcOptionsByName
    , ghcOptionsByCat
    , cabalConfigOpts
    , extraPath
    , setupInfoLocations
    , setupInfoInline
    , localProgramsBase
    , pvpBounds
    , modifyCodePage
    , rebuildGhcOptions
    , applyGhcOptions
    , applyProgOptions
    , allowNewer
    , allowNewerDeps
    , defaultInitSnapshot
    , defaultTemplate
    , allowDifferentUser
    , dumpLogs
    , saveHackageCreds
    , hackageBaseUrl
    , colorWhen
    , styles
    , hideSourcePaths
    , recommendStackUpgrade
    , notifyIfNixOnPath
    , notifyIfGhcUntested
    , notifyIfCabalUntested
    , notifyIfArchUnknown
    , notifyIfNoRunTests
    , notifyIfNoRunBenchmarks
    , casaOpts
    , casaRepoPrefix
    , snapshotLocation
    , globalHintsLocation
    , noRunCompile
    , stackDeveloperMode
    }

configMonoidWorkDirName :: Text
configMonoidWorkDirName = "work-dir"

configMonoidBuildOptsName :: Text
configMonoidBuildOptsName = "build"

configMonoidDockerOptsName :: Text
configMonoidDockerOptsName = "docker"

configMonoidNixOptsName :: Text
configMonoidNixOptsName = "nix"

configMonoidConfigureOptionsName :: Text
configMonoidConfigureOptionsName = "configure-options"

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

configMonoidSystemGHCName :: Text
configMonoidSystemGHCName = "system-ghc"

configMonoidInstallGHCName :: Text
configMonoidInstallGHCName = "install-ghc"

configMonoidInstallMsysName :: Text
configMonoidInstallMsysName = "install-msys"

configMonoidSkipGHCCheckName :: Text
configMonoidSkipGHCCheckName = "skip-ghc-check"

configMonoidSkipMsysName :: Text
configMonoidSkipMsysName = "skip-msys"

configMonoidMsysEnvironmentName :: Text
configMonoidMsysEnvironmentName = "msys-environment"

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

configMonoidHpackForceName :: Text
configMonoidHpackForceName = "hpack-force"

configMonoidConcurrentTestsName :: Text
configMonoidConcurrentTestsName = "concurrent-tests"

configMonoidLocalBinPathName :: Text
configMonoidLocalBinPathName = "local-bin-path"

configMonoidFileWatchHookName :: Text
configMonoidFileWatchHookName = "file-watch-hook"

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

configMonoidApplyProgOptionsName :: Text
configMonoidApplyProgOptionsName = "apply-prog-options"

configMonoidAllowNewerName :: Text
configMonoidAllowNewerName = "allow-newer"

configMonoidAllowNewerDepsName :: Text
configMonoidAllowNewerDepsName = "allow-newer-deps"

configMonoidDefaultInitSnapshotName :: Text
configMonoidDefaultInitSnapshotName = "default-init-snapshot"

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

configMonoidRecommendStackUpgradeName :: Text
configMonoidRecommendStackUpgradeName = "recommend-stack-upgrade"

configMonoidNotifyIfNixOnPathName :: Text
configMonoidNotifyIfNixOnPathName = "notify-if-nix-on-path"

configMonoidNotifyIfGhcUntestedName :: Text
configMonoidNotifyIfGhcUntestedName = "notify-if-ghc-untested"

configMonoidNotifyIfCabalUntestedName :: Text
configMonoidNotifyIfCabalUntestedName = "notify-if-cabal-untested"

configMonoidNotifyIfArchUnknownName :: Text
configMonoidNotifyIfArchUnknownName = "notify-if-arch-unknown"

configMonoidNotifyIfNoRunTestsName :: Text
configMonoidNotifyIfNoRunTestsName = "notify-if-no-run-tests"

configMonoidNotifyIfNoRunBenchmarksName :: Text
configMonoidNotifyIfNoRunBenchmarksName = "notify-if-no-run-benchmarks"

configMonoidCasaOptsName :: Text
configMonoidCasaOptsName = "casa"

configMonoidCasaRepoPrefixName :: Text
configMonoidCasaRepoPrefixName = "casa-repo-prefix"

configMonoidSnapshotLocationName :: Text
configMonoidSnapshotLocationName = "snapshot-location-base"

configMonoidGlobalHintsLocationName :: Text
configMonoidGlobalHintsLocationName = "global-hints-location"

configMonoidNoRunCompileName :: Text
configMonoidNoRunCompileName = "script-no-run-compile"

configMonoidStackDeveloperModeName :: Text
configMonoidStackDeveloperModeName = "stack-developer-mode"
