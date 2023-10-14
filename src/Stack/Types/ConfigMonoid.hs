{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Stack.Types.ConfigMonoid
  ( ConfigMonoid (..)
  , parseConfigMonoid
  , parseConfigMonoidObject
  , configMonoidAllowDifferentUserName
  , configMonoidGHCVariantName
  , configMonoidInstallGHCName
  , configMonoidSystemGHCName
  ) where

import           Data.Aeson.Types ( Object, Value )
import           Data.Aeson.WarningParser
                   ( WarningParser, WithJSONWarnings, (..:?), (..!=)
                   , jsonSubWarnings, jsonSubWarningsT, jsonSubWarningsTT
                   , withObjectWarnings
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
import           Stack.Prelude
import           Stack.Types.AllowNewerDeps ( AllowNewerDeps )
import           Stack.Types.ApplyGhcOptions ( ApplyGhcOptions (..) )
import           Stack.Types.ApplyProgOptions ( ApplyProgOptions (..) )
import           Stack.Types.BuildOpts ( BuildOptsMonoid )
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
import           Stack.Types.Nix ( NixOptsMonoid )
import           Stack.Types.PvpBounds ( PvpBounds )
import           Stack.Types.SCM ( SCM )
import           Stack.Types.SetupInfo ( SetupInfo )
import           Stack.Types.TemplateName ( TemplateName )
import           Stack.Types.Version
                   ( IntersectingVersionRange (..), VersionCheck )
import qualified System.FilePath as FilePath

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
  , configMonoidApplyProgOptions     :: !(First ApplyProgOptions)
    -- ^ See 'configApplyProgOptions'
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
  , configMonoidNotifyIfNixOnPath  :: !FirstTrue
    -- ^ See 'configNotifyIfNixOnPath'
  , configMonoidNotifyIfGhcUntested  :: !FirstTrue
    -- ^ See 'configNotifyIfGhcUntested'
  , configMonoidNotifyIfCabalUntested  :: !FirstTrue
    -- ^ See 'configNotifyIfCabalUntested'
  , configMonoidCasaOpts :: !CasaOptsMonoid
    -- ^ Casa configuration options.
  , configMonoidCasaRepoPrefix     :: !(First CasaRepoPrefix)
    -- ^ Casa repository prefix (deprecated).
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

  configMonoidCabalConfigOpts' <-
    obj ..:? configMonoidConfigureOptionsName ..!= mempty
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
  configMonoidApplyProgOptions <-
    First <$> obj ..:? configMonoidApplyProgOptionsName
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
  configMonoidNotifyIfNixOnPath <-
    FirstTrue <$> obj ..:? configMonoidNotifyIfNixOnPathName
  configMonoidNotifyIfGhcUntested <-
    FirstTrue <$> obj ..:? configMonoidNotifyIfGhcUntestedName
  configMonoidNotifyIfCabalUntested <-
    FirstTrue <$> obj ..:? configMonoidNotifyIfCabalUntestedName
  configMonoidCasaOpts <-
    jsonSubWarnings (obj ..:? configMonoidCasaOptsName ..!= mempty)
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

configMonoidApplyProgOptionsName :: Text
configMonoidApplyProgOptionsName = "apply-prog-options"

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

configMonoidNotifyIfNixOnPathName :: Text
configMonoidNotifyIfNixOnPathName = "notify-if-nix-on-path"

configMonoidNotifyIfGhcUntestedName :: Text
configMonoidNotifyIfGhcUntestedName = "notify-if-ghc-untested"

configMonoidNotifyIfCabalUntestedName :: Text
configMonoidNotifyIfCabalUntestedName = "notify-if-cabal-untested"

configMonoidCasaOptsName :: Text
configMonoidCasaOptsName = "casa"

configMonoidCasaRepoPrefixName :: Text
configMonoidCasaRepoPrefixName = "casa-repo-prefix"

configMonoidSnapshotLocationName :: Text
configMonoidSnapshotLocationName = "snapshot-location-base"

configMonoidNoRunCompileName :: Text
configMonoidNoRunCompileName = "script-no-run-compile"

configMonoidStackDeveloperModeName :: Text
configMonoidStackDeveloperModeName = "stack-developer-mode"
