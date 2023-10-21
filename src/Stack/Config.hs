{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | The general Stack configuration that starts everything off. This should
-- be smart to fallback if there is no stack.yaml, instead relying on
-- whatever files are available.
--
-- If there is no stack.yaml, and there is a cabal.config, we
-- read in those constraints, and if there's a cabal.sandbox.config,
-- we read any constraints from there and also find the package
-- database from there, etc. And if there's nothing, we should
-- probably default to behaving like cabal, possibly with spitting out
-- a warning that "you should run `stk init` to make things better".
module Stack.Config
  ( loadConfig
  , loadConfigYaml
  , packagesParser
  , getImplicitGlobalProjectDir
  , getSnapshots
  , makeConcreteResolver
  , checkOwnership
  , getInContainer
  , getInNixShell
  , defaultConfigYaml
  , getProjectConfig
  , withBuildConfig
  , withNewLogFunc
  ) where

import           Control.Monad.Extra ( firstJustM )
import           Data.Aeson.Types ( Value )
import           Data.Aeson.WarningParser
                    ( WithJSONWarnings (..), logJSONWarnings )
import           Data.Array.IArray ( (!), (//) )
import qualified Data.ByteString as S
import           Data.ByteString.Builder ( byteString )
import           Data.Coerce ( coerce )
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as MS
import qualified Data.Monoid
import           Data.Monoid.Map ( MonoidMap (..) )
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import           Distribution.System
                   ( Arch (OtherArch), OS (..), Platform (..), buildPlatform )
import qualified Distribution.Text ( simpleParse )
import           Distribution.Version ( simplifyVersionRange )
import           GHC.Conc ( getNumProcessors )
import           Network.HTTP.StackClient
                   ( httpJSON, parseUrlThrow, getResponseBody )
import           Options.Applicative ( Parser, help, long, metavar, strOption )
import           Path
                   ( PathException (..), (</>), parent, parseAbsDir
                   , parseAbsFile, parseRelDir, stripProperPrefix
                   )
import           Path.Extra ( toFilePathNoTrailingSep )
import           Path.Find ( findInParents )
import           Path.IO
                   ( XdgDirectory (..), canonicalizePath, doesDirExist
                   , doesFileExist, ensureDir, forgivingAbsence
                   , getAppUserDataDir, getCurrentDir, getXdgDir, resolveDir
                   , resolveDir', resolveFile'
                   )
import           RIO.List ( unzip )
import           RIO.Process
                   ( HasProcessContext (..), ProcessContext, augmentPathMap
                   , envVarsL
                   , mkProcessContext
                   )
import           RIO.Time ( toGregorian )
import           Stack.Build.Haddock ( shouldHaddockDeps )
import           Stack.Config.Build ( buildOptsFromMonoid )
import           Stack.Config.Docker ( dockerOptsFromMonoid )
import           Stack.Config.Nix ( nixOptsFromMonoid )
import           Stack.Constants
                   ( defaultGlobalConfigPath, defaultGlobalConfigPathDeprecated
                   , defaultUserConfigPath, defaultUserConfigPathDeprecated
                   , implicitGlobalProjectDir
                   , implicitGlobalProjectDirDeprecated, inContainerEnvVar
                   , inNixShellEnvVar, osIsWindows, pantryRootEnvVar
                   , platformVariantEnvVar, relDirBin, relDirStackWork
                   , relFileReadmeTxt, relFileStorage, relDirPantry
                   , relDirPrograms, relDirStackProgName, relDirUpperPrograms
                   , stackDeveloperModeDefault, stackDotYaml, stackProgName
                   , stackRootEnvVar, stackWorkEnvVar, stackXdgEnvVar
                   )
import           Stack.Lock ( lockCachedWanted )
import           Stack.Prelude
import           Stack.SourceMap
                   ( additionalDepPackage, checkFlagsUsedThrowing
                   , mkProjectPackage
                   )
import           Stack.Storage.Project ( initProjectStorage )
import           Stack.Storage.User ( initUserStorage )
import           Stack.Storage.Util ( handleMigrationException )
import           Stack.Types.AllowNewerDeps ( AllowNewerDeps (..) )
import           Stack.Types.ApplyGhcOptions ( ApplyGhcOptions (..) )
import           Stack.Types.ApplyProgOptions ( ApplyProgOptions (..) )
import           Stack.Types.Build.Exception ( BuildException (..) )
import           Stack.Types.BuildConfig ( BuildConfig (..) )
import           Stack.Types.BuildOpts ( BuildOpts (..) )
import           Stack.Types.ColorWhen ( ColorWhen (..) )
import           Stack.Types.Compiler ( defaultCompilerRepository )
import           Stack.Types.Config
                   ( Config (..), HasConfig (..), askLatestSnapshotUrl
                   , configProjectRoot, stackRootL, workDirL
                   )
import           Stack.Types.Config.Exception
                   ( ConfigException (..), ConfigPrettyException (..)
                   , ParseAbsolutePathException (..), packageIndicesWarning )
import           Stack.Types.ConfigMonoid
                   ( ConfigMonoid (..), parseConfigMonoid )
import           Stack.Types.Casa ( CasaOptsMonoid (..) )
import           Stack.Types.Docker ( DockerOptsMonoid (..), dockerEnable )
import           Stack.Types.DumpLogs ( DumpLogs (..) )
import           Stack.Types.GlobalOpts (  GlobalOpts (..) )
import           Stack.Types.Nix ( nixEnable )
import           Stack.Types.Platform
                   ( PlatformVariant (..), platformOnlyRelDir )
import           Stack.Types.Project ( Project (..) )
import           Stack.Types.ProjectAndConfigMonoid
                   ( ProjectAndConfigMonoid (..), parseProjectAndConfigMonoid )
import           Stack.Types.ProjectConfig ( ProjectConfig (..) )
import           Stack.Types.PvpBounds ( PvpBounds (..), PvpBoundsType (..) )
import           Stack.Types.Resolver ( AbstractResolver (..), Snapshots (..) )
import           Stack.Types.Runner
                   ( HasRunner (..), Runner (..), globalOptsL, terminalL )
import           Stack.Types.SourceMap
                   ( CommonPackage (..), DepPackage (..), ProjectPackage (..)
                   , SMWanted (..)
                   )
import           Stack.Types.StackYamlLoc ( StackYamlLoc (..) )
import           Stack.Types.UnusedFlags ( FlagSource (..) )
import           Stack.Types.Version
                   ( IntersectingVersionRange (..), VersionCheck (..)
                   , stackVersion, withinRange
                   )
import           System.Console.ANSI ( hSupportsANSI, setSGRCode )
import           System.Environment ( getEnvironment, lookupEnv )
import           System.Info.ShortPathName ( getShortPathName )
import           System.PosixCompat.Files ( fileOwner, getFileStatus )
import           System.Posix.User ( getEffectiveUserID )

-- | If deprecated path exists, use it and print a warning. Otherwise, return
-- the new path.
tryDeprecatedPath ::
     HasTerm env
  => Maybe T.Text
     -- ^ Description of file for warning (if Nothing, no deprecation warning is
     -- displayed)
  -> (Path Abs a -> RIO env Bool)
     -- ^ Test for existence
  -> Path Abs a
     -- ^ New path
  -> Path Abs a
     -- ^ Deprecated path
  -> RIO env (Path Abs a, Bool)
     -- ^ (Path to use, whether it already exists)
tryDeprecatedPath mWarningDesc exists new old = do
  newExists <- exists new
  if newExists
    then pure (new, True)
    else do
      oldExists <- exists old
      if oldExists
        then do
          case mWarningDesc of
            Nothing -> pure ()
            Just desc ->
              prettyWarnL
                [ flow "Location of"
                , flow (T.unpack desc)
                , "at"
                , style Dir (fromString $ toFilePath old)
                , flow "is deprecated; rename it to"
                , style Dir (fromString $ toFilePath new)
                , "instead."
                ]
          pure (old, True)
        else pure (new, False)

-- | Get the location of the implicit global project directory. If the directory
-- already exists at the deprecated location, its location is returned.
-- Otherwise, the new location is returned.
getImplicitGlobalProjectDir ::HasTerm env => Config -> RIO env (Path Abs Dir)
getImplicitGlobalProjectDir config =
  --TEST no warning printed
  fst <$> tryDeprecatedPath
    Nothing
    doesDirExist
    (implicitGlobalProjectDir stackRoot)
    (implicitGlobalProjectDirDeprecated stackRoot)
 where
  stackRoot = view stackRootL config

-- | Download the 'Snapshots' value from stackage.org.
getSnapshots :: HasConfig env => RIO env Snapshots
getSnapshots = do
  latestUrlText <- askLatestSnapshotUrl
  latestUrl <- parseUrlThrow (T.unpack latestUrlText)
  logDebug $ "Downloading snapshot versions file from " <> display latestUrlText
  result <- httpJSON latestUrl
  logDebug "Done downloading and parsing snapshot versions file"
  pure $ getResponseBody result

-- | Turn an 'AbstractResolver' into a 'Resolver'.
makeConcreteResolver ::
     HasConfig env
  => AbstractResolver
  -> RIO env RawSnapshotLocation
makeConcreteResolver (ARResolver r) = pure r
makeConcreteResolver ar = do
  r <-
    case ar of
      ARGlobal -> do
        config <- view configL
        implicitGlobalDir <- getImplicitGlobalProjectDir config
        let fp = implicitGlobalDir </> stackDotYaml
        iopc <- loadConfigYaml (parseProjectAndConfigMonoid (parent fp)) fp
        ProjectAndConfigMonoid project _ <- liftIO iopc
        pure $ projectResolver project
      ARLatestNightly ->
        RSLSynonym . Nightly . snapshotsNightly <$> getSnapshots
      ARLatestLTSMajor x -> do
        snapshots <- getSnapshots
        case IntMap.lookup x $ snapshotsLts snapshots of
          Nothing -> throwIO $ NoLTSWithMajorVersion x
          Just y -> pure $ RSLSynonym $ LTS x y
      ARLatestLTS -> do
        snapshots <- getSnapshots
        if IntMap.null $ snapshotsLts snapshots
          then throwIO NoLTSFound
          else let (x, y) = IntMap.findMax $ snapshotsLts snapshots
               in  pure $ RSLSynonym $ LTS x y
  prettyInfoL
    [ flow "Selected resolver:"
    , style Current (fromString $ T.unpack $ textDisplay r) <> "."
    ]
  pure r

-- | Get the latest snapshot resolver available.
getLatestResolver :: HasConfig env => RIO env RawSnapshotLocation
getLatestResolver = do
  snapshots <- getSnapshots
  let mlts = uncurry LTS <$>
             listToMaybe (reverse (IntMap.toList (snapshotsLts snapshots)))
  pure $ RSLSynonym $ fromMaybe (Nightly (snapshotsNightly snapshots)) mlts

-- Interprets ConfigMonoid options.
configFromConfigMonoid ::
     (HasRunner env, HasTerm env)
  => Path Abs Dir -- ^ Stack root, e.g. ~/.stack
  -> Path Abs File -- ^ user config file path, e.g. ~/.stack/config.yaml
  -> Maybe AbstractResolver
  -> ProjectConfig (Project, Path Abs File)
  -> ConfigMonoid
  -> (Config -> RIO env a)
  -> RIO env a
configFromConfigMonoid
  configStackRoot
  configUserConfigPath
  configResolver
  configProject
  ConfigMonoid{..}
  inner
  = do
    -- If --stack-work is passed, prefer it. Otherwise, if STACK_WORK
    -- is set, use that. If neither, use the default ".stack-work"
    mstackWorkEnv <- liftIO $ lookupEnv stackWorkEnvVar
    let mproject =
          case configProject of
            PCProject pair -> Just pair
            PCGlobalProject -> Nothing
            PCNoProject _deps -> Nothing
        configAllowLocals =
          case configProject of
            PCProject _ -> True
            PCGlobalProject -> True
            PCNoProject _ -> False
    configWorkDir0 <-
      let parseStackWorkEnv x =
            catch
              (parseRelDir x)
              ( \e -> case e of
                  InvalidRelDir _ ->
                    prettyThrowIO $ StackWorkEnvNotRelativeDir x
                  _ -> throwIO e
              )
      in  maybe (pure relDirStackWork) (liftIO . parseStackWorkEnv) mstackWorkEnv
    let configWorkDir = fromFirst configWorkDir0 configMonoidWorkDir
        configLatestSnapshot = fromFirst
          "https://s3.amazonaws.com/haddock.stackage.org/snapshots.json"
          configMonoidLatestSnapshot
        clConnectionCount = fromFirst 8 configMonoidConnectionCount
        configHideTHLoading = fromFirstTrue configMonoidHideTHLoading
        configPrefixTimestamps = fromFirst False configMonoidPrefixTimestamps
        configGHCVariant = getFirst configMonoidGHCVariant
        configCompilerRepository = fromFirst
          defaultCompilerRepository
          configMonoidCompilerRepository
        configGHCBuild = getFirst configMonoidGHCBuild
        configInstallGHC = fromFirstTrue configMonoidInstallGHC
        configSkipGHCCheck = fromFirstFalse configMonoidSkipGHCCheck
        configSkipMsys = fromFirstFalse configMonoidSkipMsys
        configExtraIncludeDirs = configMonoidExtraIncludeDirs
        configExtraLibDirs = configMonoidExtraLibDirs
        configCustomPreprocessorExts = configMonoidCustomPreprocessorExts
        configOverrideGccPath = getFirst configMonoidOverrideGccPath
        -- Only place in the codebase where platform is hard-coded. In theory in
        -- the future, allow it to be configured.
        (Platform defArch defOS) = buildPlatform
        arch = fromMaybe defArch
          $ getFirst configMonoidArch >>= Distribution.Text.simpleParse
        os = defOS
        configPlatform = Platform arch os
        configRequireStackVersion = simplifyVersionRange
          (getIntersectingVersionRange configMonoidRequireStackVersion)
        configCompilerCheck = fromFirst MatchMinor configMonoidCompilerCheck
    case arch of
      OtherArch "aarch64" -> pure ()
      OtherArch unk ->
        prettyWarnL
          [ flow "Unknown value for architecture setting:"
          , style Shell (fromString unk) <> "."
          ]
      _ -> pure ()
    configPlatformVariant <- liftIO $
      maybe PlatformVariantNone PlatformVariant <$> lookupEnv platformVariantEnvVar
    let configBuild = buildOptsFromMonoid configMonoidBuildOpts
    configDocker <-
      dockerOptsFromMonoid (fmap fst mproject) configResolver configMonoidDockerOpts
    configNix <- nixOptsFromMonoid configMonoidNixOpts os
    configSystemGHC <-
      case (getFirst configMonoidSystemGHC, nixEnable configNix) of
        (Just False, True) ->
          throwM NixRequiresSystemGhc
        _ ->
          pure
            (fromFirst
              (dockerEnable configDocker || nixEnable configNix)
              configMonoidSystemGHC)
    when (isJust configGHCVariant && configSystemGHC) $
      throwM ManualGHCVariantSettingsAreIncompatibleWithSystemGHC
    rawEnv <- liftIO getEnvironment
    pathsEnv <- either throwM pure
      $ augmentPathMap (map toFilePath configMonoidExtraPath)
                       (Map.fromList (map (T.pack *** T.pack) rawEnv))
    origEnv <- mkProcessContext pathsEnv
    let configProcessContextSettings _ = pure origEnv
    configLocalProgramsBase <- case getFirst configMonoidLocalProgramsBase of
      Nothing -> getDefaultLocalProgramsBase configStackRoot configPlatform origEnv
      Just path -> pure path
    let localProgramsFilePath = toFilePath configLocalProgramsBase
    when (osIsWindows && ' ' `elem` localProgramsFilePath) $ do
      ensureDir configLocalProgramsBase
      -- getShortPathName returns the long path name when a short name does not
      -- exist.
      shortLocalProgramsFilePath <-
        liftIO $ getShortPathName localProgramsFilePath
      when (' ' `elem` shortLocalProgramsFilePath) $
        prettyError $
          "[S-8432]"
          <> line
          <> fillSep
               [ flow "Stack's 'programs' path contains a space character and \
                      \has no alternative short ('8 dot 3') name. This will \
                      \cause problems with packages that use the GNU project's \
                      \'configure' shell script. Use the"
               , style Shell "local-programs-path"
               , flow "configuration option to specify an alternative path. \
                      \The current path is:"
               , style File (fromString localProgramsFilePath) <> "."
               ]
    platformOnlyDir <-
      runReaderT platformOnlyRelDir (configPlatform, configPlatformVariant)
    let configLocalPrograms = configLocalProgramsBase </> platformOnlyDir
    configLocalBin <-
      case getFirst configMonoidLocalBinPath of
        Nothing -> do
          localDir <- getAppUserDataDir "local"
          pure $ localDir </> relDirBin
        Just userPath ->
          (case mproject of
            -- Not in a project
            Nothing -> resolveDir' userPath
            -- Resolves to the project dir and appends the user path if it is
            -- relative
            Just (_, configYaml) -> resolveDir (parent configYaml) userPath)
          -- TODO: Either catch specific exceptions or add a
          -- parseRelAsAbsDirMaybe utility and use it along with
          -- resolveDirMaybe.
          `catchAny`
          const (throwIO (NoSuchDirectory userPath))
    configJobs <-
      case getFirst configMonoidJobs of
        Nothing -> liftIO getNumProcessors
        Just i -> pure i
    let configConcurrentTests = fromFirst True configMonoidConcurrentTests
        configTemplateParams = configMonoidTemplateParameters
        configScmInit = getFirst configMonoidScmInit
        configCabalConfigOpts = coerce configMonoidCabalConfigOpts
        configGhcOptionsByName = coerce configMonoidGhcOptionsByName
        configGhcOptionsByCat = coerce configMonoidGhcOptionsByCat
        configSetupInfoLocations = configMonoidSetupInfoLocations
        configSetupInfoInline = configMonoidSetupInfoInline
        configPvpBounds =
          fromFirst (PvpBounds PvpBoundsNone False) configMonoidPvpBounds
        configModifyCodePage = fromFirstTrue configMonoidModifyCodePage
        configRebuildGhcOptions = fromFirstFalse configMonoidRebuildGhcOptions
        configApplyGhcOptions = fromFirst AGOLocals configMonoidApplyGhcOptions
        configApplyProgOptions = fromFirst APOLocals configMonoidApplyProgOptions
        configAllowNewer = fromFirst False configMonoidAllowNewer
        configAllowNewerDeps = coerce configMonoidAllowNewerDeps
        configDefaultTemplate = getFirst configMonoidDefaultTemplate
        configDumpLogs = fromFirst DumpWarningLogs configMonoidDumpLogs
        configSaveHackageCreds = fromFirst True configMonoidSaveHackageCreds
        configHackageBaseUrl =
          fromFirst "https://hackage.haskell.org/" configMonoidHackageBaseUrl
        configHideSourcePaths = fromFirstTrue configMonoidHideSourcePaths
        configRecommendUpgrade = fromFirstTrue configMonoidRecommendUpgrade
        configNotifyIfNixOnPath = fromFirstTrue configMonoidNotifyIfNixOnPath
        configNotifyIfGhcUntested = fromFirstTrue configMonoidNotifyIfGhcUntested
        configNotifyIfCabalUntested = fromFirstTrue configMonoidNotifyIfCabalUntested
        configNoRunCompile = fromFirstFalse configMonoidNoRunCompile
    configAllowDifferentUser <-
      case getFirst configMonoidAllowDifferentUser of
        Just True -> pure True
        _ -> getInContainer
    configRunner' <- view runnerL
    useAnsi <- liftIO $ hSupportsANSI stderr
    let stylesUpdate' = (configRunner' ^. stylesUpdateL) <>
          configMonoidStyles
        useColor' = runnerUseColor configRunner'
        mUseColor = do
          colorWhen <- getFirst configMonoidColorWhen
          pure $ case colorWhen of
            ColorNever  -> False
            ColorAlways -> True
            ColorAuto  -> useAnsi
        useColor'' = fromMaybe useColor' mUseColor
        configRunner'' = configRunner'
          & processContextL .~ origEnv
          & stylesUpdateL .~ stylesUpdate'
          & useColorL .~ useColor''
        go = runnerGlobalOpts configRunner'
    pic <-
      case getFirst configMonoidPackageIndex of
        Nothing ->
          case getFirst configMonoidPackageIndices of
            Nothing -> pure defaultPackageIndexConfig
            Just [pic] -> do
              prettyWarn packageIndicesWarning
              pure pic
            Just x -> prettyThrowIO $ MultiplePackageIndices x
        Just pic -> pure pic
    mpantryRoot <- liftIO $ lookupEnv pantryRootEnvVar
    pantryRoot <-
      case mpantryRoot of
        Just dir ->
          case parseAbsDir dir of
            Nothing -> throwIO $ ParseAbsolutePathException pantryRootEnvVar dir
            Just x -> pure x
        Nothing -> pure $ configStackRoot </> relDirPantry
    let snapLoc =
          case getFirst configMonoidSnapshotLocation of
            Nothing -> defaultSnapshotLocation
            Just addr ->
              customSnapshotLocation
               where
                customSnapshotLocation (LTS x y) =
                  mkRSLUrl $ addr'
                    <> "/lts/" <> display x
                    <> "/" <> display y <> ".yaml"
                customSnapshotLocation (Nightly date) =
                  let (year, month, day) = toGregorian date
                  in  mkRSLUrl $ addr'
                        <> "/nightly/"
                        <> display year
                        <> "/" <> display month
                        <> "/" <> display day <> ".yaml"
                mkRSLUrl builder = RSLUrl (utf8BuilderToText builder) Nothing
                addr' = display $ T.dropWhileEnd (=='/') addr
    let configStackDeveloperMode =
          fromFirst stackDeveloperModeDefault configMonoidStackDeveloperMode
        configCasa = if fromFirstTrue $ casaMonoidEnable configMonoidCasaOpts
          then
            let casaRepoPrefix = fromFirst
                  (fromFirst defaultCasaRepoPrefix configMonoidCasaRepoPrefix)
                  (casaMonoidRepoPrefix configMonoidCasaOpts)
                casaMaxKeysPerRequest = fromFirst
                  defaultCasaMaxPerRequest
                  (casaMonoidMaxKeysPerRequest configMonoidCasaOpts)
            in  Just (casaRepoPrefix, casaMaxKeysPerRequest)
          else Nothing
    withNewLogFunc go useColor'' stylesUpdate' $ \logFunc -> do
      let configRunner = configRunner'' & logFuncL .~ logFunc
      withLocalLogFunc logFunc $ handleMigrationException $ do
        logDebug $ case configCasa of
          Nothing -> "Use of Casa server disabled."
          Just (repoPrefix, maxKeys) ->
               "Use of Casa server enabled: ("
            <> fromString (show repoPrefix)
            <> ", "
            <> fromString (show maxKeys)
            <> ")."
        withPantryConfig'
          pantryRoot
          pic
          (maybe HpackBundled HpackCommand $ getFirst configMonoidOverrideHpack)
          clConnectionCount
          configCasa
          snapLoc
          (\configPantryConfig -> initUserStorage
            (configStackRoot </> relFileStorage)
            (\configUserStorage -> inner Config {..}))

-- | Runs the provided action with the given 'LogFunc' in the environment
withLocalLogFunc :: HasLogFunc env => LogFunc -> RIO env a -> RIO env a
withLocalLogFunc logFunc = local (set logFuncL logFunc)

-- | Runs the provided action with a new 'LogFunc', given a 'StylesUpdate'.
withNewLogFunc :: MonadUnliftIO m
               => GlobalOpts
               -> Bool  -- ^ Use color
               -> StylesUpdate
               -> (LogFunc -> m a)
               -> m a
withNewLogFunc go useColor (StylesUpdate update) inner = do
  logOptions0 <- logOptionsHandle stderr False
  let logOptions
        = setLogUseColor useColor
        $ setLogLevelColors logLevelColors
        $ setLogSecondaryColor secondaryColor
        $ setLogAccentColors (const highlightColor)
        $ setLogUseTime (globalTimeInLog go)
        $ setLogMinLevel (globalLogLevel go)
        $ setLogVerboseFormat (globalLogLevel go <= LevelDebug)
        $ setLogTerminal (globalTerminal go)
          logOptions0
  withLogFunc logOptions inner
 where
  styles = defaultStyles // update
  logLevelColors :: LogLevel -> Utf8Builder
  logLevelColors level =
    fromString $ setSGRCode $ snd $ styles ! logLevelToStyle level
  secondaryColor = fromString $ setSGRCode $ snd $ styles ! Secondary
  highlightColor = fromString $ setSGRCode $ snd $ styles ! Highlight

-- | Get the default location of the local programs directory.
getDefaultLocalProgramsBase :: MonadThrow m
                            => Path Abs Dir
                            -> Platform
                            -> ProcessContext
                            -> m (Path Abs Dir)
getDefaultLocalProgramsBase configStackRoot configPlatform override =
  case configPlatform of
    -- For historical reasons, on Windows a subdirectory of LOCALAPPDATA is
    -- used instead of a subdirectory of STACK_ROOT. Unifying the defaults would
    -- mean that Windows users would manually have to move data from the old
    -- location to the new one, which is undesirable.
    Platform _ Windows -> do
      let envVars = view envVarsL override
      case T.unpack <$> Map.lookup "LOCALAPPDATA" envVars of
        Just t -> case parseAbsDir t of
          Nothing ->
            throwM $ ParseAbsolutePathException "LOCALAPPDATA" t
          Just lad ->
            pure $ lad </> relDirUpperPrograms </> relDirStackProgName
        Nothing -> pure defaultBase
    _ -> pure defaultBase
 where
  defaultBase = configStackRoot </> relDirPrograms

-- | Load the configuration, using current directory, environment variables,
-- and defaults as necessary.
loadConfig ::
     (HasRunner env, HasTerm env)
  => (Config -> RIO env a)
  -> RIO env a
loadConfig inner = do
  mstackYaml <- view $ globalOptsL.to globalStackYaml
  mproject <- loadProjectConfig mstackYaml
  mresolver <- view $ globalOptsL.to globalResolver
  configArgs <- view $ globalOptsL.to globalConfigMonoid
  (configRoot, stackRoot, userOwnsStackRoot) <-
    determineStackRootAndOwnership configArgs

  let (mproject', addConfigMonoid) =
        case mproject of
          PCProject (proj, fp, cm) -> (PCProject (proj, fp), (cm:))
          PCGlobalProject -> (PCGlobalProject, id)
          PCNoProject deps -> (PCNoProject deps, id)

  userConfigPath <- getDefaultUserConfigPath configRoot
  extraConfigs0 <- getExtraConfigs userConfigPath >>=
    mapM (\file -> loadConfigYaml (parseConfigMonoid (parent file)) file)
  let extraConfigs =
        -- non-project config files' existence of a docker section should never
        -- default docker to enabled, so make it look like they didn't exist
        map
          ( \c -> c {configMonoidDockerOpts =
              (configMonoidDockerOpts c) {dockerMonoidDefaultEnable = Any False}}
          )
          extraConfigs0

  let withConfig =
        configFromConfigMonoid
          stackRoot
          userConfigPath
          mresolver
          mproject'
          (mconcat $ configArgs : addConfigMonoid extraConfigs)

  withConfig $ \config -> do
    unless (stackVersion `withinRange` configRequireStackVersion config)
      (throwM (BadStackVersionException (configRequireStackVersion config)))
    unless (configAllowDifferentUser config) $ do
      unless userOwnsStackRoot $
        throwM (UserDoesn'tOwnDirectory stackRoot)
      forM_ (configProjectRoot config) $ \dir ->
        checkOwnership (dir </> configWorkDir config)
    inner config

-- | Load the build configuration, adds build-specific values to config loaded
-- by @loadConfig@. values.
withBuildConfig :: RIO BuildConfig a -> RIO Config a
withBuildConfig inner = do
  config <- ask

  -- If provided, turn the AbstractResolver from the command line into a
  -- Resolver that can be used below.

  -- The configResolver and mcompiler are provided on the command line. In order
  -- to properly deal with an AbstractResolver, we need a base directory (to
  -- deal with custom snapshot relative paths). We consider the current working
  -- directory to be the correct base. Let's calculate the mresolver first.
  mresolver <- forM (configResolver config) $ \aresolver -> do
    logDebug ("Using resolver: " <> display aresolver <> " specified on command line")
    makeConcreteResolver aresolver

  (project', stackYamlFP) <- case configProject config of
    PCProject (project, fp) -> do
      forM_ (projectUserMsg project) prettyWarnS
      pure (project, fp)
    PCNoProject extraDeps -> do
      p <-
        case mresolver of
          Nothing -> throwIO NoResolverWhenUsingNoProject
          Just _ -> getEmptyProject mresolver extraDeps
      pure (p, configUserConfigPath config)
    PCGlobalProject -> do
      logDebug "Run from outside a project, using implicit global project config"
      destDir <- getImplicitGlobalProjectDir config
      let dest :: Path Abs File
          dest = destDir </> stackDotYaml
          dest' :: FilePath
          dest' = toFilePath dest
      ensureDir destDir
      exists <- doesFileExist dest
      if exists
        then do
          iopc <- loadConfigYaml (parseProjectAndConfigMonoid destDir) dest
          ProjectAndConfigMonoid project _ <- liftIO iopc
          when (view terminalL config) $
            case configResolver config of
              Nothing ->
                logDebug $
                  "Using resolver: " <>
                  display (projectResolver project) <>
                  " from implicit global project's config file: " <>
                  fromString dest'
              Just _ -> pure ()
          pure (project, dest)
        else do
          prettyInfoL
            [ flow "Writing the configuration file for the implicit \
                   \global project to:"
            , pretty dest <> "."
            , flow "Note: You can change the snapshot via the"
            , style Shell "resolver"
            , flow "field there."
            ]
          p <- getEmptyProject mresolver []
          liftIO $ do
            writeBinaryFileAtomic dest $ byteString $ S.concat
              [ "# This is the implicit global project's config file, which is only used when\n"
              , "# 'stack' is run outside of a real project. Settings here do _not_ act as\n"
              , "# defaults for all projects. To change Stack's default settings, edit\n"
              , "# '", encodeUtf8 (T.pack $ toFilePath $ configUserConfigPath config), "' instead.\n"
              , "#\n"
              , "# For more information about Stack's configuration, see\n"
              , "# http://docs.haskellstack.org/en/stable/yaml_configuration/\n"
              , "#\n"
              , Yaml.encode p]
            writeBinaryFileAtomic (parent dest </> relFileReadmeTxt) $
              "This is the implicit global project, which is " <>
              "used only when 'stack' is run\noutside of a " <>
              "real project.\n"
          pure (p, dest)
  mcompiler <- view $ globalOptsL.to globalCompiler
  let project = project'
        { projectCompiler = mcompiler <|> projectCompiler project'
        , projectResolver = fromMaybe (projectResolver project') mresolver
        }
  extraPackageDBs <- mapM resolveDir' (projectExtraPackageDBs project)

  wanted <- lockCachedWanted stackYamlFP (projectResolver project) $
    fillProjectWanted stackYamlFP config project

  -- Unfortunately redoes getProjectWorkDir, since we don't have a BuildConfig
  -- yet
  workDir <- view workDirL
  let projectStorageFile = parent stackYamlFP </> workDir </> relFileStorage

  initProjectStorage projectStorageFile $ \projectStorage -> do
    let bc = BuildConfig
          { bcConfig = config
          , bcSMWanted = wanted
          , bcExtraPackageDBs = extraPackageDBs
          , bcStackYaml = stackYamlFP
          , bcCurator = projectCurator project
          , bcProjectStorage = projectStorage
          }
    runRIO bc inner
 where
  getEmptyProject ::
       Maybe RawSnapshotLocation
    -> [PackageIdentifierRevision]
    -> RIO Config Project
  getEmptyProject mresolver extraDeps = do
    r <- case mresolver of
      Just resolver -> do
        prettyInfoL
          [ flow "Using the snapshot"
          , style Current (fromString $ T.unpack $ textDisplay resolver)
          , flow "specified on the command line."
          ]
        pure resolver
      Nothing -> do
        r'' <- getLatestResolver
        prettyInfoL
          [ flow "Using the latest snapshot"
          , style Current (fromString $ T.unpack $ textDisplay r'') <> "."
          ]
        pure r''
    pure Project
      { projectUserMsg = Nothing
      , projectPackages = []
      , projectDependencies =
          map (RPLImmutable . flip RPLIHackage Nothing) extraDeps
      , projectFlags = mempty
      , projectResolver = r
      , projectCompiler = Nothing
      , projectExtraPackageDBs = []
      , projectCurator = Nothing
      , projectDropPackages = mempty
      }

fillProjectWanted ::
     (HasLogFunc env, HasPantryConfig env, HasProcessContext env)
  => Path Abs t
  -> Config
  -> Project
  -> Map RawPackageLocationImmutable PackageLocationImmutable
  -> WantedCompiler
  -> Map PackageName (Bool -> RIO env DepPackage)
  -> RIO env (SMWanted, [CompletedPLI])
fillProjectWanted stackYamlFP config project locCache snapCompiler snapPackages = do
  let bopts = configBuild config

  packages0 <- for (projectPackages project) $ \fp@(RelFilePath t) -> do
    abs' <- resolveDir (parent stackYamlFP) (T.unpack t)
    let resolved = ResolvedPath fp abs'
    pp <- mkProjectPackage YesPrintWarnings resolved (boptsHaddock bopts)
    pure (cpName $ ppCommon pp, pp)

  -- prefetch git repos to avoid cloning per subdirectory
  -- see https://github.com/commercialhaskell/stack/issues/5411
  let gitRepos = mapMaybe
        ( \case
            (RPLImmutable (RPLIRepo repo rpm)) -> Just (repo, rpm)
            _ -> Nothing
        )
        (projectDependencies project)
  logDebug ("Prefetching git repos: " <> display (T.pack (show gitRepos)))
  fetchReposRaw gitRepos

  (deps0, mcompleted) <- fmap unzip . forM (projectDependencies project) $ \rpl -> do
    (pl, mCompleted) <- case rpl of
       RPLImmutable rpli -> do
         (compl, mcompl) <-
           case Map.lookup rpli locCache of
             Just compl -> pure (compl, Just compl)
             Nothing -> do
               cpl <- completePackageLocation rpli
               if cplHasCabalFile cpl
                 then pure (cplComplete cpl, Just $ cplComplete cpl)
                 else do
                   warnMissingCabalFile rpli
                   pure (cplComplete cpl, Nothing)
         pure (PLImmutable compl, CompletedPLI rpli <$> mcompl)
       RPLMutable p ->
         pure (PLMutable p, Nothing)
    dp <- additionalDepPackage (shouldHaddockDeps bopts) pl
    pure ((cpName $ dpCommon dp, dp), mCompleted)

  checkDuplicateNames $
    map (second (PLMutable . ppResolvedDir)) packages0 ++
    map (second dpLocation) deps0

  let packages1 = Map.fromList packages0
      snPackages = snapPackages
        `Map.difference` packages1
        `Map.difference` Map.fromList deps0
        `Map.withoutKeys` projectDropPackages project

  snDeps <- for snPackages $ \getDep -> getDep (shouldHaddockDeps bopts)

  let deps1 = Map.fromList deps0 `Map.union` snDeps

  let mergeApply m1 m2 f =
        MS.merge MS.preserveMissing MS.dropMissing (MS.zipWithMatched f) m1 m2
      pFlags = projectFlags project
      packages2 = mergeApply packages1 pFlags $
        \_ p flags -> p{ppCommon=(ppCommon p){cpFlags=flags}}
      deps2 = mergeApply deps1 pFlags $
        \_ d flags -> d{dpCommon=(dpCommon d){cpFlags=flags}}

  checkFlagsUsedThrowing pFlags FSStackYaml packages1 deps1

  let pkgGhcOptions = configGhcOptionsByName config
      deps = mergeApply deps2 pkgGhcOptions $
        \_ d options -> d{dpCommon=(dpCommon d){cpGhcOptions=options}}
      packages = mergeApply packages2 pkgGhcOptions $
        \_ p options -> p{ppCommon=(ppCommon p){cpGhcOptions=options}}
      unusedPkgGhcOptions =
        pkgGhcOptions `Map.restrictKeys` Map.keysSet packages2
          `Map.restrictKeys` Map.keysSet deps2

  unless (Map.null unusedPkgGhcOptions) $
    throwM $ InvalidGhcOptionsSpecification (Map.keys unusedPkgGhcOptions)

  let wanted = SMWanted
        { smwCompiler = fromMaybe snapCompiler (projectCompiler project)
        , smwProject = packages
        , smwDeps = deps
        , smwSnapshotLocation = projectResolver project
        }

  pure (wanted, catMaybes mcompleted)


-- | Check if there are any duplicate package names and, if so, throw an
-- exception.
checkDuplicateNames :: MonadThrow m => [(PackageName, PackageLocation)] -> m ()
checkDuplicateNames locals =
  case filter hasMultiples $ Map.toList $ Map.fromListWith (++) $ map (second pure) locals of
    [] -> pure ()
    x -> prettyThrowM $ DuplicateLocalPackageNames x
 where
  hasMultiples (_, _:_:_) = True
  hasMultiples _ = False


-- | Get the Stack root, e.g. @~/.stack@, and determine whether the user owns it.
--
-- On Windows, the second value is always 'True'.
determineStackRootAndOwnership ::
     MonadIO m
  => ConfigMonoid
  -- ^ Parsed command-line arguments
  -> m (Path Abs Dir, Path Abs Dir, Bool)
determineStackRootAndOwnership clArgs = liftIO $ do
  (configRoot, stackRoot) <- do
    case getFirst (configMonoidStackRoot clArgs) of
      Just x -> pure (x, x)
      Nothing -> do
        mstackRoot <- lookupEnv stackRootEnvVar
        case mstackRoot of
          Nothing -> do
            wantXdg <- fromMaybe "" <$> lookupEnv stackXdgEnvVar
            if not (null wantXdg)
              then do
                xdgRelDir <- parseRelDir stackProgName
                (,)
                  <$> getXdgDir XdgConfig (Just xdgRelDir)
                  <*> getXdgDir XdgData (Just xdgRelDir)
              else do
                oldStyleRoot <- getAppUserDataDir stackProgName
                pure (oldStyleRoot, oldStyleRoot)
          Just x -> case parseAbsDir x of
            Nothing ->
              throwIO $ ParseAbsolutePathException stackRootEnvVar x
            Just parsed -> pure (parsed, parsed)

  (existingStackRootOrParentDir, userOwnsIt) <- do
    mdirAndOwnership <- findInParents getDirAndOwnership stackRoot
    case mdirAndOwnership of
      Just x -> pure x
      Nothing -> throwIO (BadStackRoot stackRoot)

  when (existingStackRootOrParentDir /= stackRoot) $
    if userOwnsIt
      then ensureDir stackRoot
      else throwIO $
        Won'tCreateStackRootInDirectoryOwnedByDifferentUser
          stackRoot
          existingStackRootOrParentDir

  configRoot' <- canonicalizePath configRoot
  stackRoot' <- canonicalizePath stackRoot
  pure (configRoot', stackRoot', userOwnsIt)

-- | @'checkOwnership' dir@ throws 'UserDoesn'tOwnDirectory' if @dir@ isn't
-- owned by the current user.
--
-- If @dir@ doesn't exist, its parent directory is checked instead.
-- If the parent directory doesn't exist either,
-- @'NoSuchDirectory' ('parent' dir)@ is thrown.
checkOwnership :: MonadIO m => Path Abs Dir -> m ()
checkOwnership dir = do
  mdirAndOwnership <- firstJustM getDirAndOwnership [dir, parent dir]
  case mdirAndOwnership of
    Just (_, True) -> pure ()
    Just (dir', False) -> throwIO (UserDoesn'tOwnDirectory dir')
    Nothing ->
      throwIO . NoSuchDirectory $ (toFilePathNoTrailingSep . parent) dir

-- | @'getDirAndOwnership' dir@ returns @'Just' (dir, 'True')@ when @dir@
-- exists and the current user owns it in the sense of 'isOwnedByUser'.
getDirAndOwnership ::
     MonadIO m
  => Path Abs Dir
  -> m (Maybe (Path Abs Dir, Bool))
getDirAndOwnership dir = liftIO $ forgivingAbsence $ do
    ownership <- isOwnedByUser dir
    pure (dir, ownership)

-- | Check whether the current user (determined with 'getEffectiveUserId') is
-- the owner for the given path.
--
-- Will always pure 'True' on Windows.
isOwnedByUser :: MonadIO m => Path Abs t -> m Bool
isOwnedByUser path = liftIO $
  if osIsWindows
    then pure True
    else do
      fileStatus <- getFileStatus (toFilePath path)
      user <- getEffectiveUserID
      pure (user == fileOwner fileStatus)

-- | 'True' if we are currently running inside a Docker container.
getInContainer :: MonadIO m => m Bool
getInContainer = liftIO (isJust <$> lookupEnv inContainerEnvVar)

-- | 'True' if we are currently running inside a Nix.
getInNixShell :: MonadIO m => m Bool
getInNixShell = liftIO (isJust <$> lookupEnv inNixShellEnvVar)

-- | Determine the extra config file locations which exist.
--
-- Returns most local first
getExtraConfigs :: HasTerm env
                => Path Abs File -- ^ use config path
                -> RIO env [Path Abs File]
getExtraConfigs userConfigPath = do
  defaultStackGlobalConfigPath <- getDefaultGlobalConfigPath
  liftIO $ do
    env <- getEnvironment
    mstackConfig <-
        maybe (pure Nothing) (fmap Just . parseAbsFile)
      $ lookup "STACK_CONFIG" env
    mstackGlobalConfig <-
        maybe (pure Nothing) (fmap Just . parseAbsFile)
      $ lookup "STACK_GLOBAL_CONFIG" env
    filterM doesFileExist
        $ fromMaybe userConfigPath mstackConfig
        : maybe [] pure (mstackGlobalConfig <|> defaultStackGlobalConfigPath)

-- | Load and parse YAML from the given config file. Throws
-- 'ParseConfigFileException' when there's a decoding error.
loadConfigYaml ::
     HasLogFunc env
  => (Value -> Yaml.Parser (WithJSONWarnings a))
  -> Path Abs File -> RIO env a
loadConfigYaml parser path = do
  eres <- loadYaml parser path
  case eres of
    Left err -> prettyThrowM (ParseConfigFileException path err)
    Right res -> pure res

-- | Load and parse YAML from the given file.
loadYaml ::
     HasLogFunc env
  => (Value -> Yaml.Parser (WithJSONWarnings a))
  -> Path Abs File
  -> RIO env (Either Yaml.ParseException a)
loadYaml parser path = do
  eres <- liftIO $ Yaml.decodeFileEither (toFilePath path)
  case eres  of
    Left err -> pure (Left err)
    Right val ->
      case Yaml.parseEither parser val of
        Left err -> pure (Left (Yaml.AesonException err))
        Right (WithJSONWarnings res warnings) -> do
          logJSONWarnings (toFilePath path) warnings
          pure (Right res)

-- | Get the location of the project config file, if it exists.
getProjectConfig :: HasTerm env
                 => StackYamlLoc
                 -- ^ Override stack.yaml
                 -> RIO env (ProjectConfig (Path Abs File))
getProjectConfig (SYLOverride stackYaml) = pure $ PCProject stackYaml
getProjectConfig SYLGlobalProject = pure PCGlobalProject
getProjectConfig SYLDefault = do
  env <- liftIO getEnvironment
  case lookup "STACK_YAML" env of
    Just fp -> do
      prettyInfoS
        "Getting the project-level configuration file from the \
        \STACK_YAML environment variable."
      PCProject <$> resolveFile' fp
    Nothing -> do
      currDir <- getCurrentDir
      maybe PCGlobalProject PCProject <$> findInParents getStackDotYaml currDir
 where
  getStackDotYaml dir = do
    let fp = dir </> stackDotYaml
        fp' = toFilePath fp
    logDebug $ "Checking for project config at: " <> fromString fp'
    exists <- doesFileExist fp
    if exists
      then pure $ Just fp
      else pure Nothing
getProjectConfig (SYLNoProject extraDeps) = pure $ PCNoProject extraDeps

-- | Find the project config file location, respecting environment variables
-- and otherwise traversing parents. If no config is found, we supply a default
-- based on current directory.
loadProjectConfig ::
     HasTerm env
  => StackYamlLoc
     -- ^ Override stack.yaml
  -> RIO env (ProjectConfig (Project, Path Abs File, ConfigMonoid))
loadProjectConfig mstackYaml = do
  mfp <- getProjectConfig mstackYaml
  case mfp of
    PCProject fp -> do
      currDir <- getCurrentDir
      logDebug $ "Loading project config file " <>
                  fromString (maybe (toFilePath fp) toFilePath (stripProperPrefix currDir fp))
      PCProject <$> load fp
    PCGlobalProject -> do
      logDebug "No project config file found, using defaults."
      pure PCGlobalProject
    PCNoProject extraDeps -> do
      logDebug "Ignoring config files"
      pure $ PCNoProject extraDeps
 where
  load fp = do
    iopc <- loadConfigYaml (parseProjectAndConfigMonoid (parent fp)) fp
    ProjectAndConfigMonoid project config <- liftIO iopc
    pure (project, fp, config)

-- | Get the location of the default Stack configuration file. If a file already
-- exists at the deprecated location, its location is returned. Otherwise, the
-- new location is returned.
getDefaultGlobalConfigPath ::
     HasTerm env
  => RIO env (Maybe (Path Abs File))
getDefaultGlobalConfigPath =
  case (defaultGlobalConfigPath, defaultGlobalConfigPathDeprecated) of
    (Just new, Just old) ->
      Just . fst <$>
        tryDeprecatedPath
          (Just "non-project global configuration file")
          doesFileExist
          new
          old
    (Just new,Nothing) -> pure (Just new)
    _ -> pure Nothing

-- | Get the location of the default user configuration file. If a file already
-- exists at the deprecated location, its location is returned. Otherwise, the
-- new location is returned.
getDefaultUserConfigPath ::
     HasTerm env
  => Path Abs Dir
  -> RIO env (Path Abs File)
getDefaultUserConfigPath stackRoot = do
  (path, exists) <- tryDeprecatedPath
    (Just "non-project configuration file")
    doesFileExist
    (defaultUserConfigPath stackRoot)
    (defaultUserConfigPathDeprecated stackRoot)
  unless exists $ do
    ensureDir (parent path)
    liftIO $ writeBinaryFileAtomic path defaultConfigYaml
  pure path

packagesParser :: Parser [String]
packagesParser = many (strOption
                   (long "package" <>
                     metavar "PACKAGE" <>
                     help "Add a package (can be specified multiple times)"))

defaultConfigYaml :: (IsString s, Semigroup s) => s
defaultConfigYaml =
  "# This file contains default non-project-specific settings for Stack, used\n" <>
  "# in all projects. For more information about Stack's configuration, see\n" <>
  "# http://docs.haskellstack.org/en/stable/yaml_configuration/\n" <>
  "\n" <>
  "# The following parameters are used by 'stack new' to automatically fill fields\n" <>
  "# in the Cabal file. We recommend uncommenting them and filling them out if\n" <>
  "# you intend to use 'stack new'.\n" <>
  "# See https://docs.haskellstack.org/en/stable/yaml_configuration/#templates\n" <>
  "templates:\n" <>
  "  params:\n" <>
  "#    author-name:\n" <>
  "#    author-email:\n" <>
  "#    copyright:\n" <>
  "#    github-username:\n" <>
  "\n" <>
  "# The following parameter specifies Stack's output styles; STYLES is a\n" <>
  "# colon-delimited sequence of key=value, where 'key' is a style name and\n" <>
  "# 'value' is a semicolon-delimited list of 'ANSI' SGR (Select Graphic\n" <>
  "# Rendition) control codes (in decimal). Use 'stack ls stack-colors --basic'\n" <>
  "# to see the current sequence.\n" <>
  "# stack-colors: STYLES\n"
