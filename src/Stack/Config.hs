{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Stack.Config
Description : The general Stack configuration.
License     : BSD-3-Clause

The general Stack configuration that starts everything off. This should be smart
to fallback if there is no stack.yaml, instead relying on whatever files are
available.

If there is no stack.yaml, and there is a cabal.config, we read in those
constraints, and if there's a cabal.sandbox.config, we read any constraints from
there and also find the package database from there, etc. And if there's
nothing, we should probably default to behaving like cabal, possibly with
spitting out a warning that "you should run `stk init` to make things better".
-}

module Stack.Config
  ( loadConfig
  , loadConfigYaml
  , packagesParser
  , getImplicitGlobalProjectDir
  , getSnapshots
  , makeConcreteSnapshot
  , getRawSnapshot
  , checkOwnership
  , getInContainer
  , getInNixShell
  , defaultConfigYaml
  , getProjectConfig
  , withBuildConfig
  , withNewLogFunc
  , determineStackRootAndOwnership
  ) where

import           Control.Monad.Extra ( firstJustM )
import           Data.Aeson.Types ( Value )
import           Data.Aeson.WarningParser
                    ( WithJSONWarnings (..), logJSONWarnings )
import           Data.Array.IArray ( (!), (//) )
import qualified Data.ByteString as S
import           Data.ByteString.Builder ( byteString )
import           Data.Char ( isLatin1 )
import           Data.Coerce ( coerce )
import qualified Data.Either.Extra as EE
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as MS
import qualified Data.Monoid
import           Data.Monoid.Map ( MonoidMap (..) )
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Distribution.PackageDescription as PD
import           Distribution.System
                   ( Arch (..), OS (..), Platform (..), buildPlatform )
import qualified Distribution.Text ( simpleParse )
import           Distribution.Version ( simplifyVersionRange )
import qualified Hpack
import           GHC.Conc ( getNumProcessors )
import           Network.HTTP.StackClient
                   ( httpJSON, parseUrlThrow, getResponseBody )
import           Options.Applicative ( Parser, help, long, metavar, strOption )
import           Pantry ( loadSnapshot )
import           Path
                   ( PathException (..), (</>), parent, parseAbsDir
                   , parseAbsFile, parseRelDir, stripProperPrefix
                   )
import           Path.Extra ( toFilePathNoTrailingSep )
import           Path.Find ( findInParents )
import           Path.IO
                   ( XdgDirectory (..), canonicalizePath, doesFileExist
                   , ensureDir, forgivingAbsence, getAppUserDataDir
                   , getCurrentDir, getXdgDir, resolveDir, resolveDir'
                   , resolveFile, resolveFile'
                   )
import           RIO.List ( unzip, intersperse )
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
                   ( defaultGlobalConfigPath, defaultUserConfigPath
                   , implicitGlobalProjectDir, inContainerEnvVar
                   , inNixShellEnvVar, osIsWindows, pantryRootEnvVar
                   , platformVariantEnvVar, relDirBin, relDirStackWork
                   , relFileReadmeTxt, relFileStorage, relDirPantry
                   , relDirPrograms, relDirStackProgName, relDirUpperPrograms
                   , stackDeveloperModeDefault, stackDotYaml, stackProgName
                   , stackRootEnvVar, stackWorkEnvVar, stackXdgEnvVar
                   )
import qualified Stack.Constants as Constants
import           Stack.Lock ( lockCachedWanted )
import           Stack.Prelude
import           Stack.SourceMap ( additionalDepPackage, mkProjectPackage )
import           Stack.Storage.Project ( initProjectStorage )
import           Stack.Storage.User ( initUserStorage )
import           Stack.Storage.Util ( handleMigrationException )
import           Stack.Types.AllowNewerDeps ( AllowNewerDeps (..) )
import           Stack.Types.ApplyGhcOptions ( ApplyGhcOptions (..) )
import           Stack.Types.ApplyProgOptions ( ApplyProgOptions (..) )
import           Stack.Types.Build.Exception
                   ( BuildException (..), BuildPrettyException (..) )
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
                   , ParseAbsolutePathException (..)
                   )
import           Stack.Types.ConfigMonoid
                   ( ConfigMonoid (..), parseConfigMonoid )
import           Stack.Types.Casa ( CasaOptsMonoid (..) )
import           Stack.Types.Docker ( DockerOpts (..), DockerOptsMonoid (..) )
import           Stack.Types.DumpLogs ( DumpLogs (..) )
import           Stack.Types.GlobalOpts (  GlobalOpts (..) )
import           Stack.Types.MsysEnvironment
                   ( MsysEnvironment (..), msysEnvArch )
import           Stack.Types.Nix ( NixOpts (..) )
import           Stack.Types.Platform
                   ( PlatformVariant (..), platformOnlyRelDir )
import           Stack.Types.Project ( Project (..) )
import qualified Stack.Types.Project as Project ( Project (..) )
import           Stack.Types.ProjectAndConfigMonoid
                   ( ProjectAndConfigMonoid (..), parseProjectAndConfigMonoid )
import           Stack.Types.ProjectConfig ( ProjectConfig (..) )
import           Stack.Types.PvpBounds ( PvpBounds (..), PvpBoundsType (..) )
import           Stack.Types.Runner
                   ( HasRunner (..), Runner (..), globalOptsL, terminalL )
import           Stack.Types.Snapshot ( AbstractSnapshot (..), Snapshots (..) )
import           Stack.Types.SourceMap
                   ( CommonPackage (..), DepPackage (..), ProjectPackage (..)
                   , SMWanted (..)
                   )
import           Stack.Types.StackYamlLoc ( StackYamlLoc (..) )
import           Stack.Types.UnusedFlags ( FlagSource (..), UnusedFlags (..) )
import           Stack.Types.Version
                   ( IntersectingVersionRange (..), VersionCheck (..)
                   , stackVersion, withinRange
                   )
import           System.Console.ANSI ( hNowSupportsANSI, setSGRCode )
import           System.Environment ( getEnvironment, lookupEnv )
import           System.Info.ShortPathName ( getShortPathName )
import           System.PosixCompat.Files ( fileOwner, getFileStatus )
import           System.Posix.User ( getEffectiveUserID )

-- | Get the location of the implicit global project directory.
getImplicitGlobalProjectDir :: HasConfig env => RIO env (Path Abs Dir)
getImplicitGlobalProjectDir = view $ stackRootL . to implicitGlobalProjectDir

-- | Download the t'Snapshots' value from stackage.org.
getSnapshots :: HasConfig env => RIO env Snapshots
getSnapshots = do
  latestUrlText <- askLatestSnapshotUrl
  latestUrl <- parseUrlThrow (T.unpack latestUrlText)
  logDebug $ "Downloading snapshot versions file from " <> display latestUrlText
  result <- httpJSON latestUrl
  logDebug "Done downloading and parsing snapshot versions file"
  pure $ getResponseBody result

-- | Turn an 'AbstractSnapshot' into a 'RawSnapshotLocation'.
makeConcreteSnapshot ::
     HasConfig env
  => AbstractSnapshot
  -> RIO env RawSnapshotLocation
makeConcreteSnapshot (ASSnapshot s) = pure s
makeConcreteSnapshot as = do
  s <-
    case as of
      ASGlobal -> do
        fp <- getImplicitGlobalProjectDir <&> (</> stackDotYaml)
        iopc <- loadConfigYaml (parseProjectAndConfigMonoid (parent fp)) fp
        ProjectAndConfigMonoid project _ <- liftIO iopc
        pure project.snapshot
      ASLatestNightly ->
        RSLSynonym . Nightly . (.nightly) <$> getSnapshots
      ASLatestLTSMajor x -> do
        snapshots <- getSnapshots
        case IntMap.lookup x snapshots.lts of
          Nothing -> throwIO $ NoLTSWithMajorVersion x
          Just y -> pure $ RSLSynonym $ LTS x y
      ASLatestLTS -> do
        snapshots <- getSnapshots
        if IntMap.null snapshots.lts
          then throwIO NoLTSFound
          else let (x, y) = IntMap.findMax snapshots.lts
               in  pure $ RSLSynonym $ LTS x y
  prettyInfoL
    [ flow "Selected snapshot:"
    , style Current (fromString $ T.unpack $ textDisplay s) <> "."
    ]
  pure s

-- | Get the raw snapshot from the global options.
getRawSnapshot :: HasConfig env => RIO env (Maybe RawSnapshot)
getRawSnapshot = do
  mASnapshot <- view $ globalOptsL . to (.snapshot)
  forM mASnapshot $ \aSnapshot -> do
    concrete <- makeConcreteSnapshot aSnapshot
    loc <- completeSnapshotLocation concrete
    loadSnapshot loc

-- | Get the latest snapshot available.
getLatestSnapshot :: HasConfig env => RIO env RawSnapshotLocation
getLatestSnapshot = do
  snapshots <- getSnapshots
  let mlts = uncurry LTS <$>
             listToMaybe (reverse (IntMap.toList snapshots.lts))
  pure $ RSLSynonym $ fromMaybe (Nightly snapshots.nightly) mlts

-- Interprets ConfigMonoid options.
configFromConfigMonoid ::
     (HasRunner env, HasTerm env)
  => Path Abs Dir -- ^ Stack root, e.g. ~/.stack
  -> Path Abs File
     -- ^ User-specific global configuration file.
  -> Maybe AbstractSnapshot
  -> ProjectConfig (Project, Path Abs File)
  -> ConfigMonoid
  -> (Config -> RIO env a)
  -> RIO env a
configFromConfigMonoid
  stackRoot
  userGlobalConfigFile
  snapshot
  project
  configMonoid
  inner
  = do
    -- If --stack-work is passed, prefer it. Otherwise, if STACK_WORK
    -- is set, use that. If neither, use the default ".stack-work"
    mstackWorkEnv <- liftIO $ lookupEnv stackWorkEnvVar
    let mproject =
          case project of
            PCProject pair -> Just pair
            PCGlobalProject -> Nothing
            PCNoProject _deps -> Nothing
        allowLocals =
          case project of
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
    let workDir = fromFirst configWorkDir0 configMonoid.workDir
        -- The history of the URL below is as follows:
        --
        -- * Before Stack 1.3.0 it was
        --   https://www.stackage.org/download/snapshots.json.
        -- * From Stack 1.3.0 to 2.15.3 it was
        --   https://s3.amazonaws.com/haddock.stackage.org/snapshots.json. The
        --   change was made because S3 was expected to have greater uptime than
        --   stackage.org.
        -- * In early 2024, the Stackage project was handed over to the Haskell
        --   Foundation. Following that handover, the URL below was considered
        --   the most reliable source of the file in question.
        latestSnapshot = fromFirst
          "https://stackage-haddock.haskell.org/snapshots.json"
          configMonoid.latestSnapshot
        clConnectionCount = fromFirst 8 configMonoid.connectionCount
        hideTHLoading = fromFirstTrue configMonoid.hideTHLoading
        prefixTimestamps = fromFirst False configMonoid.prefixTimestamps
        ghcVariant = getFirst configMonoid.ghcVariant
        compilerRepository = fromFirst
          defaultCompilerRepository
          configMonoid.compilerRepository
        ghcBuild = getFirst configMonoid.ghcBuild
        installGHC = fromFirstTrue configMonoid.installGHC
        installMsys = fromFirst installGHC configMonoid.installMsys
        skipGHCCheck = fromFirstFalse configMonoid.skipGHCCheck
        skipMsys = fromFirstFalse configMonoid.skipMsys
        defMsysEnvironment = case platform of
          Platform I386 Windows -> Just MINGW32
          Platform X86_64 Windows -> Just MINGW64
          _ -> Nothing
        extraIncludeDirs = configMonoid.extraIncludeDirs
        extraLibDirs = configMonoid.extraLibDirs
        customPreprocessorExts = configMonoid.customPreprocessorExts
        overrideGccPath = getFirst configMonoid.overrideGccPath
        -- Only place in the codebase where platform is hard-coded. In theory in
        -- the future, allow it to be configured.
        (Platform defArch defOS) = buildPlatform
        arch = fromMaybe defArch
          $ getFirst configMonoid.arch >>= Distribution.Text.simpleParse
        os = defOS
        platform = Platform arch os
        requireStackVersion = simplifyVersionRange
          configMonoid.requireStackVersion.intersectingVersionRange
        compilerCheck = fromFirst MatchMinor configMonoid.compilerCheck
    msysEnvironment <- case defMsysEnvironment of
      -- Ignore the configuration setting if there is no default for the
      -- platform.
      Nothing -> pure Nothing
      Just defMsysEnv -> do
        let msysEnv = fromFirst defMsysEnv configMonoid.msysEnvironment
        if msysEnvArch msysEnv == arch
          then pure $ Just msysEnv
          else prettyThrowM $ BadMsysEnvironment msysEnv arch
    platformVariant <- liftIO $
      maybe PlatformVariantNone PlatformVariant <$> lookupEnv platformVariantEnvVar
    let build = buildOptsFromMonoid configMonoid.buildOpts
    docker <-
      dockerOptsFromMonoid (fmap fst mproject) snapshot configMonoid.dockerOpts
    nix <- nixOptsFromMonoid configMonoid.nixOpts os
    systemGHC <-
      case (getFirst configMonoid.systemGHC, nix.enable) of
        (Just False, True) ->
          throwM NixRequiresSystemGhc
        _ ->
          pure
            (fromFirst
              (docker.enable || nix.enable)
              configMonoid.systemGHC)
    when (isJust ghcVariant && systemGHC) $
      throwM ManualGHCVariantSettingsAreIncompatibleWithSystemGHC
    rawEnv <- liftIO getEnvironment
    pathsEnv <- either throwM pure
      $ augmentPathMap (map toFilePath configMonoid.extraPath)
                       (Map.fromList (map (T.pack *** T.pack) rawEnv))
    origEnv <- mkProcessContext pathsEnv
    let processContextSettings _ = pure origEnv
    localProgramsBase <- case getFirst configMonoid.localProgramsBase of
      Nothing -> getDefaultLocalProgramsBase stackRoot platform origEnv
      Just path -> pure path
    let localProgramsFilePath = toFilePath localProgramsBase
        spaceInLocalProgramsPath = ' ' `elem` localProgramsFilePath
        nonLatin1InLocalProgramsPath = not $ all isLatin1 localProgramsFilePath
        problematicLocalProgramsPath =
             nonLatin1InLocalProgramsPath
          || (osIsWindows && spaceInLocalProgramsPath)
    when problematicLocalProgramsPath $ do
      let msgSpace =
            [ flow "It contains a space character. This will prevent building \
                   \with GHC 9.4.1 or later."
            | osIsWindows && spaceInLocalProgramsPath
            ]
      msgNoShort <- if osIsWindows && spaceInLocalProgramsPath
        then do
          ensureDir localProgramsBase
          -- getShortPathName returns the long path name when a short name does not
          -- exist.
          shortLocalProgramsFilePath <-
            liftIO $ getShortPathName localProgramsFilePath
          pure [ flow "It also has no alternative short ('8 dot 3') name. This \
                      \will cause problems with packages that use the GNU \
                      \project's 'configure' shell script."
               | ' ' `elem` shortLocalProgramsFilePath
               ]
        else pure []
      let msgNonLatin1 = if nonLatin1InLocalProgramsPath
            then
              [ flow "It contains at least one non-ISO/IEC 8859-1 (Latin-1) \
                     \character (Unicode code point > 255). This will cause \
                     \problems with packages that build using the"
              , style Shell "hsc2hs"
              , flow "tool with its default template"
              , style Shell "template-hsc.h" <> "."
              ]
            else []
      prettyWarn $
          "[S-8432]"
          <> line
          <> fillSep
               (  [ flow "Stack's 'programs' path is"
                  , style File (fromString localProgramsFilePath) <> "."
                  ]
               <> msgSpace
               <> msgNoShort
               <> msgNonLatin1
               )
          <> blankLine
          <> fillSep
               [ flow "To avoid sucn problems, use the"
               , style Shell "local-programs-path"
               , flow "non-project specific configuration option to specify an \
                      \alternative path without those characteristics."
               ]
          <> line
    platformOnlyDir <-
      runReaderT platformOnlyRelDir (platform, platformVariant)
    let localPrograms = localProgramsBase </> platformOnlyDir
    localBin <-
      case getFirst configMonoid.localBinPath of
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
    fileWatchHook <-
      case getFirst configMonoid.fileWatchHook of
        Nothing -> pure Nothing
        Just userPath ->
          ( case mproject of
              -- Not in a project
              Nothing -> Just <$> resolveFile' userPath
              -- Resolves to the project dir and appends the user path if it is
              -- relative
              Just (_, configYaml) ->
                Just <$> resolveFile (parent configYaml) userPath
          )
          -- TODO: Either catch specific exceptions or add a
          -- parseRelAsAbsFileMaybe utility and use it along with
          -- resolveFileMaybe.
          `catchAny`
          const (throwIO (NoSuchFile userPath))
    jobs <-
      case getFirst configMonoid.jobs of
        Nothing -> liftIO getNumProcessors
        Just i -> pure i
    let concurrentTests =
          fromFirst True configMonoid.concurrentTests
        templateParams = configMonoid.templateParameters
        scmInit = getFirst configMonoid.scmInit
        cabalConfigOpts = coerce configMonoid.cabalConfigOpts
        ghcOptionsByName = coerce configMonoid.ghcOptionsByName
        ghcOptionsByCat = coerce configMonoid.ghcOptionsByCat
        setupInfoLocations = configMonoid.setupInfoLocations
        setupInfoInline = configMonoid.setupInfoInline
        pvpBounds =
          fromFirst (PvpBounds PvpBoundsNone False) configMonoid.pvpBounds
        modifyCodePage = fromFirstTrue configMonoid.modifyCodePage
        rebuildGhcOptions =
          fromFirstFalse configMonoid.rebuildGhcOptions
        applyGhcOptions =
          fromFirst AGOLocals configMonoid.applyGhcOptions
        applyProgOptions =
          fromFirst APOLocals configMonoid.applyProgOptions
        allowNewer = configMonoid.allowNewer
        allowNewerDeps = coerce configMonoid.allowNewerDeps
    defaultInitSnapshot <- do
      root <- getCurrentDir
      let resolve = (First <$>) . traverse (resolvePaths (Just root)) . getFirst
      resolve configMonoid.defaultInitSnapshot
    let defaultTemplate = getFirst configMonoid.defaultTemplate
        dumpLogs = fromFirst DumpWarningLogs configMonoid.dumpLogs
        saveHackageCreds = configMonoid.saveHackageCreds
        hackageBaseUrl =
          fromFirst Constants.hackageBaseUrl configMonoid.hackageBaseUrl
        hideSourcePaths = fromFirstTrue configMonoid.hideSourcePaths
        recommendStackUpgrade = fromFirstTrue configMonoid.recommendStackUpgrade
        notifyIfNixOnPath = fromFirstTrue configMonoid.notifyIfNixOnPath
        notifyIfGhcUntested = fromFirstTrue configMonoid.notifyIfGhcUntested
        notifyIfCabalUntested = fromFirstTrue configMonoid.notifyIfCabalUntested
        notifyIfArchUnknown = fromFirstTrue configMonoid.notifyIfArchUnknown
        notifyIfNoRunTests = fromFirstTrue configMonoid.notifyIfNoRunTests
        notifyIfNoRunBenchmarks =
          fromFirstTrue configMonoid.notifyIfNoRunBenchmarks
        noRunCompile = fromFirstFalse configMonoid.noRunCompile
    allowDifferentUser <-
      case getFirst configMonoid.allowDifferentUser of
        Just True -> pure True
        _ -> getInContainer
    configRunner' <- view runnerL
    useAnsi <- liftIO $ hNowSupportsANSI stderr
    let stylesUpdate' = (configRunner' ^. stylesUpdateL) <>
          configMonoid.styles
        useColor' = configRunner'.useColor
        mUseColor = do
          colorWhen <- getFirst configMonoid.colorWhen
          pure $ case colorWhen of
            ColorNever  -> False
            ColorAlways -> True
            ColorAuto  -> useAnsi
        useColor'' = fromMaybe useColor' mUseColor
        configRunner'' = configRunner'
          & processContextL .~ origEnv
          & stylesUpdateL .~ stylesUpdate'
          & useColorL .~ useColor''
        go = configRunner'.globalOpts
        pic = fromFirst  defaultPackageIndexConfig configMonoid.packageIndex
    mpantryRoot <- liftIO $ lookupEnv pantryRootEnvVar
    pantryRoot <-
      case mpantryRoot of
        Just dir ->
          case parseAbsDir dir of
            Nothing -> throwIO $ ParseAbsolutePathException pantryRootEnvVar dir
            Just x -> pure x
        Nothing -> pure $ stackRoot </> relDirPantry
    let snapLoc =
          case getFirst configMonoid.snapshotLocation of
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
    globalHintsLoc <- case getFirst configMonoid.globalHintsLocation of
      Nothing -> pure defaultGlobalHintsLocation
      Just unresolverGlobalHintsLoc -> do
        resolvedGlobalHintsLocation <-
          resolvePaths (Just stackRoot) unresolverGlobalHintsLoc
        pure $ const resolvedGlobalHintsLocation
    let stackDeveloperMode = fromFirst
          stackDeveloperModeDefault
          configMonoid.stackDeveloperMode
        hpackForce = if fromFirstFalse configMonoid.hpackForce
          then Hpack.Force
          else Hpack.NoForce
        casa =
          if fromFirstTrue configMonoid.casaOpts.enable
            then
              let casaRepoPrefix = fromFirst
                    (fromFirst defaultCasaRepoPrefix configMonoid.casaRepoPrefix)
                    configMonoid.casaOpts.repoPrefix
                  casaMaxKeysPerRequest = fromFirst
                    defaultCasaMaxPerRequest
                    configMonoid.casaOpts.maxKeysPerRequest
              in  Just (casaRepoPrefix, casaMaxKeysPerRequest)
            else Nothing
    withNewLogFunc go useColor'' stylesUpdate' $ \logFunc -> do
      let runner = configRunner'' & logFuncL .~ logFunc
      withLocalLogFunc logFunc $ handleMigrationException $ do
        logDebug $ case casa of
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
          (maybe HpackBundled HpackCommand $ getFirst configMonoid.overrideHpack)
          hpackForce
          clConnectionCount
          casa
          snapLoc
          globalHintsLoc
          (\pantryConfig -> initUserStorage
            (stackRoot </> relFileStorage)
            ( \userStorage -> inner Config
                { workDir
                , userGlobalConfigFile
                , build
                , docker
                , nix
                , processContextSettings
                , localProgramsBase
                , localPrograms
                , hideTHLoading
                , prefixTimestamps
                , platform
                , platformVariant
                , ghcVariant
                , ghcBuild
                , latestSnapshot
                , systemGHC
                , installGHC
                , installMsys
                , skipGHCCheck
                , skipMsys
                , msysEnvironment
                , compilerCheck
                , compilerRepository
                , localBin
                , fileWatchHook
                , requireStackVersion
                , jobs
                , overrideGccPath
                , extraIncludeDirs
                , extraLibDirs
                , customPreprocessorExts
                , concurrentTests
                , templateParams
                , scmInit
                , ghcOptionsByName
                , ghcOptionsByCat
                , cabalConfigOpts
                , setupInfoLocations
                , setupInfoInline
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
                , project
                , allowLocals
                , saveHackageCreds
                , hackageBaseUrl
                , runner
                , pantryConfig
                , stackRoot
                , snapshot
                , userStorage
                , hideSourcePaths
                , recommendStackUpgrade
                , notifyIfNixOnPath
                , notifyIfGhcUntested
                , notifyIfCabalUntested
                , notifyIfArchUnknown
                , notifyIfNoRunTests
                , notifyIfNoRunBenchmarks
                , noRunCompile
                , stackDeveloperMode
                , casa
                }
            )
          )

-- | Runs the provided action with the given 'LogFunc' in the environment
withLocalLogFunc :: HasLogFunc env => LogFunc -> RIO env a -> RIO env a
withLocalLogFunc logFunc = local (set logFuncL logFunc)

-- | Runs the provided action with a new 'LogFunc', given a t'StylesUpdate'.
withNewLogFunc ::
     MonadUnliftIO m
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
        $ setLogUseTime go.timeInLog
        $ setLogMinLevel go.logLevel
        $ setLogVerboseFormat (go.logLevel <= LevelDebug)
        $ setLogTerminal go.terminal
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
getDefaultLocalProgramsBase ::
     MonadThrow m
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
  mstackYaml <- view $ globalOptsL . to (.stackYaml)
  mproject <- loadProjectConfig mstackYaml
  mASnapshot <- view $ globalOptsL . to (.snapshot)
  configArgs <- view $ globalOptsL . to (.configMonoid)
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
          (\c -> c {dockerOpts = c.dockerOpts { defaultEnable = Any False }})
          extraConfigs0

  let withConfig =
        configFromConfigMonoid
          stackRoot
          userConfigPath
          mASnapshot
          mproject'
          (mconcat $ configArgs : addConfigMonoid extraConfigs)

  withConfig $ \config -> do
    let Platform arch _ = config.platform
    case arch of
      OtherArch unknownArch
        | config.notifyIfArchUnknown ->
            prettyWarnL
              [ flow "Unknown value for architecture setting:"
              , style Shell (fromString unknownArch) <> "."
              , flow "To mute this message in future, set"
              , style Shell (flow "notify-if-arch-unknown: false")
              , flow "in Stack's configuration."
              ]
      _ -> pure ()
    unless (stackVersion `withinRange` config.requireStackVersion)
      (throwM (BadStackVersionException config.requireStackVersion))
    unless config.allowDifferentUser $ do
      unless userOwnsStackRoot $
        throwM (UserDoesn'tOwnDirectory stackRoot)
      forM_ (configProjectRoot config) $ \dir ->
        checkOwnership (dir </> config.workDir)
    inner config

-- | Load the build configuration, adds build-specific values to config loaded
-- by @loadConfig@. values.
withBuildConfig :: RIO BuildConfig a -> RIO Config a
withBuildConfig inner = do
  config <- ask

  -- If provided, turn the AbstractSnapshot from the command line into a
  -- snapshot that can be used below.

  -- The snapshot and mcompiler are provided on the command line. In order
  -- to properly deal with an AbstractSnapshot, we need a base directory (to
  -- deal with custom snapshot relative paths). We consider the current working
  -- directory to be the correct base. Let's calculate the mSnapshot first.
  mSnapshot <- forM config.snapshot $ \aSnapshot -> do
    logDebug $
          "Using snapshot: "
       <> display aSnapshot
       <> " specified on command line"
    makeConcreteSnapshot aSnapshot

  (project', configFile) <- case config.project of
    PCProject (project, fp) -> do
      forM_ project.userMsg prettyUserMessage
      pure (project, Right fp)
    PCNoProject extraDeps -> do
      p <-
        case mSnapshot of
          Nothing -> throwIO NoSnapshotWhenUsingNoProject
          Just _ -> getEmptyProject mSnapshot extraDeps
      pure (p, Left config.userGlobalConfigFile)
    PCGlobalProject -> do
      logDebug "Run from outside a project, using implicit global project config"
      destDir <- getImplicitGlobalProjectDir
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
            case config.snapshot of
              Nothing ->
                logDebug $
                     "Using snapshot: "
                  <> display project.snapshot
                  <> " from implicit global project's config file: "
                  <> fromString dest'
              Just _ -> pure ()
          pure (project, Right dest)
        else do
          prettyInfoL
            [ flow "Writing the configuration file for the implicit \
                   \global project to:"
            , pretty dest <> "."
            , flow "Note: You can change the snapshot via the"
            , style Shell "snapshot"
            , flow "key there."
            ]
          p <- getEmptyProject mSnapshot []
          liftIO $ do
            writeBinaryFileAtomic dest $ byteString $ S.concat
              [ "# This is the implicit global project's configuration file, which is only used\n"
              , "# when 'stack' is run outside of a real project. Settings here do _not_ act as\n"
              , "# defaults for all projects. To change Stack's default settings, edit\n"
              , "# '", encodeUtf8 (T.pack $ toFilePath config.userGlobalConfigFile), "' instead.\n"
              , "#\n"
              , "# For more information about Stack's configuration, see\n"
              , "# http://docs.haskellstack.org/en/stable/configure/yaml/\n"
              , "#\n"
              , Yaml.encode p]
            writeBinaryFileAtomic (parent dest </> relFileReadmeTxt) $
              "This is the implicit global project, which is " <>
              "used only when 'stack' is run\noutside of a " <>
              "real project.\n"
          pure (p, Right dest)
  mcompiler <- view $ globalOptsL . to (.compiler)
  let project :: Project
      project = project'
        { Project.compiler = mcompiler <|> project'.compiler
        , Project.snapshot = fromMaybe project'.snapshot mSnapshot
        }
      -- We are indifferent as to whether the configuration file is a
      -- user-specific global or a project-level one.
      eitherConfigFile = EE.fromEither configFile
  extraPackageDBs <- mapM resolveDir' project.extraPackageDBs

  smWanted <- lockCachedWanted eitherConfigFile project.snapshot $
    fillProjectWanted eitherConfigFile config project

  -- Unfortunately redoes getWorkDir, since we don't have a BuildConfig yet
  workDir <- view workDirL
  let projectStorageFile = parent eitherConfigFile </> workDir </> relFileStorage

  initProjectStorage projectStorageFile $ \projectStorage -> do
    let bc = BuildConfig
          { config
          , smWanted
          , extraPackageDBs
          , configFile
          , curator = project.curator
          , projectStorage
          }
    runRIO bc inner
 where
  getEmptyProject ::
       Maybe RawSnapshotLocation
    -> [RawPackageLocationImmutable]
    -> RIO Config Project
  getEmptyProject mSnapshot extraDeps = do
    snapshot <- case mSnapshot of
      Just snapshot -> do
        prettyInfoL
          [ flow "Using the snapshot"
          , style Current (fromString $ T.unpack $ textDisplay snapshot)
          , flow "specified on the command line."
          ]
        pure snapshot
      Nothing -> do
        r'' <- getLatestSnapshot
        prettyInfoL
          [ flow "Using the latest snapshot"
          , style Current (fromString $ T.unpack $ textDisplay r'') <> "."
          ]
        pure r''
    pure Project
      { userMsg = Nothing
      , packages = []
      , extraDeps = map RPLImmutable extraDeps
      , flagsByPkg = mempty
      , snapshot
      , compiler = Nothing
      , extraPackageDBs = []
      , curator = Nothing
      , dropPackages = mempty
      }
  prettyUserMessage :: String -> RIO Config ()
  prettyUserMessage userMsg = do
    let userMsgs = map flow $ splitAtLineEnds userMsg
        warningDoc = mconcat $ intersperse blankLine userMsgs
    prettyWarn warningDoc
   where
    splitAtLineEnds = reverse . map reverse . go []
     where
      go :: [String] -> String -> [String]
      go ss [] = ss
      go ss s = case go' [] s of
        ([], rest) -> go ss rest
        (s', rest) -> go (s' : ss) rest
      go' :: String -> String -> (String, String)
      go' s [] = (s, [])
      go' s [c] = (c:s, [])
      go' s "\n\n" = (s, [])
      go' s [c1, c2] = (c2:c1:s, [])
      go' s ('\n':'\n':rest) = (s, stripLineEnds rest)
      go' s ('\n':'\r':'\n':rest) = (s, stripLineEnds rest)
      go' s ('\r':'\n':'\n':rest) = (s, stripLineEnds rest)
      go' s ('\r':'\n':'\r':'\n':rest) = (s, stripLineEnds rest)
      go' s (c:rest) = go' (c:s) rest
      stripLineEnds :: String -> String
      stripLineEnds ('\n':rest) = stripLineEnds rest
      stripLineEnds ('\r':'\n':rest) = stripLineEnds rest
      stripLineEnds rest = rest

fillProjectWanted ::
     (HasLogFunc env, HasPantryConfig env, HasProcessContext env)
  => Path Abs File
     -- ^ Location of the configuration file, which may be either a
     -- user-specific global or a project-level one.
  -> Config
  -> Project
  -> Map RawPackageLocationImmutable PackageLocationImmutable
  -> WantedCompiler
  -> Map PackageName (Bool -> RIO env DepPackage)
  -> RIO env (SMWanted, [CompletedPLI])
fillProjectWanted configFile config project locCache snapCompiler snapPackages = do
  let bopts = config.build

  packages0 <- for project.packages $ \fp@(RelFilePath t) -> do
    abs' <- resolveDir (parent configFile) (T.unpack t)
    let resolved = ResolvedPath fp abs'
    pp <- mkProjectPackage YesPrintWarnings resolved bopts.buildHaddocks
    pure (pp.projectCommon.name, pp)

  -- prefetch git repos to avoid cloning per subdirectory
  -- see https://github.com/commercialhaskell/stack/issues/5411
  let gitRepos = mapMaybe
        ( \case
            (RPLImmutable (RPLIRepo repo rpm)) -> Just (repo, rpm)
            _ -> Nothing
        )
        project.extraDeps
  logDebug ("Prefetching git repos: " <> display (T.pack (show gitRepos)))
  fetchReposRaw gitRepos

  (deps0, mcompleted) <- fmap unzip . forM project.extraDeps $ \rpl -> do
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
    pure ((dp.depCommon.name, dp), mCompleted)

  checkDuplicateNames $
    map (second (PLMutable . (.resolvedDir))) packages0 ++
    map (second (.location)) deps0

  let packages1 = Map.fromList packages0
      snPackages = snapPackages
        `Map.difference` packages1
        `Map.difference` Map.fromList deps0
        `Map.withoutKeys` project.dropPackages

  snDeps <- for snPackages $ \getDep -> getDep (shouldHaddockDeps bopts)

  let deps1 = Map.fromList deps0 `Map.union` snDeps

  let mergeApply m1 m2 f =
        MS.merge MS.preserveMissing MS.dropMissing (MS.zipWithMatched f) m1 m2
      pFlags = project.flagsByPkg
      packages2 = mergeApply packages1 pFlags $ \_ p flags ->
        p { projectCommon = p.projectCommon { flags = flags } }
      deps2 = mergeApply deps1 pFlags $ \_ d flags ->
        d { depCommon = d.depCommon { flags = flags } }

  checkFlagsUsedThrowing pFlags packages1 deps1

  let pkgGhcOptions = config.ghcOptionsByName
      deps = mergeApply deps2 pkgGhcOptions $ \_ d options ->
        d { depCommon = d.depCommon { ghcOptions = options } }
      packages = mergeApply packages2 pkgGhcOptions $ \_ p options ->
        p { projectCommon = p.projectCommon { ghcOptions = options } }
      unusedPkgGhcOptions =
        pkgGhcOptions `Map.restrictKeys` Map.keysSet packages2
          `Map.restrictKeys` Map.keysSet deps2

  unless (Map.null unusedPkgGhcOptions) $
    throwM $ InvalidGhcOptionsSpecification (Map.keys unusedPkgGhcOptions)

  let wanted = SMWanted
        { compiler = fromMaybe snapCompiler project.compiler
        , project = packages
        , deps = deps
        , snapshotLocation = project.snapshot
        }

  pure (wanted, catMaybes mcompleted)

-- | Check if a package is a project package or a dependency and, if it is,
-- if all the specified flags are defined in the package's Cabal file.
checkFlagsUsedThrowing ::
     forall m. (MonadIO m, MonadThrow m)
  => Map PackageName (Map FlagName Bool)
  -> Map PackageName ProjectPackage
  -> Map PackageName DepPackage
  -> m ()
checkFlagsUsedThrowing packageFlags projectPackages deps = do
  unusedFlags <- forMaybeM (Map.toList packageFlags) getUnusedPackageFlags
  unless (null unusedFlags) $
    prettyThrowM $ InvalidFlagSpecification unusedFlags
 where
  getUnusedPackageFlags ::
       (PackageName, Map FlagName Bool)
    -> m (Maybe UnusedFlags)
  getUnusedPackageFlags (name, userFlags) = case maybeCommon of
    -- Package is not available as project or dependency
    Nothing -> pure $ Just $ UFNoPackage FSStackYaml name
    -- Package exists, let's check if the flags are defined
    Just common -> do
      gpd <- liftIO common.gpd
      let pname = pkgName $ PD.package $ PD.packageDescription gpd
          pkgFlags = Set.fromList $ map PD.flagName $ PD.genPackageFlags gpd
          unused = Map.keysSet $ Map.withoutKeys userFlags pkgFlags
      pure $ if Set.null unused
        -- All flags are defined, nothing to do
        then Nothing
        -- Error about the undefined flags
        else Just $ UFFlagsNotDefined FSStackYaml pname pkgFlags unused
   where
    maybeCommon =     fmap (.projectCommon) (Map.lookup name projectPackages)
                  <|> fmap (.depCommon) (Map.lookup name deps)

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
    case getFirst clArgs.stackRoot of
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

-- | Check whether the current user (determined with
-- 'System.Posix.User.getEffectiveUserId') is the owner for the given path.
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
getExtraConfigs ::
     HasTerm env
  => Path Abs File -- ^ use config path
  -> RIO env [Path Abs File]
getExtraConfigs userConfigPath = liftIO $ do
  env <- getEnvironment
  mstackConfig <-
      maybe (pure Nothing) (fmap Just . parseAbsFile)
    $ lookup "STACK_CONFIG" env
  mstackGlobalConfig <-
      maybe (pure Nothing) (fmap Just . parseAbsFile)
    $ lookup "STACK_GLOBAL_CONFIG" env
  filterM doesFileExist
    $ fromMaybe userConfigPath mstackConfig
    : maybe [] pure (mstackGlobalConfig <|> defaultGlobalConfigPath)

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
getProjectConfig ::
     HasTerm env
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

-- | Get the location of the default user global configuration file.
getDefaultUserConfigPath ::
     HasTerm env
  => Path Abs Dir
  -> RIO env (Path Abs File)
getDefaultUserConfigPath configRoot = do
  let userConfigPath = defaultUserConfigPath configRoot
  userConfigExists <- doesFileExist userConfigPath
  unless userConfigExists $ do
    ensureDir (parent userConfigPath)
    liftIO $ writeBinaryFileAtomic userConfigPath defaultConfigYaml
  pure userConfigPath

packagesParser :: Parser [String]
packagesParser = many (strOption
                   (long "package" <>
                     metavar "PACKAGE" <>
                     help "Add a package (can be specified multiple times)"))

defaultConfigYaml :: (IsString s, Semigroup s) => s
defaultConfigYaml =
  "# This file contains default non-project-specific settings for Stack, used\n" <>
  "# in all projects. For more information about Stack's configuration, see\n" <>
  "# http://docs.haskellstack.org/en/stable/configure/yaml/\n" <>
  "\n" <>
  "# The following parameters are used by 'stack new' to automatically fill fields\n" <>
  "# in the Cabal file. We recommend uncommenting them and filling them out if\n" <>
  "# you intend to use 'stack new'.\n" <>
  "# See https://docs.haskellstack.org/en/stable/configure/yaml/non-project/#templates\n" <>
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
