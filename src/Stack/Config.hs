{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | The general Stack configuration that starts everything off. This should
-- be smart to falback if there is no stack.yaml, instead relying on
-- whatever files are available.
--
-- If there is no stack.yaml, and there is a cabal.config, we
-- read in those constraints, and if there's a cabal.sandbox.config,
-- we read any constraints from there and also find the package
-- database from there, etc. And if there's nothing, we should
-- probably default to behaving like cabal, possibly with spitting out
-- a warning that "you should run `stk init` to make things better".
module Stack.Config
  (loadConfig
  ,loadConfigYaml
  ,packagesParser
  ,getImplicitGlobalProjectDir
  ,getSnapshots
  ,makeConcreteResolver
  ,checkOwnership
  ,getInContainer
  ,getInNixShell
  ,defaultConfigYaml
  ,getProjectConfig
  ,loadBuildConfig
  ) where

import           Control.Monad.Extra (firstJustM)
import           Stack.Prelude
import           Data.Aeson.Extended
import qualified Data.ByteString as S
import           Data.Coerce (coerce)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as MS
import qualified Data.Monoid
import           Data.Monoid.Map (MonoidMap(..))
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Yaml as Yaml
import           Distribution.System (OS (..), Platform (..), buildPlatform, Arch(OtherArch))
import qualified Distribution.Text
import           Distribution.Version (simplifyVersionRange, mkVersion')
import           GHC.Conc (getNumProcessors)
import           Lens.Micro ((.~))
import           Network.HTTP.StackClient (httpJSON, parseUrlThrow, getResponseBody)
import           Options.Applicative (Parser, strOption, long, help)
import           Path
import           Path.Extra (toFilePathNoTrailingSep)
import           Path.Find (findInParents)
import           Path.IO
import qualified Paths_stack as Meta
import           Stack.Config.Build
import           Stack.Config.Docker
import           Stack.Config.Nix
import           Stack.Constants
import           Stack.Build.Haddock (shouldHaddockDeps)
import           Stack.Lock (lockCachedWanted)
import           Stack.Storage (initStorage)
import           Stack.SourceMap
import           Stack.Types.Build
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.Docker
import           Stack.Types.Nix
import           Stack.Types.Resolver
import           Stack.Types.SourceMap
import           Stack.Types.Version
import           System.Console.ANSI (hSupportsANSIWithoutEmulation)
import           System.Environment
import           System.PosixCompat.Files (fileOwner, getFileStatus)
import           System.PosixCompat.User (getEffectiveUserID)
import           RIO.List (unzip)
import           RIO.PrettyPrint (stylesUpdateL, useColorL)
import           RIO.Process

-- | If deprecated path exists, use it and print a warning.
-- Otherwise, return the new path.
tryDeprecatedPath
    :: HasLogFunc env
    => Maybe T.Text -- ^ Description of file for warning (if Nothing, no deprecation warning is displayed)
    -> (Path Abs a -> RIO env Bool) -- ^ Test for existence
    -> Path Abs a -- ^ New path
    -> Path Abs a -- ^ Deprecated path
    -> RIO env (Path Abs a, Bool) -- ^ (Path to use, whether it already exists)
tryDeprecatedPath mWarningDesc exists new old = do
    newExists <- exists new
    if newExists
        then return (new, True)
        else do
            oldExists <- exists old
            if oldExists
                then do
                    case mWarningDesc of
                        Nothing -> return ()
                        Just desc ->
                            logWarn $
                                "Warning: Location of " <> display desc <> " at '" <>
                                fromString (toFilePath old) <>
                                "' is deprecated; rename it to '" <>
                                fromString (toFilePath new) <>
                                "' instead"
                    return (old, True)
                else return (new, False)

-- | Get the location of the implicit global project directory.
-- If the directory already exists at the deprecated location, its location is returned.
-- Otherwise, the new location is returned.
getImplicitGlobalProjectDir
    :: HasLogFunc env
    => Config -> RIO env (Path Abs Dir)
getImplicitGlobalProjectDir config =
    --TEST no warning printed
    liftM fst $ tryDeprecatedPath
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
    return $ getResponseBody result

-- | Turn an 'AbstractResolver' into a 'Resolver'.
makeConcreteResolver
    :: HasConfig env
    => AbstractResolver
    -> RIO env RawSnapshotLocation
makeConcreteResolver (ARResolver r) = pure r
makeConcreteResolver ar = do
    snapshots <- getSnapshots
    r <-
        case ar of
            ARResolver r -> assert False $ makeConcreteResolver (ARResolver r)
            ARGlobal -> do
                config <- view configL
                implicitGlobalDir <- getImplicitGlobalProjectDir config
                let fp = implicitGlobalDir </> stackDotYaml
                iopc <- loadConfigYaml (parseProjectAndConfigMonoid (parent fp)) fp
                ProjectAndConfigMonoid project _ <- liftIO iopc
                return $ projectResolver project
            ARLatestNightly -> return $ nightlySnapshotLocation $ snapshotsNightly snapshots
            ARLatestLTSMajor x ->
                case IntMap.lookup x $ snapshotsLts snapshots of
                    Nothing -> throwString $ "No LTS release found with major version " ++ show x
                    Just y -> return $ ltsSnapshotLocation x y
            ARLatestLTS
                | IntMap.null $ snapshotsLts snapshots -> throwString "No LTS releases found"
                | otherwise ->
                    let (x, y) = IntMap.findMax $ snapshotsLts snapshots
                     in return $ ltsSnapshotLocation x y
    logInfo $ "Selected resolver: " <> display r
    return r

-- | Get the latest snapshot resolver available.
getLatestResolver :: HasConfig env => RIO env RawSnapshotLocation
getLatestResolver = do
    snapshots <- getSnapshots
    let mlts = uncurry ltsSnapshotLocation <$>
               listToMaybe (reverse (IntMap.toList (snapshotsLts snapshots)))
    pure $ fromMaybe (nightlySnapshotLocation (snapshotsNightly snapshots)) mlts

-- Interprets ConfigMonoid options.
configFromConfigMonoid
    :: HasRunner env
    => Path Abs Dir -- ^ stack root, e.g. ~/.stack
    -> Path Abs File -- ^ user config file path, e.g. ~/.stack/config.yaml
    -> Maybe AbstractResolver
    -> ProjectConfig (Project, Path Abs File)
    -> ConfigMonoid
    -> (Config -> RIO env a)
    -> RIO env a
configFromConfigMonoid
    configStackRoot configUserConfigPath configResolver
    configProject ConfigMonoid{..} inner = do
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
     configWorkDir0 <- maybe (return relDirStackWork) (liftIO . parseRelDir) mstackWorkEnv
     let configWorkDir = fromFirst configWorkDir0 configMonoidWorkDir
         configLatestSnapshot = fromFirst
           "https://s3.amazonaws.com/haddock.stackage.org/snapshots.json"
           configMonoidLatestSnapshot
         clConnectionCount = fromFirst 8 configMonoidConnectionCount
         configHideTHLoading = fromFirstTrue configMonoidHideTHLoading

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
         configOverrideGccPath = getFirst configMonoidOverrideGccPath

         -- Only place in the codebase where platform is hard-coded. In theory
         -- in the future, allow it to be configured.
         (Platform defArch defOS) = buildPlatform
         arch = fromMaybe defArch
              $ getFirst configMonoidArch >>= Distribution.Text.simpleParse
         os = defOS
         configPlatform = Platform arch os

         configRequireStackVersion = simplifyVersionRange (getIntersectingVersionRange configMonoidRequireStackVersion)

         configCompilerCheck = fromFirst MatchMinor configMonoidCompilerCheck

     case arch of
         OtherArch "aarch64" -> return ()
         OtherArch unk -> logWarn $ "Warning: Unknown value for architecture setting: " <> displayShow unk
         _ -> return ()

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
                 return
                     (fromFirst
                         (dockerEnable configDocker || nixEnable configNix)
                         configMonoidSystemGHC)

     when (isJust configGHCVariant && configSystemGHC) $
         throwM ManualGHCVariantSettingsAreIncompatibleWithSystemGHC

     rawEnv <- liftIO getEnvironment
     pathsEnv <- either throwM return
               $ augmentPathMap (map toFilePath configMonoidExtraPath)
                                (Map.fromList (map (T.pack *** T.pack) rawEnv))
     origEnv <- mkProcessContext pathsEnv
     let configProcessContextSettings _ = return origEnv

     configLocalProgramsBase <- case getFirst configMonoidLocalProgramsBase of
       Nothing -> getDefaultLocalProgramsBase configStackRoot configPlatform origEnv
       Just path -> return path
     platformOnlyDir <- runReaderT platformOnlyRelDir (configPlatform, configPlatformVariant)
     let configLocalPrograms = configLocalProgramsBase </> platformOnlyDir

     configLocalBin <-
         case getFirst configMonoidLocalBinPath of
             Nothing -> do
                 localDir <- getAppUserDataDir "local"
                 return $ localDir </> relDirBin
             Just userPath ->
                 (case mproject of
                     -- Not in a project
                     Nothing -> resolveDir' userPath
                     -- Resolves to the project dir and appends the user path if it is relative
                     Just (_, configYaml) -> resolveDir (parent configYaml) userPath)
                 -- TODO: Either catch specific exceptions or add a
                 -- parseRelAsAbsDirMaybe utility and use it along with
                 -- resolveDirMaybe.
                 `catchAny`
                 const (throwIO (NoSuchDirectory userPath))

     configJobs <-
        case getFirst configMonoidJobs of
            Nothing -> liftIO getNumProcessors
            Just i -> return i
     let configConcurrentTests = fromFirst True configMonoidConcurrentTests

     let configTemplateParams = configMonoidTemplateParameters
         configScmInit = getFirst configMonoidScmInit
         configCabalConfigOpts = coerce configMonoidCabalConfigOpts
         configGhcOptionsByName = coerce configMonoidGhcOptionsByName
         configGhcOptionsByCat = coerce configMonoidGhcOptionsByCat
         configSetupInfoLocations = configMonoidSetupInfoLocations
         configPvpBounds = fromFirst (PvpBounds PvpBoundsNone False) configMonoidPvpBounds
         configModifyCodePage = fromFirstTrue configMonoidModifyCodePage
         configExplicitSetupDeps = configMonoidExplicitSetupDeps
         configRebuildGhcOptions = fromFirstFalse configMonoidRebuildGhcOptions
         configApplyGhcOptions = fromFirst AGOLocals configMonoidApplyGhcOptions
         configAllowNewer = fromFirst False configMonoidAllowNewer
         configDefaultTemplate = getFirst configMonoidDefaultTemplate
         configDumpLogs = fromFirst DumpWarningLogs configMonoidDumpLogs
         configSaveHackageCreds = fromFirst True configMonoidSaveHackageCreds
         configHackageBaseUrl = fromFirst "https://hackage.haskell.org/" configMonoidHackageBaseUrl
         configHideSourcePaths = fromFirstTrue configMonoidHideSourcePaths
         configRecommendUpgrade = fromFirstTrue configMonoidRecommendUpgrade

     configAllowDifferentUser <-
        case getFirst configMonoidAllowDifferentUser of
            Just True -> return True
            _ -> getInContainer

     configRunner' <- view runnerL

     useAnsi <- liftIO $ fromMaybe True <$>
                         hSupportsANSIWithoutEmulation stderr

     let stylesUpdate' = (configRunner' ^. stylesUpdateL) <>
           configMonoidStyles
         useColor' = runnerUseColor configRunner'
         mUseColor = do
            colorWhen <- getFirst configMonoidColorWhen
            return $ case colorWhen of
                ColorNever  -> False
                ColorAlways -> True
                ColorAuto  -> useAnsi
         configRunner = configRunner'
             & processContextL .~ origEnv
             & stylesUpdateL .~ stylesUpdate'
             & useColorL .~ fromMaybe useColor' mUseColor

     hsc <-
       case getFirst configMonoidPackageIndices of
         Nothing -> pure defaultHackageSecurityConfig
         Just [hsc] -> pure hsc
         Just x -> error $ "When overriding the default package index, you must provide exactly one value, received: " ++ show x
     mpantryRoot <- liftIO $ lookupEnv "PANTRY_ROOT"
     pantryRoot <-
       case mpantryRoot of
         Just dir ->
           case parseAbsDir dir of
             Nothing -> throwString $ "Failed to parse PANTRY_ROOT environment variable (expected absolute directory): " ++ show dir
             Just x -> pure x
         Nothing -> pure $ configStackRoot </> relDirPantry
     withPantryConfig
       pantryRoot
       hsc
       (maybe HpackBundled HpackCommand $ getFirst configMonoidOverrideHpack)
       clConnectionCount
       (\configPantryConfig -> initStorage
         (configStackRoot </> relFileStorage)
         (\configStorage -> inner Config {..}))

-- | Get the default location of the local programs directory.
getDefaultLocalProgramsBase :: MonadThrow m
                            => Path Abs Dir
                            -> Platform
                            -> ProcessContext
                            -> m (Path Abs Dir)
getDefaultLocalProgramsBase configStackRoot configPlatform override =
  let
    defaultBase = configStackRoot </> relDirPrograms
  in
    case configPlatform of
      -- For historical reasons, on Windows a subdirectory of LOCALAPPDATA is
      -- used instead of a subdirectory of STACK_ROOT. Unifying the defaults would
      -- mean that Windows users would manually have to move data from the old
      -- location to the new one, which is undesirable.
      Platform _ Windows ->
        case Map.lookup "LOCALAPPDATA" $ view envVarsL override of
          Just t ->
            case parseAbsDir $ T.unpack t of
              Nothing -> throwM $ stringException ("Failed to parse LOCALAPPDATA environment variable (expected absolute directory): " ++ show t)
              Just lad ->
                return $ lad </> relDirUpperPrograms </> relDirStackProgName
          Nothing -> return defaultBase
      _ -> return defaultBase

-- | Load the configuration, using current directory, environment variables,
-- and defaults as necessary.
loadConfig :: HasRunner env => (Config -> RIO env a) -> RIO env a
loadConfig inner = do
    mstackYaml <- view $ globalOptsL.to globalStackYaml
    mproject <- loadProjectConfig mstackYaml
    mresolver <- view $ globalOptsL.to globalResolver
    configArgs <- view $ globalOptsL.to globalConfigMonoid
    (stackRoot, userOwnsStackRoot) <- determineStackRootAndOwnership configArgs

    let (mproject', addConfigMonoid) =
          case mproject of
            PCProject (proj, fp, cm) -> (PCProject (proj, fp), (cm:))
            PCGlobalProject -> (PCGlobalProject, id)
            PCNoProject deps -> (PCNoProject deps, id)

    userConfigPath <- getDefaultUserConfigPath stackRoot
    extraConfigs0 <- getExtraConfigs userConfigPath >>=
        mapM (\file -> loadConfigYaml (parseConfigMonoid (parent file)) file)
    let extraConfigs =
          -- non-project config files' existence of a docker section should never default docker
          -- to enabled, so make it look like they didn't exist
          map (\c -> c {configMonoidDockerOpts =
                            (configMonoidDockerOpts c) {dockerMonoidDefaultEnable = Any False}})
              extraConfigs0

    let withConfig =
          configFromConfigMonoid
            stackRoot
            userConfigPath
            mresolver
            mproject'
            (mconcat $ configArgs : addConfigMonoid extraConfigs)

    withConfig $ \config -> do
      unless (mkVersion' Meta.version `withinRange` configRequireStackVersion config)
          (throwM (BadStackVersionException (configRequireStackVersion config)))
      unless (configAllowDifferentUser config) $ do
          unless userOwnsStackRoot $
              throwM (UserDoesn'tOwnDirectory stackRoot)
          forM_ (configProjectRoot config) $ \dir ->
              checkOwnership (dir </> configWorkDir config)
      inner config

-- | Load the build configuration, adds build-specific values to config loaded by @loadConfig@.
-- values.
loadBuildConfig :: RIO Config BuildConfig
loadBuildConfig = do
    config <- ask

    -- If provided, turn the AbstractResolver from the command line
    -- into a Resolver that can be used below.

    -- The configResolver and mcompiler are provided on the command
    -- line. In order to properly deal with an AbstractResolver, we
    -- need a base directory (to deal with custom snapshot relative
    -- paths). We consider the current working directory to be the
    -- correct base. Let's calculate the mresolver first.
    mresolver <- forM (configResolver config) $ \aresolver -> do
      logDebug ("Using resolver: " <> display aresolver <> " specified on command line")
      makeConcreteResolver aresolver

    (project', stackYamlFP) <- case configProject config of
      PCProject (project, fp) -> do
          forM_ (projectUserMsg project) (logWarn . fromString)
          return (project, fp)
      PCNoProject extraDeps -> do
          p <-
            case mresolver of
              Nothing -> throwIO NoResolverWhenUsingNoProject
              Just _ -> getEmptyProject mresolver extraDeps
          return (p, configUserConfigPath config)
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
                           Just _ -> return ()
                   return (project, dest)
               else do
                   logInfo ("Writing implicit global project config file to: " <> fromString dest')
                   logInfo "Note: You can change the snapshot via the resolver field there."
                   p <- getEmptyProject mresolver []
                   liftIO $ do
                       S.writeFile dest' $ S.concat
                           [ "# This is the implicit global project's config file, which is only used when\n"
                           , "# 'stack' is run outside of a real project.  Settings here do _not_ act as\n"
                           , "# defaults for all projects.  To change stack's default settings, edit\n"
                           , "# '", encodeUtf8 (T.pack $ toFilePath $ configUserConfigPath config), "' instead.\n"
                           , "#\n"
                           , "# For more information about stack's configuration, see\n"
                           , "# http://docs.haskellstack.org/en/stable/yaml_configuration/\n"
                           , "#\n"
                           , Yaml.encode p]
                       S.writeFile (toFilePath $ parent dest </> relFileReadmeTxt) $ S.concat
                           [ "This is the implicit global project, which is used only when 'stack' is run\n"
                           , "outside of a real project.\n" ]
                   return (p, dest)
    mcompiler <- view $ globalOptsL.to globalCompiler
    let project = project'
            { projectCompiler = mcompiler <|> projectCompiler project'
            , projectResolver = fromMaybe (projectResolver project') mresolver
            }
    extraPackageDBs <- mapM resolveDir' (projectExtraPackageDBs project)

    wanted <- lockCachedWanted stackYamlFP (projectResolver project) $
        fillProjectWanted stackYamlFP config project

    return BuildConfig
        { bcConfig = config
        , bcSMWanted = wanted
        , bcExtraPackageDBs = extraPackageDBs
        , bcStackYaml = stackYamlFP
        , bcCurator = projectCurator project
        }
  where
    getEmptyProject :: Maybe RawSnapshotLocation -> [PackageIdentifierRevision] -> RIO Config Project
    getEmptyProject mresolver extraDeps = do
      r <- case mresolver of
            Just resolver -> do
                logInfo ("Using resolver: " <> display resolver <> " specified on command line")
                return resolver
            Nothing -> do
                r'' <- getLatestResolver
                logInfo ("Using latest snapshot resolver: " <> display r'')
                return r''
      return Project
        { projectUserMsg = Nothing
        , projectPackages = []
        , projectDependencies = map (RPLImmutable . flip RPLIHackage Nothing) extraDeps
        , projectFlags = mempty
        , projectResolver = r
        , projectCompiler = Nothing
        , projectExtraPackageDBs = []
        , projectCurator = Nothing
        , projectDropPackages = mempty
        }

fillProjectWanted ::
       (HasProcessContext env, HasLogFunc env, HasPantryConfig env)
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

    (deps0, mcompleted) <- fmap unzip . forM (projectDependencies project) $ \rpl -> do
      (pl, mCompleted) <- case rpl of
         RPLImmutable rpli -> do
           compl <- maybe (completePackageLocation rpli) pure (Map.lookup rpli locCache)
           pure (PLImmutable compl, Just (rpli, compl))
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
        unusedPkgGhcOptions = pkgGhcOptions `Map.restrictKeys` Map.keysSet packages2
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
    case filter hasMultiples $ Map.toList $ Map.fromListWith (++) $ map (second return) locals of
        [] -> return ()
        x -> throwM $ DuplicateLocalPackageNames x
  where
    hasMultiples (_, _:_:_) = True
    hasMultiples _ = False


-- | Get the stack root, e.g. @~/.stack@, and determine whether the user owns it.
--
-- On Windows, the second value is always 'True'.
determineStackRootAndOwnership
    :: (MonadIO m)
    => ConfigMonoid
    -- ^ Parsed command-line arguments
    -> m (Path Abs Dir, Bool)
determineStackRootAndOwnership clArgs = liftIO $ do
    stackRoot <- do
        case getFirst (configMonoidStackRoot clArgs) of
            Just x -> return x
            Nothing -> do
                mstackRoot <- lookupEnv stackRootEnvVar
                case mstackRoot of
                    Nothing -> getAppUserDataDir stackProgName
                    Just x -> case parseAbsDir x of
                        Nothing -> throwString ("Failed to parse STACK_ROOT environment variable (expected absolute directory): " ++ show x)
                        Just parsed -> return parsed

    (existingStackRootOrParentDir, userOwnsIt) <- do
        mdirAndOwnership <- findInParents getDirAndOwnership stackRoot
        case mdirAndOwnership of
            Just x -> return x
            Nothing -> throwIO (BadStackRoot stackRoot)

    when (existingStackRootOrParentDir /= stackRoot) $
        if userOwnsIt
            then ensureDir stackRoot
            else throwIO $
                Won'tCreateStackRootInDirectoryOwnedByDifferentUser
                    stackRoot
                    existingStackRootOrParentDir

    stackRoot' <- canonicalizePath stackRoot
    return (stackRoot', userOwnsIt)

-- | @'checkOwnership' dir@ throws 'UserDoesn'tOwnDirectory' if @dir@
-- isn't owned by the current user.
--
-- If @dir@ doesn't exist, its parent directory is checked instead.
-- If the parent directory doesn't exist either, @'NoSuchDirectory' ('parent' dir)@
-- is thrown.
checkOwnership :: (MonadIO m) => Path Abs Dir -> m ()
checkOwnership dir = do
    mdirAndOwnership <- firstJustM getDirAndOwnership [dir, parent dir]
    case mdirAndOwnership of
        Just (_, True) -> return ()
        Just (dir', False) -> throwIO (UserDoesn'tOwnDirectory dir')
        Nothing ->
            (throwIO . NoSuchDirectory) $ (toFilePathNoTrailingSep . parent) dir

-- | @'getDirAndOwnership' dir@ returns @'Just' (dir, 'True')@ when @dir@
-- exists and the current user owns it in the sense of 'isOwnedByUser'.
getDirAndOwnership
    :: (MonadIO m)
    => Path Abs Dir
    -> m (Maybe (Path Abs Dir, Bool))
getDirAndOwnership dir = liftIO $ forgivingAbsence $ do
    ownership <- isOwnedByUser dir
    return (dir, ownership)

-- | Check whether the current user (determined with 'getEffectiveUserId') is
-- the owner for the given path.
--
-- Will always return 'True' on Windows.
isOwnedByUser :: MonadIO m => Path Abs t -> m Bool
isOwnedByUser path = liftIO $ do
    if osIsWindows
        then return True
        else do
            fileStatus <- getFileStatus (toFilePath path)
            user <- getEffectiveUserID
            return (user == fileOwner fileStatus)

-- | 'True' if we are currently running inside a Docker container.
getInContainer :: (MonadIO m) => m Bool
getInContainer = liftIO (isJust <$> lookupEnv inContainerEnvVar)

-- | 'True' if we are currently running inside a Nix.
getInNixShell :: (MonadIO m) => m Bool
getInNixShell = liftIO (isJust <$> lookupEnv inNixShellEnvVar)

-- | Determine the extra config file locations which exist.
--
-- Returns most local first
getExtraConfigs :: HasLogFunc env
                => Path Abs File -- ^ use config path
                -> RIO env [Path Abs File]
getExtraConfigs userConfigPath = do
  defaultStackGlobalConfigPath <- getDefaultGlobalConfigPath
  liftIO $ do
    env <- getEnvironment
    mstackConfig <-
        maybe (return Nothing) (fmap Just . parseAbsFile)
      $ lookup "STACK_CONFIG" env
    mstackGlobalConfig <-
        maybe (return Nothing) (fmap Just . parseAbsFile)
      $ lookup "STACK_GLOBAL_CONFIG" env
    filterM doesFileExist
        $ fromMaybe userConfigPath mstackConfig
        : maybe [] return (mstackGlobalConfig <|> defaultStackGlobalConfigPath)

-- | Load and parse YAML from the given config file. Throws
-- 'ParseConfigFileException' when there's a decoding error.
loadConfigYaml
    :: HasLogFunc env
    => (Value -> Yaml.Parser (WithJSONWarnings a)) -> Path Abs File -> RIO env a
loadConfigYaml parser path = do
    eres <- loadYaml parser path
    case eres of
        Left err -> liftIO $ throwM (ParseConfigFileException path err)
        Right res -> return res

-- | Load and parse YAML from the given file.
loadYaml
    :: HasLogFunc env
    => (Value -> Yaml.Parser (WithJSONWarnings a)) -> Path Abs File -> RIO env (Either Yaml.ParseException a)
loadYaml parser path = do
    eres <- liftIO $ Yaml.decodeFileEither (toFilePath path)
    case eres  of
        Left err -> return (Left err)
        Right val ->
            case Yaml.parseEither parser val of
                Left err -> return (Left (Yaml.AesonException err))
                Right (WithJSONWarnings res warnings) -> do
                    logJSONWarnings (toFilePath path) warnings
                    return (Right res)

-- | Get the location of the project config file, if it exists.
getProjectConfig :: HasLogFunc env
                 => StackYamlLoc
                 -- ^ Override stack.yaml
                 -> RIO env (ProjectConfig (Path Abs File))
getProjectConfig (SYLOverride stackYaml) = return $ PCProject stackYaml
getProjectConfig SYLGlobalProject = return PCGlobalProject
getProjectConfig SYLDefault = do
    env <- liftIO getEnvironment
    case lookup "STACK_YAML" env of
        Just fp -> do
            logInfo "Getting project config file from STACK_YAML environment"
            liftM PCProject $ resolveFile' fp
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
            then return $ Just fp
            else return Nothing
getProjectConfig (SYLNoProject extraDeps) = return $ PCNoProject extraDeps

-- | Find the project config file location, respecting environment variables
-- and otherwise traversing parents. If no config is found, we supply a default
-- based on current directory.
loadProjectConfig :: HasLogFunc env
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
            return PCGlobalProject
        PCNoProject extraDeps -> do
            logDebug "Ignoring config files"
            return $ PCNoProject extraDeps
  where
    load fp = do
        iopc <- loadConfigYaml (parseProjectAndConfigMonoid (parent fp)) fp
        ProjectAndConfigMonoid project config <- liftIO iopc
        return (project, fp, config)

-- | Get the location of the default stack configuration file.
-- If a file already exists at the deprecated location, its location is returned.
-- Otherwise, the new location is returned.
getDefaultGlobalConfigPath
    :: HasLogFunc env
    => RIO env (Maybe (Path Abs File))
getDefaultGlobalConfigPath =
    case (defaultGlobalConfigPath, defaultGlobalConfigPathDeprecated) of
        (Just new,Just old) ->
            liftM (Just . fst ) $
            tryDeprecatedPath
                (Just "non-project global configuration file")
                doesFileExist
                new
                old
        (Just new,Nothing) -> return (Just new)
        _ -> return Nothing

-- | Get the location of the default user configuration file.
-- If a file already exists at the deprecated location, its location is returned.
-- Otherwise, the new location is returned.
getDefaultUserConfigPath
    :: HasLogFunc env
    => Path Abs Dir -> RIO env (Path Abs File)
getDefaultUserConfigPath stackRoot = do
    (path, exists) <- tryDeprecatedPath
        (Just "non-project configuration file")
        doesFileExist
        (defaultUserConfigPath stackRoot)
        (defaultUserConfigPathDeprecated stackRoot)
    unless exists $ do
        ensureDir (parent path)
        liftIO $ S.writeFile (toFilePath path) defaultConfigYaml
    return path

packagesParser :: Parser [String]
packagesParser = many (strOption (long "package" <> help "Additional packages that must be installed"))

defaultConfigYaml :: S.ByteString
defaultConfigYaml = S.intercalate "\n"
     [ "# This file contains default non-project-specific settings for 'stack', used"
     , "# in all projects.  For more information about stack's configuration, see"
     , "# http://docs.haskellstack.org/en/stable/yaml_configuration/"
     , ""
     , "# The following parameters are used by \"stack new\" to automatically fill fields"
     , "# in the cabal config. We recommend uncommenting them and filling them out if"
     , "# you intend to use 'stack new'."
     , "# See https://docs.haskellstack.org/en/stable/yaml_configuration/#templates"
     , "templates:"
     , "  params:"
     , "#    author-name:"
     , "#    author-email:"
     , "#    copyright:"
     , "#    github-username:"
     , ""
     , "# The following parameter specifies stack's output styles; STYLES is a"
     , "# colon-delimited sequence of key=value, where 'key' is a style name and"
     , "# 'value' is a semicolon-delimited list of 'ANSI' SGR (Select Graphic"
     , "# Rendition) control codes (in decimal). Use \"stack ls stack-colors --basic\""
     , "# to see the current sequence."
     , "# stack-colors: STYLES"
     , ""
     ]
