{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Build project(s).

module Stack.Build
  (build
  ,clean)
  where

import qualified Control.Applicative as A
import           Control.Arrow ((&&&))
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Exception.Enclosed (handleIO)
import           Control.Monad
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (asks, runReaderT)
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import           Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Conduit
import           Data.Conduit.Binary (sinkHandle)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Either
import           Data.Function
import           Data.IORef
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set as Set
import qualified Data.Streaming.Process as Process
import           Data.Streaming.Process hiding (env,callProcess)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Development.Shake (addOracle,Action)
import           Distribution.Package (Dependency (..))
import           Distribution.System (Platform (Platform), OS (Windows))
import           Distribution.Version (intersectVersionRanges)
import           Network.HTTP.Conduit (Manager)
import           Network.HTTP.Download
import           Path as FL
import           Prelude hiding (FilePath,writeFile)
import           Shake
import           Stack.Build.Types
import           Stack.BuildPlan
import           Stack.Constants
import           Stack.Fetch as Fetch
import           Stack.GhcPkg
import           Stack.Package
import           Stack.Types
import           Stack.Types.Internal
import           Stack.Types.StackT
import           System.Directory hiding (findFiles, findExecutable)
import           System.Exit (ExitCode (ExitSuccess))
import           System.IO
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process.Read

{- EKB FIXME: doc generation for stack-doc-server
#ifndef mingw32_HOST_OS
import           System.Posix.Files (createSymbolicLink,removeLink)
#endif
--}

-- | Build using Shake.
build :: (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m,HasLogLevel env)
      => BuildOpts -> m ()
build bopts = do
    cabalPkgVer <- getMinimalEnvOverride >>= getCabalPkgVer

    -- FIXME currently this will install all dependencies for the entire
    -- project even if just building a subset of the project
    locals <- determineLocals cabalPkgVer bopts
    localsWanted <- checkWanted locals bopts
    ranges <- getDependencyRanges locals
    dependencies <- getDependencies locals $
        M.unionWith (M.unionWith intersectVersionRanges)
            ranges
            (case boptsTargets bopts of
                Left _ -> M.empty
                Right names -> M.fromList $ map (, M.empty) names)

    installDependencies cabalPkgVer bopts dependencies
    toRemove <- getPackagesToRemove (Set.map packageIdentifier (S.fromList locals))
    buildLocals bopts localsWanted toRemove

-- | Given a list of local packages and some options, determine which ones are
-- wanted.
checkWanted :: (MonadIO m, MonadThrow m)
            => [Package] -> BuildOpts -> m (Map Package Wanted)
checkWanted packages bopts = do
    targets <- mapM parseTarget $
        case boptsTargets bopts of
            Left [] -> ["."]
            Left x -> x
            Right _ -> []
    (dirs, names0) <- case partitionEithers targets of
        ([], targets') -> return $ partitionEithers targets'
        (bad, _) -> throwM $ Couldn'tParseTargets bad

    -- Check for unknown names
    let names = Set.fromList names0
        known = Set.fromList $ map packageName packages
        unknown = Set.difference names known
    unless (Set.null unknown) $ throwM $ UnknownTargets $ Set.toList unknown

    return $ M.fromList $ map (id &&& wanted dirs names) packages
  where
    parseTarget t = do
        let s = T.unpack t
        isDir <- liftIO $ doesDirectoryExist s
        if isDir
            then liftM (Right . Left) $ liftIO (canonicalizePath s) >>= parseAbsDir
            else return $ case parsePackageNameFromString s of
                     Left _ -> Left t
                     Right pname -> Right $ Right pname
    wanted dirs names package = boolToWanted $
        packageName package `Set.member` names ||
        any (`FL.isParentOf` packageDir package) dirs ||
        any (== packageDir package) dirs
      where
        boolToWanted True = Wanted; boolToWanted _ = NotWanted

-- | Get currently user-local-db-installed packages that need to be
-- removed before we install the new package set.
getPackagesToRemove :: (MonadIO m, MonadLogger m, MonadReader env m, HasBuildConfig env, MonadThrow m, MonadCatch m)
                    => Set PackageIdentifier -> m (Set PackageIdentifier)
getPackagesToRemove toInstall = do
    menv <- getMinimalEnvOverride
    localDB <- packageDatabaseLocal
    depDB <- packageDatabaseDeps
    globalDB <- getGlobalDB menv
    let allDBs =
            [localDB, depDB, globalDB]
    installed <-
        getPackageVersionsSet menv allDBs (== localDB)
    $logDebug
        ("Installed: " <>
         T.pack (show installed))
    $logDebug
        ("Package databases: " <>
         T.pack (show allDBs))
    let toRemove =
            Set.filter
                (\ident ->
                      Set.member
                          (packageIdentifierName ident)
                          (Set.map packageIdentifierName toInstall) &&
                      not (Set.member ident toInstall))
                installed
    $logDebug
        ("To remove: " <>
         T.pack (show toRemove))
    return toRemove

-- | Determine all of the local packages we wish to install. This does not
-- include any dependencies.
determineLocals
    :: (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
    => PackageIdentifier -- ^ Cabal version
    -> BuildOpts
    -> m [Package]
determineLocals cabalPkgVer bopts = do
    bconfig <- asks getBuildConfig

    $logDebug "Unpacking packages as necessary"
    menv <- getMinimalEnvOverride
    mdist <- liftM Just $ distRelativeDir cabalPkgVer
    paths2 <- unpackPackageIdents menv (configLocalUnpackDir bconfig) mdist
            $ Set.fromList
            $ map fromTuple
            $ M.toList
            $ bcExtraDeps bconfig
    let paths = M.fromList (map (, PTUser) $ Set.toList $ bcPackages bconfig)
             <> M.fromList (map (, PTDep) $ M.elems paths2)
    $logDebug $ "Installing from local directories: " <> T.pack (show paths)
    locals <- forM (M.toList paths) $ \(dir, ptype) -> do
        cabalfp <- getCabalFileName dir
        name <- parsePackageNameFromFilePath cabalfp
        readPackage (packageConfig name bconfig ptype) cabalfp ptype
    $logDebug $ "Local packages to install: " <> T.intercalate ", "
        (map (packageIdentifierText . packageIdentifier) locals)
    return locals
  where
    finalAction = boptsFinalAction bopts

    packageConfig name bconfig PTDep = PackageConfig
        { packageConfigEnableTests = False
        , packageConfigEnableBenchmarks = False
        , packageConfigFlags =
               fromMaybe M.empty (M.lookup name $ bcFlags bconfig)
        , packageConfigGhcVersion = bcGhcVersion bconfig
        , packageConfigPlatform = configPlatform (getConfig bconfig)
        }
    packageConfig name bconfig PTUser = PackageConfig
        { packageConfigEnableTests =
            case finalAction of
                DoTests -> True
                _ -> False
        , packageConfigEnableBenchmarks =
            case finalAction of
                DoBenchmarks -> True
                _ -> False
        , packageConfigFlags =
               fromMaybe M.empty (M.lookup name $ bcFlags bconfig)
        , packageConfigGhcVersion = bcGhcVersion bconfig
        , packageConfigPlatform = configPlatform (getConfig bconfig)
        }

-- | Get the version ranges for all dependencies. This takes care of checking
-- for consistency amongst the local packages themselves, and removing locally
-- provided dependencies from that list.
--
-- Note that we return a Map from the package name of the dependency, to a Map
-- of the user and the required range. This allows us to give user friendly
-- error messages.
getDependencyRanges
    :: (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
    => [Package] -- ^ locals
    -> m (Map PackageName (Map PackageName VersionRange))
getDependencyRanges locals = do
    -- All version ranges demanded by our local packages. We keep track of where the range came from for nicer error messages
    let allRanges =
            M.unionsWith M.union $ flip map locals $ \l ->
            fmap (M.singleton (packageName l)) (packageDeps l)

    -- Check and then strip out any dependencies provided by a local package
    let stripLocal (errs, ranges) local' =
            (errs', ranges')
          where
            name = packageName local'
            errs' = checkMismatches local' (fromMaybe M.empty $ M.lookup name ranges)
                 ++ errs
            ranges' = M.delete name ranges
        checkMismatches :: Package
                        -> Map PackageName VersionRange
                        -> [StackBuildException]
        checkMismatches pkg users =
            mapMaybe go (M.toList users)
          where
            version = packageVersion pkg
            go (user, range)
                | withinRange version range = Nothing
                | otherwise = Just $ MismatchedLocalDep
                    (packageName pkg)
                    (packageVersion pkg)
                    user
                    range

    case foldl' stripLocal ([], allRanges) locals of
        ([], ranges) -> return ranges
        (errs, _) -> throwM $ DependencyIssues errs

-- | Determine all of the dependencies which need to be available.
--
-- This function checks that the dependency ranges will all be satisfies
getDependencies
    :: (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
    => [Package] -- ^ locals
    -> Map PackageName (Map PackageName VersionRange) -- ^ ranges
    -> m (Map PackageName (Version, Map FlagName Bool))
getDependencies locals ranges = do
    -- Get global packages
    menv <- getMinimalEnvOverride


    bconfig <- asks getBuildConfig
    dependencies <- case bcResolver bconfig of
        ResolverSnapshot snapName -> do
            $logDebug $ "Checking resolver: " <> renderSnapName snapName
            mbp0 <- loadMiniBuildPlan snapName
            globals <- getPackageVersionMapWithGlobalDb menv (Just mbp0) []
            let mbp = mbp0
                    { mbpPackages = mbpPackages mbp0 `Map.union`
                        fmap (\v -> MiniPackageInfo
                            { mpiVersion = v
                            , mpiFlags = Map.empty
                            , mpiPackageDeps = Set.empty
                            , mpiToolDeps = Set.empty
                            , mpiExes = Set.empty
                            }) globals
                    }

            let toolMap = getToolMap mbp
                shadowed = Set.fromList $ map packageName locals
                isShadowed = (`Set.member` shadowed)
                toolDeps = M.unionsWith Set.union
                         $ flip concatMap locals
                         $ \local -> flip concatMap (packageTools local)
                         $ \(Dependency name' _) ->
                             let name = packageNameByteString $ fromCabalPackageName name'
                              in case M.lookup name toolMap of
                                     Nothing -> []
                                     Just pkgs -> map
                                        (\pkg -> M.singleton pkg (Set.singleton $ packageName local))
                                        (Set.toList pkgs)
                localTools = M.fromList $ map (\p -> (packageName p, ())) locals
                toolDeps' = M.difference toolDeps localTools

            (deps, users) <- resolveBuildPlan menv mbp isShadowed $ M.unionWith Set.union
                (fmap M.keysSet ranges)
                toolDeps'
            forM_ (M.toList users) $ \(name, users') -> $logDebug $
                T.concat
                    [ packageNameText name
                    , " used by "
                    , T.intercalate ", " $ map packageNameText
                                         $ Set.toList users'
                    ]
            return deps
        ResolverGhc _ -> do
            globals <- getPackageVersionMapWithGlobalDb menv Nothing []
            return $ fmap (, M.empty) globals

    let checkDepRange (dep, users) =
            concatMap go $ M.toList users
          where
            go (user, range) =
                case M.lookup dep dependencies of
                    Nothing -> [MissingDep2 user dep range]
                    Just (version, _)
                        | withinRange version range -> []
                        | otherwise -> [MismatchedDep dep version user range]
    case concatMap checkDepRange $ M.toList ranges of
        [] -> return ()
        errs -> throwM $ DependencyIssues errs
    return dependencies

-- | Install the given set of dependencies into the dependency database, if missing.
installDependencies
    :: (MonadIO m,MonadReader env m,HasLogLevel env,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
    => PackageIdentifier -- ^ Cabal version
    -> BuildOpts
    -> Map PackageName (Version, Map FlagName Bool)
    -> m ()
installDependencies cabalPkgVer bopts deps' = do
    bconfig <- asks getBuildConfig
    mgr <- asks getHttpManager
    logLevel <- asks getLogLevel
    pkgDbs <- getPackageDatabases bconfig BTDeps
    menv <- getMinimalEnvOverride

    installed <- liftM toIdents $ getPackageVersionMapWithGlobalDb menv Nothing pkgDbs
    let toInstall' = M.difference deps installed

    -- Get rid of non-library dependencies which are already installed
    toInstall <- liftM M.unions $ forM (M.toList toInstall') $ \(ident, flags) -> do
        dest <- configPackageInstalled ident
        exists <- liftIO $ doesFileExist $ toFilePath dest
        return $ if exists
            then M.empty
            else M.singleton ident flags

    configureResource <- newResource "cabal configure" 1
    installResource <- newResource "cabal install" 1
    cfgVar <- liftIO $ newMVar ConfigLock

    if M.null toInstall
        then $logDebug "All dependencies are already installed"
        else do
            if boptsDryrun bopts
               then dryRunPrint "dependencies" mempty (S.fromList (M.keys toInstall))
               else do
                 $logInfo $ "Installing dependencies: " <> T.intercalate ", " (map packageIdentifierText (M.keys toInstall))
                 withTempUnpacked cabalPkgVer (M.keys toInstall) $ \newPkgDirs -> do
                   $logInfo "All dependencies unpacked"
                   packages <- liftM S.fromList $ forM newPkgDirs $ \dir -> do
                       cabalfp <- getCabalFileName dir
                       name <- parsePackageNameFromFilePath cabalfp
                       flags <- case M.lookup name deps' of
                           Nothing -> assert False $ return M.empty
                           Just (_, flags) -> return flags
                       readPackage (packageConfig flags bconfig) cabalfp PTDep
                   plans <- forM (S.toList packages) $ \package -> do
                       let gconfig = GenConfig -- FIXME
                               { gconfigOptimize = False
                               , gconfigLibProfiling = True
                               , gconfigExeProfiling = False
                               , gconfigGhcOptions = []
                               , gconfigFlags = packageFlags package
                               , gconfigPkgId = error "gconfigPkgId"
                               }
                       return $ makePlan -- FIXME dedupe this code with buildLocals
                           mgr
                           logLevel
                           cabalPkgVer
                           M.empty
                           Wanted
                           bopts
                           bconfig
                           BTDeps
                           gconfig
                           packages
                           package
                           configureResource
                           installResource
                           (userDocsDir (bcConfig bconfig))
                           cfgVar
                   runPlans bopts
                      (M.fromList $ map (, Wanted) $ Set.toList packages)
                      plans
                      (userDocsDir (bcConfig bconfig))
  where
    deps = M.fromList $ map (\(name, (version, flags)) -> (PackageIdentifier name version, flags))
                      $ M.toList deps'
    toIdents = M.fromList . map (\(name, version) -> (PackageIdentifier name version, ())) . M.toList

    packageConfig flags bconfig = PackageConfig
        { packageConfigEnableTests = False
        , packageConfigEnableBenchmarks = False
        , packageConfigFlags = flags
        , packageConfigGhcVersion = bcGhcVersion bconfig
        , packageConfigPlatform = configPlatform (getConfig bconfig)
        }

-- | Build all of the given local packages, assuming all necessary dependencies
-- are already installed.
buildLocals
    :: (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m,HasLogLevel env)
    => BuildOpts
    -> Map Package Wanted
    -> Set PackageIdentifier
    -> m ()
buildLocals bopts packagesToInstall packagesToRemove = do
     env <- ask
     bconfig <- asks getBuildConfig
     mgr <- asks getHttpManager
     logLevel <- asks getLogLevel
     menv <- getMinimalEnvOverride
     localDB <- packageDatabaseLocal
     depDB <- packageDatabaseDeps
     globalDB <- getGlobalDB menv
     -- Note that this unregistering must come before getting the list
     -- of 'pkgIds' below, because those ids are used for calculation
     -- of when a user package has been unregistered in the package
     -- database and therefore should be rebuilt and installed.
     unless (boptsDryrun bopts)
            (unregisterPackages menv [localDB,globalDB,depDB] (==localDB) packagesToRemove)
     pkgIds <- getGhcPkgIds menv [localDB]
                (map packageName (M.keys packagesToInstall))
     cabalPkgVer <- getCabalPkgVer menv
     configureResource <- newResource "cabal configure" 1
     installResource <- newResource "cabal install" 1
     cfgVar <- liftIO $ newMVar ConfigLock
     plans <-
       forM (M.toList packagesToInstall)
            (\(package, wantedTarget) ->
               do when (wantedTarget == Wanted && boptsFinalAction bopts /= DoNothing &&
                        packageType package == PTUser)
                       (liftIO (deleteGenFile cabalPkgVer (packageDir package)))
                  gconfig <- readGenConfigFile
                      cabalPkgVer
                      pkgIds
                      bopts
                      wantedTarget
                      package
                      cfgVar
                  return (makePlan mgr
                                   logLevel
                                   cabalPkgVer
                                   pkgIds
                                   wantedTarget
                                   bopts
                                   (getBuildConfig env)
                                   BTLocals
                                   gconfig
                                   (M.keysSet packagesToInstall)
                                   package
                                   configureResource
                                   installResource
                                   (userDocsDir (bcConfig bconfig))
                                   cfgVar))

     if boptsDryrun bopts
        then dryRunPrint "local packages" packagesToRemove (Set.map packageIdentifier (M.keysSet packagesToInstall))
        else runPlans bopts packagesToInstall plans (userDocsDir (bcConfig bconfig))

-- FIXME clean up this function to make it more nicely shareable
runPlans :: (MonadIO m, MonadReader env m, HasBuildConfig env, HasLogLevel env, HasHttpManager env, MonadLogger m, MonadBaseControl IO m)
         => BuildOpts
         -> Map Package Wanted
         -> [(PkgDepsOracle -> Action [PackageIdentifier]) -> Rules ()]
         -> Path Abs Dir
         -> m ()
runPlans _bopts packagesToInstall plans _docLoc = do
    shakeDir <- asks configShakeFilesDir
    shakeArgs
        shakeDir
        defaultShakeThreads
        (do getPkgDeps <- makeOracle
            mapM_ ($ getPkgDeps) plans)
        {- EKB FIXME: doc generation for stack-doc-server
        when
            (boptsFinalAction bopts == DoHaddock)
            (buildDocIndex
                 (wanted pwd)
                 docLoc
                 packages
                 mgr
                 logLevel)
                                  --}
  where makeOracle =
            addOracle
                (\(PkgDeps name) ->
                      case find ((==name).packageName) (M.keys packagesToInstall) of
                        Just package ->
                            return
                                (map packageIdentifier
                                     (mapMaybe
                                          (\depname ->
                                                find
                                                    ((== depname) . packageName)
                                                    (M.keys packagesToInstall))
                                          (M.keys (packageDeps package))))
                        Nothing -> return [])

-- | Dry run output.
dryRunPrint :: MonadLogger m => Text -> Set PackageIdentifier -> Set PackageIdentifier -> m ()
dryRunPrint label toRemove toInstall = do
    unless
        (Set.null toRemove)
        (do $logInfo ("The following " <> label <> " will be unregistered:")
            forM_
                (S.toList toRemove)
                ($logInfo .
                 packageIdentifierText))
    unless
        (Set.null toInstall)
        (do $logInfo ("The following " <> label <> " will be installed:")
            forM_
                (S.toList toInstall)
                ($logInfo .
                 packageIdentifierText))

-- | Reset the build (remove Shake database and .gen files).
clean :: forall m env.
         (MonadIO m, MonadReader env m, HasHttpManager env, HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m)
      => m ()
clean =
  do env <- ask
     menv <- getMinimalEnvOverride
     cabalPkgVer <- getCabalPkgVer menv
     forM_ (S.toList (bcPackages $ getBuildConfig env))
           (\pkgdir ->
              do deleteGenFile cabalPkgVer pkgdir
                 distDir' <- liftM FL.toFilePath
                       (distDirFromDir cabalPkgVer pkgdir)
                 liftIO $ do
                     exists <- doesDirectoryExist distDir'
                     when exists (removeDirectoryRecursive distDir'))
     shakeDir <- asks configShakeFilesDir
     liftIO (do exists <- doesDirectoryExist (toFilePath shakeDir)
                when exists
                     (removeDirectoryRecursive (toFilePath shakeDir)))

--------------------------------------------------------------------------------
-- Shake plan

-- | Whether the target is wanted or not.
data Wanted
  = NotWanted
  | Wanted
  deriving (Eq)

-- | Make a Shake plan for a package.
makePlan :: Manager
         -> LogLevel
         -> PackageIdentifier
         -> Map PackageName GhcPkgId
         -> Wanted
         -> BuildOpts
         -> BuildConfig
         -> BuildType
         -> GenConfig
         -> Set Package
         -> Package
         -> Resource
         -> Resource
         -> Path Abs Dir
         -> MVar ConfigLock
         -> (PkgDepsOracle -> Action [PackageIdentifier])
         -> Rules ()
makePlan mgr logLevel cabalPkgVer pkgIds wanted bopts bconfig buildType gconfig packages package installResource configureResource docLoc cfgVar getPkgDeps = do
    configureTarget <-
        either throw return $
        liftM FL.toFilePath
            (configuredFileFromDir cabalPkgVer dir)
    buildTarget <-
        either throw return $
        liftM FL.toFilePath
            (builtFileFromDir cabalPkgVer dir)
    when
        (wanted == Wanted)
        (want [buildTarget])
    configureTarget %>
        const (do void (getPkgDeps (PkgDeps (packageName package)))
                  runWithLogging configureAction)
    buildTarget %> const (runWithLogging (buildAction configureTarget))
  where
    needSourceFiles =
        need (map FL.toFilePath (S.toList (packageFiles package)))
    dir =
        packageDir package
    runWithLogging =
        runStackLoggingT mgr logLevel
    configureAction = do
        needDependencies cabalPkgVer pkgIds bopts packages package cfgVar
        need
            [ toFilePath
                  (packageCabalFile package)]
        (setuphs,removeAfterwards) <-
            liftIO (ensureSetupHs dir)
        actionFinally
            (configurePackage
                 cabalPkgVer
                 bconfig
                 configureResource
                 setuphs
                 buildType
                 package
                 gconfig
                 (if wanted == Wanted && packageType package == PTUser
                      then boptsFinalAction bopts
                      else DoNothing))
            removeAfterwards
    buildAction configureTarget = do
        need [configureTarget]
        needSourceFiles
        (setuphs,removeAfterwards) <-
            liftIO (ensureSetupHs dir)
        actionFinally
            (buildPackage
                 cabalPkgVer
                 bopts
                 bconfig
                 setuphs
                 buildType
                 packages
                 package
                 gconfig
                 (if wanted == Wanted && packageType package == PTUser
                      then boptsFinalAction bopts
                      else DoNothing)
                 installResource
                 docLoc)
            removeAfterwards
        writeFinalFiles cabalPkgVer cfgVar gconfig bconfig buildType dir package

-- | Specify that the given package needs the following other
-- packages.
needDependencies :: MonadAction m
                 => PackageIdentifier -- ^ Cabal version
                 -> Map PackageName GhcPkgId
                 -> BuildOpts
                 -> Set Package
                 -> Package
                 -> MVar ConfigLock
                 -> m ()
needDependencies cabalPkgVer pkgIds bopts packages package cfgVar =
  do deps <- mapM (\package' ->
                     let dir' = packageDir package'
                     in do genFile <- liftIO $ builtFileFromDir
                                cabalPkgVer
                                dir'
                           void (readGenConfigFile cabalPkgVer
                                                   pkgIds
                                                   bopts
                                                   NotWanted
                                                   package'
                                                   cfgVar)
                           return (FL.toFilePath genFile))
                  (mapMaybe (\name ->
                               find ((== name) . packageName)
                                    (S.toList packages))
                            (M.keys (packageDeps package)))
     need deps

--------------------------------------------------------------------------------
-- Build actions

getPackageDatabases :: MonadIO m => BuildConfig -> BuildType -> m [Path Abs Dir]
getPackageDatabases bconfig BTDeps =
    liftIO $ liftM return $ runReaderT packageDatabaseDeps bconfig
getPackageDatabases bconfig BTLocals = liftIO $ flip runReaderT bconfig $
    sequence
        [ packageDatabaseDeps
        , packageDatabaseLocal
        ]

getInstallRoot :: MonadIO m => BuildConfig -> BuildType -> m (Path Abs Dir)
getInstallRoot bconfig BTDeps = liftIO $ runReaderT installationRootDeps bconfig
getInstallRoot bconfig BTLocals = liftIO $ runReaderT installationRootLocal bconfig

-- | Write the final generated files after a build successfully
-- completes.
writeFinalFiles :: (MonadIO m)
                => PackageIdentifier -- ^ Cabal version
                -> MVar ConfigLock
                -> GenConfig
                -> BuildConfig
                -> BuildType
                -> Path Abs Dir
                -> Package
                -> m ()
writeFinalFiles cabalPkgVer cfgVar gconfig bconfig buildType dir package = liftIO $
         (do pkgDbs <- getPackageDatabases bconfig buildType
             menv <- runReaderT getMinimalEnvOverride bconfig
             mpkgid <- runNoLoggingT
                      $ flip runReaderT bconfig
                      $ findGhcPkgId
                            menv
                            pkgDbs
                            (packageName package)
             when (packageHasLibrary package && isNothing mpkgid)
                (throwIO (Couldn'tFindPkgId (packageName package)))

             -- Write out some record that we installed the package
             when (buildType == BTDeps && not (packageHasLibrary package)) $ do
                 dest <- flip runReaderT bconfig
                       $ configPackageInstalled $ PackageIdentifier
                            (packageName package)
                            (packageVersion package)
                 createDirectoryIfMissing True $ toFilePath $ parent dest
                 writeFile (toFilePath dest) "Installed"

             withMVar cfgVar (writeGenConfigFile
                                       cabalPkgVer
                                       dir
                                       gconfig {gconfigPkgId = mpkgid})
             updateGenFile cabalPkgVer dir)

-- | Build the given package with the given configuration.
configurePackage :: (MonadAction m)
                 => PackageIdentifier
                 -> BuildConfig
                 -> Resource
                 -> Path Abs File -- ^ Setup.hs file
                 -> BuildType
                 -> Package
                 -> GenConfig
                 -> FinalAction
                 -> m ()
configurePackage cabalPkgVer bconfig configureResource setuphs buildType package gconfig setupAction =
  do logPath <- liftIO $ runReaderT (buildLogPath package) bconfig
     liftIO (void (try (removeFile (FL.toFilePath logPath)) :: IO (Either IOException ())))
     pkgDbs <- getPackageDatabases bconfig buildType
     installRoot <- getInstallRoot bconfig buildType
     let runhaskell' = runhaskell False
                                  cabalPkgVer package setuphs bconfig buildType
     -- Uncertain as to why we cannot run configures in parallel. This appears
     -- to be a Cabal library bug. Original issue:
     -- https://github.com/fpco/stack/issues/84. Ideally we'd be able to remove
     -- this call to withResource.
     withResource configureResource 1 $ runhaskell'
       (concat [["configure","--user"]
               ,["--package-db=clear","--package-db=global"]
               ,map (("--package-db=" ++) . toFilePath) pkgDbs
               ,["--libdir=" ++ toFilePath (installRoot </> $(mkRelDir "lib"))
                ,"--bindir=" ++ toFilePath (installRoot </> bindirSuffix)
                ,"--datadir=" ++ toFilePath (installRoot </> $(mkRelDir "share"))
                ,"--docdir=" ++ toFilePath (installRoot </> $(mkRelDir "doc"))
                ]
               ,["--enable-library-profiling" | gconfigLibProfiling gconfig]
               ,["--enable-executable-profiling" | gconfigExeProfiling gconfig]
               ,["--enable-tests" | setupAction == DoTests]
               ,["--enable-benchmarks" | setupAction == DoBenchmarks]
               ,map (\(name,enabled) ->
                       "-f" <>
                       (if enabled
                           then ""
                           else "-") <>
                       flagNameString name)
                    (M.toList (packageFlags package))])

-- | Remove the dist/ dir of a package.
cleanPackage :: PackageIdentifier -- ^ Cabal version
             -> Package
             -> ConfigLock -- ^ Needed because this affects the config directory.
             -> IO ()
cleanPackage cabalPkgVer package _ = do
    dist <- distRelativeDir cabalPkgVer
    handleIO onErr $ removeDirectoryRecursive
        (toFilePath
             (packageDir package </> dist))
  where
    onErr e = putStrLn $ "FIXME Race condition https://github.com/fpco/stack/issues/155 triggered: " ++ show e

-- | Whether we're building dependencies (separate database and build
-- process), or locally specified packages.
data BuildType = BTDeps | BTLocals
    deriving (Eq)

-- | Build the given package with the given configuration.
buildPackage :: MonadAction m
             => PackageIdentifier
             -> BuildOpts
             -> BuildConfig
             -> Path Abs File -- ^ Setup.hs file
             -> BuildType
             -> Set Package
             -> Package
             -> GenConfig
             -> FinalAction
             -> Resource
             -> Path Abs Dir
             -> m ()
buildPackage cabalPkgVer bopts bconfig setuphs buildType _packages package gconfig setupAction installResource _docLoc =
  do logPath <- liftIO $ runReaderT (buildLogPath package) bconfig
     liftIO (void (try (removeFile (FL.toFilePath logPath)) :: IO (Either IOException ())))
     let runhaskell' live = runhaskell live cabalPkgVer package setuphs bconfig buildType
         singularBuild = S.size (bcPackages bconfig) == 1 && packageType package == PTUser
     runhaskell'
       singularBuild
       (concat [["build"]
               ,["--ghc-options=-O2" | gconfigOptimize gconfig]
               ,concat [["--ghc-options",T.unpack opt]
                        | opt <- boptsGhcOptions bopts
                        , packageType package == PTUser]])

     case setupAction of
       DoTests -> do
         let pkgRoot = packageDir package
         distRelativeDir' <- liftIO $ distRelativeDir cabalPkgVer
         let buildDir = pkgRoot </> distRelativeDir'
         menv <- liftIO $ configEnvOverride (getConfig bconfig) EnvSettings
            { esIncludeLocals = True
            , esIncludeGhcPackagePath = True
            }
         forM_ (Set.toList $ packageTests package) $ \testName -> do
           let exeExtension =
                case configPlatform $ getConfig bconfig of
                    Platform _ Windows -> ".exe"
                    _ -> ""
           nameDir <- liftIO $ parseRelDir $ T.unpack testName
           nameExe <- liftIO $ parseRelFile $ T.unpack testName ++ exeExtension
           let exeName = buildDir </> $(mkRelDir "build") </> nameDir </> nameExe
           exists <- liftIO $ doesFileExist $ toFilePath exeName
           if exists
               then runTestSuite
                        menv
                        bopts
                        (if singularBuild then Nothing else Just logPath)
                        pkgRoot
                        exeName
               else $logInfo $ T.concat
                    [ "Test suite "
                    , testName
                    , " executable not found found for "
                    , T.pack $ packageNameString $ packageName package
                    ]
           -- Previously just used this, but see https://github.com/fpco/stack/issues/167
           -- runhaskell' singularBuild ["test"]
       DoHaddock ->
           do
              {- EKB FIXME: doc generation for stack-doc-server
 #ifndef mingw32_HOST_OS
              liftIO (removeDocLinks docLoc package)
 #endif
              ifcOpts <- liftIO (haddockInterfaceOpts docLoc package packages)
              --}
              runhaskell'
                         singularBuild
                         ["haddock"
                         ,"--html"]
              {- EKB FIXME: doc generation for stack-doc-server
                         ,"--hoogle"
                         ,"--hyperlink-source"
                         ,"--html-location=../$pkg-$version/"
                         ,"--haddock-options=" ++ intercalate " " ifcOpts ]
              haddockLocs <-
                liftIO (findFiles (packageDocDir package)
                                  (\loc -> FilePath.takeExtensions (toFilePath loc) ==
                                           "." ++ haddockExtension)
                                  (not . isHiddenDir))
              forM_ haddockLocs $ \haddockLoc ->
                do let hoogleTxtPath = FilePath.replaceExtension (toFilePath haddockLoc) "txt"
                       hoogleDbPath = FilePath.replaceExtension hoogleTxtPath hoogleDbExtension
                   hoogleExists <- liftIO (doesFileExist hoogleTxtPath)
                   when hoogleExists
                        (callProcess
                             mempty -- FIXME: ?
                             "hoogle"
                             ["convert"
                             ,"--haddock"
                             ,hoogleTxtPath
                             ,hoogleDbPath])
              --}
       DoBenchmarks -> runhaskell' singularBuild ["bench"]
       _ -> return ()
     withResource installResource 1 (runhaskell' False ["install"])
     {- EKB FIXME: doc generation for stack-doc-server
 #ifndef mingw32_HOST_OS
     case setupAction of
       DoHaddock -> liftIO (createDocLinks docLoc package)
       _ -> return ()
 #endif
     --}

-- | Run a single test suite
runTestSuite :: MonadIO m
             => EnvOverride
             -> BuildOpts
             -> Maybe (Path Abs File) -- ^ optional log file, otherwise use console
             -> Path Abs Dir -- ^ working directory
             -> Path Abs File -- ^ executable
             -> m ()
runTestSuite menv bopts mlogFile pkgRoot fp = liftIO $ do
    case mlogFile of
        Nothing -> go Inherit
        Just logFile -> withBinaryFile (toFilePath logFile) AppendMode $ go . UseHandle
  where
    args = map T.unpack $ boptsTestArgs bopts
    go outerr = do
        (Just stdin', Nothing, Nothing, ph) <- createProcess (proc (toFilePath fp) args)
            { cwd = Just $ toFilePath pkgRoot
            , Process.env = envHelper menv
            , std_in = CreatePipe
            , std_out = outerr
            , std_err = outerr
            }
        hClose stdin'
        ec <- waitForProcess ph
        case ec of
            ExitSuccess -> return ()
            _ -> throwM $ TestSuiteFailure fp mlogFile ec

-- | Run the Haskell command for the given package.
runhaskell :: (HasBuildConfig config,MonadAction m)
           => Bool
           -> PackageIdentifier
           -> Package
           -> Path Abs File -- ^ Setup.hs or Setup.lhs file
           -> config
           -> BuildType
           -> [String]
           -> m ()
runhaskell liveOutput cabalPkgVer package setuphs config' buildType args =
  do buildDir <- liftIO (stackageBuildDir cabalPkgVer package)
     liftIO (createDirectoryIfMissing True (FL.toFilePath buildDir))
     $logInfo display
     outRef <- liftIO (newIORef mempty)
     errRef <- liftIO (newIORef mempty)
     join (liftIO (catch (runWithRefs outRef errRef)
                         (\e@ProcessExitedUnsuccessfully{} ->
                              return (dumpLog outRef errRef e))))
  where
    runWithRefs outRef errRef = do
        menv <- liftIO $ iomenv
        exeName <- liftIO $ join $ findExecutable menv "runhaskell"
        distRelativeDir' <- liftIO $ distRelativeDir cabalPkgVer
        let subEnv =
                 fmap (filter (\(x, _) -> x /= "GHC_PACKAGE_PATH"))
               $ envHelper menv
        withSink $ \sink -> withCheckedProcess
          (cp exeName distRelativeDir')
             {cwd = Just (FL.toFilePath (packageDir package))
             ,Process.env = subEnv}
          (\ClosedStream stdout' stderr' -> runConcurrently $
                Concurrently (logFrom stdout' sink outRef) A.*>
                Concurrently (logFrom stderr' sink errRef))
        return (return ())
    dumpLog outRef errRef e = do
        if liveOutput
           then return ()
           else do $logError (display <> ": ERROR")
                   errs <- liftIO (readIORef errRef)
                   outs <- liftIO (readIORef outRef)
                   unless (S8.null outs)
                          (do $logError "Stdout was:"
                              $logError (T.decodeUtf8 outs))
                   unless (S8.null errs)
                          (do $logError "Stderr was:"
                              $logError (T.decodeUtf8 errs))
        liftIO (throwIO e)
    withSink inner = do
         logPath <- liftIO $ runReaderT (buildLogPath package) config'
         liftIO $ createDirectoryIfMissing True $ FL.toFilePath
                $ parent logPath
         withBinaryFile (FL.toFilePath logPath) AppendMode (inner . stdoutToo)
      where stdoutToo h
                | not liveOutput = sinkHandle h
                | configHideTHLoading (getConfig config') =
                        CL.iterM (S8.hPut h)
                    =$= CB.lines
                    =$= CL.filter (not . isTHLoading)
                    =$= CL.mapM_ S8.putStrLn
                | otherwise = CL.iterM S8.putStr =$= sinkHandle h
    logFrom src sink ref =
        src $=
        CL.mapM (\chunk ->
                   do liftIO (modifyIORef' ref (<> chunk))
                      return chunk) $$
        sink
    display =
      packageIdentifierText (packageIdentifier package) <>
      ": " <>
      case args of
        (cname:_) -> T.pack cname
        _ -> mempty
    cp exeName distRelativeDir' =
      proc (toFilePath exeName)
        (("-package=" ++ packageIdentifierString cabalPkgVer)
         : "-clear-package-db"
         : "-global-package-db"
         -- TODO: Perhaps we want to include the snapshot package database here
         -- as well
         : toFilePath setuphs
         : ("--builddir=" ++ toFilePath distRelativeDir')
         : args)
    iomenv = configEnvOverride (getConfig config') EnvSettings
            { esIncludeLocals =
                case buildType of
                    BTDeps -> False
                    BTLocals -> True
            , esIncludeGhcPackagePath = False
            }

-- | Is this line a Template Haskell "Loading package" line
-- ByteString
isTHLoading :: S8.ByteString -> Bool
isTHLoading bs =
    "Loading package " `S8.isPrefixOf` bs &&
    ("done." `S8.isSuffixOf` bs || "done.\r" `S8.isSuffixOf` bs)

-- | Ensure Setup.hs exists in the given directory. Returns an action
-- to remove it later.
ensureSetupHs :: Path Abs Dir -> IO (Path Abs File, IO ())
ensureSetupHs dir =
  do exists1 <- doesFileExist (FL.toFilePath fp1)
     exists2 <- doesFileExist (FL.toFilePath fp2)
     if exists1 || exists2
        then return (if exists1 then fp1 else fp2, return ())
        else do writeFile (FL.toFilePath fp1) "import Distribution.Simple\nmain = defaultMain"
                return (fp1, removeFile (FL.toFilePath fp1))
  where fp1 = dir </> $(mkRelFile "Setup.hs")
        fp2 = dir </> $(mkRelFile "Setup.lhs")

{- EKB FIXME: doc generation for stack-doc-server
-- | Build the haddock documentation index and contents.
buildDocIndex :: (Package -> Wanted)
              -> Path Abs Dir
              -> Set Package
              -> Manager
              -> LogLevel
              -> Rules ()
buildDocIndex wanted docLoc packages mgr logLevel =
  do runHaddock "--gen-contents" $(mkRelFile "index.html")
     runHaddock "--gen-index" $(mkRelFile "doc-index.html")
     combineHoogle
  where
    runWithLogging = runStackLoggingT mgr logLevel
    runHaddock genOpt destFilename =
      do let destPath = toFilePath (docLoc </> destFilename)
         want [destPath]
         destPath %> \_ ->
           runWithLogging
               (do needDeps
                   ifcOpts <- liftIO (fmap concat (mapM toInterfaceOpt (S.toList packages)))
                   runIn docLoc
                         "haddock"
                         mempty
                         (genOpt:ifcOpts)
                         Nothing)
    toInterfaceOpt package =
      do let pv = joinPkgVer (packageName package,packageVersion package)
             srcPath = (toFilePath docLoc) ++ "/" ++
                       pv ++ "/" ++
                       packageNameString (packageName package) ++ "." ++
                       haddockExtension
         exists <- doesFileExist srcPath
         return (if exists
                    then ["-i"
                         ,"../" ++
                          pv ++
                          "," ++
                          srcPath]
                     else [])
    combineHoogle =
      do let destHoogleDbLoc = hoogleDatabaseFile docLoc
             destPath = FL.toFilePath destHoogleDbLoc
         want [destPath]
         destPath %> \_ ->
           runWithLogging
               (do needDeps
                   srcHoogleDbs <- liftIO (fmap concat (mapM toSrcHoogleDb (S.toList packages)))
                   callProcess
                        mempty -- FIXME: ?
                        "hoogle"
                        ("combine" :
                         "-o" :
                         FL.toFilePath destHoogleDbLoc :
                         srcHoogleDbs))
    toSrcHoogleDb package =
      do let srcPath = toFilePath docLoc ++ "/" ++
                       joinPkgVer (packageName package,packageVersion package) ++ "/" ++
                       packageNameString (packageName package) ++ "." ++
                       hoogleDbExtension
         exists <- doesFileExist srcPath
         return (if exists
                    then [srcPath]
                    else [])
    needDeps =
      need (concatMap (\package -> if wanted package == Wanted
                                    then let dir = packageDir package
                                         in [FL.toFilePath (builtFileFromDir dir)]
                                    else [])
                      (S.toList packages))

#ifndef mingw32_HOST_OS
-- | Remove existing links docs for package from @~/.shake/doc@.
removeDocLinks :: Path Abs Dir -> Package -> IO ()
removeDocLinks docLoc package =
  --EKB FIXME: only when running in Docker, for now.
  do createDirectoryIfMissing True
                              (FL.toFilePath docLoc)
     userDocLs <-
       fmap (map (FL.toFilePath docLoc ++))
            (getDirectoryContents (FL.toFilePath docLoc))
     forM_ userDocLs $
       \docPath ->
         do isDir <- doesDirectoryExist docPath
            when isDir
                 (case breakPkgVer (FilePath.takeFileName docPath) of
                    Just (p,_) ->
                      when (p == packageName package)
                           (removeLink docPath)
                    Nothing -> return ())

-- | Add link for package to @~/.shake/doc@.
createDocLinks :: Path Abs Dir -> Package -> IO ()
createDocLinks docLoc package =
  --EKB FIXME: only when running in Docker, for now.
  do let pkgVer =
           joinPkgVer (packageName package,(packageVersion package))
     pkgVerLoc <- liftIO (parseRelDir pkgVer)
     let pkgDestDocLoc = docLoc </> pkgVerLoc
         pkgDestDocPath =
           FilePath.dropTrailingPathSeparator (FL.toFilePath pkgDestDocLoc)
         cabalDocLoc = parent docLoc </>
                      --EKB FIXME: this does not work with .stack-work
                       $(mkRelDir "share/doc/")
     haddockLocs <-
       do cabalDocExists <- doesDirectoryExist (toFilePath cabalDocLoc)
          if cabalDocExists
             then findFiles cabalDocLoc
                            (\fileLoc ->
                               FilePath.takeExtensions (toFilePath fileLoc) ==
                               "." ++ haddockExtension &&
                               dirname (parent fileLoc) ==
                               $(mkRelDir "html/") &&
                               toFilePath (dirname (parent (parent fileLoc))) ==
                               (pkgVer ++ "/"))
                            (\dirLoc ->
                               not (isHiddenDir dirLoc) &&
                               dirname (parent (parent dirLoc)) /=
                               $(mkRelDir "html/"))
             else return []
     case haddockLocs of
       [haddockLoc] ->
         case FL.stripDir (parent docLoc)
                          haddockLoc of
           Just relHaddockPath ->
             do let srcRelPathCollapsed =
                      FilePath.takeDirectory (FilePath.dropTrailingPathSeparator (FL.toFilePath relHaddockPath))
                    {-srcRelPath = "../" ++ srcRelPathCollapsed-}
                createSymbolicLink (FilePath.dropTrailingPathSeparator srcRelPathCollapsed)
                                   pkgDestDocPath
           Nothing -> return ()
       _ -> return ()
#endif /* not defined(mingw32_HOST_OS) */

-- | Get @-i@ arguments for haddock for dependencies.
haddockInterfaceOpts :: Path Abs Dir -> Package -> Set Package -> IO [String]
haddockInterfaceOpts userDocLoc package packages =
  do mglobalDocLoc <- getGlobalDocPath
     globalPkgVers <-
       case mglobalDocLoc of
         Nothing -> return M.empty
         Just globalDocLoc -> getDocPackages globalDocLoc
     let toInterfaceOpt pn =
           case find (\dpi -> packageName dpi == pn) (S.toList packages) of
             Nothing ->
               return (case (M.lookup pn globalPkgVers,mglobalDocLoc) of
                         (Just (v:_),Just globalDocLoc) ->
                           ["-i"
                           ,"../" ++ joinPkgVer (pn,v) ++
                            "," ++
                            toFilePath globalDocLoc ++ "/" ++
                            joinPkgVer (pn,v) ++ "/" ++
                            packageNameString pn ++ "." ++
                            haddockExtension]
                         _ -> [])
             Just dpi ->
               do let destPath = (FL.toFilePath userDocLoc ++ "/" ++
                                 joinPkgVer (pn,packageVersion dpi) ++ "/" ++
                                 packageNameString pn ++ "." ++
                                 haddockExtension)
                  exists <- doesFileExist destPath
                  return (if exists
                             then ["-i"
                                  ,"../" ++
                                   joinPkgVer (pn,packageVersion dpi) ++
                                   "," ++
                                   destPath]
                             else [])
     --TODO: use not only direct dependencies, but dependencies of dependencies etc.
     --(e.g. redis-fp doesn't include @text@ in its dependencies which means the 'Text'
     --datatype isn't linked in its haddocks)
     fmap concat (mapM toInterfaceOpt (S.toList (packageAllDeps package)))
--}

--------------------------------------------------------------------------------
-- Generated files

-- | Should the generated config be considered invalid?
genFileInvalidated :: Map PackageName GhcPkgId
                   -> BuildOpts
                   -> GenConfig
                   -> PackageName
                   -> Package
                   -> Bool
genFileInvalidated pkgIds bopts gconfig pname package =
  or [installedPkgIdChanged
     ,optimizationsChanged
     ,profilingChanged
     ,ghcOptsChanged
     ,flagsChanged]
  where installedPkgIdChanged =
          gconfigPkgId gconfig /=
          M.lookup pname pkgIds
        ghcOptsChanged = boptsGhcOptions bopts /= gconfigGhcOptions gconfig
        profilingChanged =
          (boptsLibProfile bopts &&
           not (gconfigLibProfiling gconfig)) ||
          (boptsExeProfile bopts &&
           not (gconfigExeProfiling gconfig))
        optimizationsChanged =
          case boptsEnableOptimizations bopts of
            Just optimize
              | optimize /= gconfigOptimize gconfig && optimize -> True
            _ -> False
        flagsChanged = packageFlags package /= gconfigFlags gconfig

-- | Should the generated config be updated?
genFileChanged :: Map PackageName GhcPkgId
               -> BuildOpts
               -> GenConfig
               -> Package
               -> Bool
genFileChanged pkgIds bopts gconfig package =
  or [installedPkgIdChanged
     ,optimizationsChanged && not isDependency
     ,profilingChanged && not isDependency
     ,ghcOptsChanged && not isDependency
     ,flagsChanged]
  where pname = packageName package
        isDependency = packageType package == PTDep
        installedPkgIdChanged =
          gconfigPkgId gconfig /=
          M.lookup pname pkgIds
        ghcOptsChanged = boptsGhcOptions bopts /= gconfigGhcOptions gconfig
        profilingChanged =
          (boptsLibProfile bopts &&
           not (gconfigLibProfiling gconfig)) ||
          (boptsExeProfile bopts &&
           not (gconfigExeProfiling gconfig))
        optimizationsChanged =
          maybe False (/= gconfigOptimize gconfig) (boptsEnableOptimizations bopts)
        flagsChanged =
          packageFlags package /=
          gconfigFlags gconfig

-- | Write out the gen file for the build dir.
updateGenFile :: MonadIO m
              => PackageIdentifier -- ^ Cabal version
              -> Path Abs Dir -> m ()
updateGenFile cabalPkgVer dir = liftIO $ do
  fp <- builtFileFromDir cabalPkgVer dir
  L.writeFile (FL.toFilePath fp) ""

-- | Delete the gen file, which will cause a rebuild.
deleteGenFile :: MonadIO m
              => PackageIdentifier -- ^ Cabal version
              -> Path Abs Dir -> m ()
deleteGenFile cabalPkgVer dir = liftIO $ do
  fp <- configuredFileFromDir cabalPkgVer dir
  catch (removeFile (FL.toFilePath fp))
        (\(_ :: IOException) -> return ())

-- | Save generated configuration.
writeGenConfigFile :: MonadIO m
                   => PackageIdentifier -- ^ Cabal version
                   -> Path Abs Dir
                   -> GenConfig
                   -> ConfigLock -- ^ Needed because this affects the config directory.
                   -> m ()
writeGenConfigFile cabalPkgVer dir gconfig _ = liftIO $
  do built <- builtConfigFileFromDir cabalPkgVer dir
     createDirectoryIfMissing True (FL.toFilePath (FL.parent built))
     fp <- builtConfigFileFromDir cabalPkgVer dir
     L.writeFile (FL.toFilePath fp) (encode gconfig)

-- | Read the generated config file, or return a default based on the
-- build configuration.
readGenConfigFile :: MonadIO m
                  => PackageIdentifier -- ^ Cabal version
                  -> Map PackageName GhcPkgId
                  -> BuildOpts
                  -> Wanted
                  -> Package
                  -> MVar ConfigLock
                  -> m GenConfig
readGenConfigFile cabalPkgVer pkgIds bopts wanted package cfgVar = liftIO (withMVar cfgVar go)
  where name = packageName package
        dir = packageDir package
        go configLock =
          do fp <- builtConfigFileFromDir cabalPkgVer dir
             bytes <-
               catch (fmap Just
                           (S.readFile (FL.toFilePath fp)))
                     (\(_ :: IOException) ->
                        return Nothing)
             case bytes >>= decode . L.fromStrict of
               Just gconfig ->
                 if genFileChanged pkgIds bopts gconfig package
                    then
                         -- If the build config has changed such that the gen
                         -- config needs to be regenerated...
                         do let invalidated =
                                  genFileInvalidated pkgIds bopts gconfig name package
                            when (invalidated || wanted == Wanted)
                                 (deleteGenFile cabalPkgVer dir)
                            let gconfig' =
                                  (newConfig gconfig bopts package)
                            -- When a file has been invalidated it means the
                            -- configuration has changed such that things need
                            -- to be recompiled, hence the above setting of force
                            -- recomp.
                            cleanPackage cabalPkgVer package configLock
                            writeGenConfigFile cabalPkgVer dir gconfig' configLock
                            return gconfig'
                    else return gconfig -- No change, the generated config is consistent with the build config.
               Nothing ->
                 do maybe (return ())
                          (const (putStrLn ("Warning: Couldn't parse config file for " ++
                                            packageNameString name ++
                                            ", migrating to latest configuration format. This will force a rebuild.")))
                          bytes
                    deleteGenFile cabalPkgVer dir
                    let gconfig' =
                          newConfig defaultGenConfig bopts package
                    writeGenConfigFile cabalPkgVer dir gconfig' configLock
                    return gconfig' -- Probably doesn't exist or is out of date (not parseable.)

-- | Update a gen configuration using the build configuration.
newConfig :: GenConfig -- ^ Build configuration.
          -> BuildOpts -- ^ A base gen configuration.
          -> Package
          -> GenConfig
newConfig gconfig bopts package =
  defaultGenConfig
      {gconfigOptimize =
         maybe (gconfigOptimize gconfig)
               id
               (boptsEnableOptimizations bopts)
      ,gconfigLibProfiling = boptsLibProfile bopts ||
                             gconfigLibProfiling gconfig
      ,gconfigExeProfiling = boptsExeProfile bopts ||
                             gconfigExeProfiling gconfig
      ,gconfigGhcOptions = boptsGhcOptions bopts
      ,gconfigFlags = packageFlags package
      ,gconfigPkgId = gconfigPkgId gconfig}

--------------------------------------------------------------------------------
-- Package fetching

-- | Fetch and unpack the package.
withTempUnpacked :: (MonadIO m,MonadThrow m,MonadLogger m,MonadMask m,MonadReader env m,HasHttpManager env,HasConfig env,MonadBaseControl IO m)
                 => PackageIdentifier -- ^ Cabal version
                 -> [PackageIdentifier]
                 -> ([Path Abs Dir] -> m a)
                 -> m a
withTempUnpacked cabalPkgVer pkgs inner = withSystemTempDirectory "stack-unpack" $ \tmp -> do
    dest <- parseAbsDir tmp
    menv <- getMinimalEnvOverride
    mdist <- liftM Just $ distRelativeDir cabalPkgVer
    m <- unpackPackageIdents menv dest mdist $ Set.fromList pkgs
    inner $ M.elems m

--------------------------------------------------------------------------------
-- Paths

{- EKB FIXME: doc generation for stack-doc-server
-- | Returns true for paths whose last directory component begins with ".".
isHiddenDir :: Path b Dir -> Bool
isHiddenDir = isPrefixOf "." . toFilePath . dirname
--}

-- | Get the version of Cabal from the global package database.
getCabalPkgVer :: (MonadThrow m,MonadIO m,MonadLogger m)
               => EnvOverride -> m PackageIdentifier
getCabalPkgVer menv = do
    db <- getGlobalDB menv
    findGhcPkgId
        menv
        [db]
        cabalName >>=
        maybe
            (throwM (Couldn'tFindPkgId cabalName))
            (return . ghcPkgIdPackageIdentifier)
  where
    cabalName =
        $(mkPackageName "Cabal")
