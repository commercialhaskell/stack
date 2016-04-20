{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
-- | Perform a build
module Stack.Build.Execute
    ( printPlan
    , preFetch
    , executePlan
    -- * Running Setup.hs
    , ExecuteEnv
    , withExecuteEnv
    , withSingleContext
    ) where

import           Control.Applicative
import           Control.Arrow ((&&&), second)
import           Control.Concurrent.Execute
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.STM
import           Control.Exception.Enclosed (catchIO)
import           Control.Exception.Lifted
import           Control.Monad (liftM, when, unless, void)
import           Control.Monad.Catch (MonadCatch, MonadMask)
import           Control.Monad.Extra (anyM, (&&^))
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader, asks)
import           Control.Monad.Trans.Control (liftBaseWith)
import           Control.Monad.Trans.Resource
import           Data.Attoparsec.Text hiding (try)
import qualified Data.ByteString as S
import           Data.Char (isSpace)
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import           Data.Either (isRight)
import           Data.Foldable (forM_, any)
import           Data.Function
import           Data.IORef.RunOnce (runOnce)
import           Data.List hiding (any)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Maybe.Extra (forMaybeM)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Streaming.Process hiding (callProcess, env)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time.Clock (getCurrentTime)
import           Data.Traversable (forM)
import           Data.Tuple
import qualified Distribution.PackageDescription as C
import           Distribution.System            (OS (Windows),
                                                 Platform (Platform))
import           Language.Haskell.TH as TH (location)
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Path.Extra (toFilePathNoTrailingSep, rejectMissingFile)
import           Path.IO hiding (findExecutable, makeAbsolute)
import           Prelude hiding (FilePath, writeFile, any)
import           Stack.Build.Cache
import           Stack.Build.Haddock
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Build.Target
import           Stack.Config
import           Stack.Constants
import           Stack.Coverage
import           Stack.Fetch as Fetch
import           Stack.GhcPkg
import           Stack.Package
import           Stack.PackageDump
import           Stack.Types
import           Stack.Types.Internal
import           Stack.Types.StackT
import qualified System.Directory as D
import           System.Environment (getExecutablePath)
import           System.Exit (ExitCode (ExitSuccess))
import qualified System.FilePath as FP
import           System.IO
import           System.PosixCompat.Files (createLink)
import           System.Process.Log (showProcessArgDebug)
import           System.Process.Read
import           System.Process.Run

#if !MIN_VERSION_process(1,2,1)
import           System.Process.Internals (createProcess_)
#endif

type M env m = (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadMask m,HasLogLevel env,HasEnvConfig env,HasTerminal env, HasConfig env)

-- | Fetch the packages necessary for a build, for example in combination with a dry run.
preFetch :: M env m => Plan -> m ()
preFetch plan
    | Set.null idents = $logDebug "Nothing to fetch"
    | otherwise = do
        $logDebug $ T.pack $
            "Prefetching: " ++
            intercalate ", " (map packageIdentifierString $ Set.toList idents)
        menv <- getMinimalEnvOverride
        fetchPackages menv idents
  where
    idents = Set.unions $ map toIdent $ Map.toList $ planTasks plan

    toIdent (name, task) =
        case taskType task of
            TTLocal _ -> Set.empty
            TTUpstream package _ -> Set.singleton $ PackageIdentifier
                name
                (packageVersion package)

-- | Print a description of build plan for human consumption.
printPlan :: M env m
          => Plan
          -> m ()
printPlan plan = do
    case Map.elems $ planUnregisterLocal plan of
        [] -> $logInfo "No packages would be unregistered."
        xs -> do
            $logInfo "Would unregister locally:"
            forM_ xs $ \(ident, mreason) -> $logInfo $ T.concat
                [ T.pack $ packageIdentifierString ident
                , case mreason of
                    Nothing -> ""
                    Just reason -> T.concat
                        [ " ("
                        , reason
                        , ")"
                        ]
                ]

    $logInfo ""

    case Map.elems $ planTasks plan of
        [] -> $logInfo "Nothing to build."
        xs -> do
            $logInfo "Would build:"
            mapM_ ($logInfo . displayTask) xs

    let hasTests = not . Set.null . testComponents . taskComponents
        hasBenches = not . Set.null . benchComponents . taskComponents
        tests = Map.elems $ Map.filter hasTests $ planFinals plan
        benches = Map.elems $ Map.filter hasBenches $ planFinals plan

    unless (null tests) $ do
        $logInfo ""
        $logInfo "Would test:"
        mapM_ ($logInfo . displayTask) tests
    unless (null benches) $ do
        $logInfo ""
        $logInfo "Would benchmark:"
        mapM_ ($logInfo . displayTask) benches

    $logInfo ""

    case Map.toList $ planInstallExes plan of
        [] -> $logInfo "No executables to be installed."
        xs -> do
            $logInfo "Would install executables:"
            forM_ xs $ \(name, loc) -> $logInfo $ T.concat
                [ name
                , " from "
                , case loc of
                    Snap -> "snapshot"
                    Local -> "local"
                , " database"
                ]

-- | For a dry run
displayTask :: Task -> Text
displayTask task = T.pack $ concat
    [ packageIdentifierString $ taskProvides task
    , ": database="
    , case taskLocation task of
        Snap -> "snapshot"
        Local -> "local"
    , ", source="
    , case taskType task of
        TTLocal lp -> concat
            [ toFilePath $ lpDir lp
            ]
        TTUpstream _ _ -> "package index"
    , if Set.null missing
        then ""
        else ", after: " ++ intercalate "," (map packageIdentifierString $ Set.toList missing)
    ]
  where
    missing = tcoMissing $ taskConfigOpts task

data ExecuteEnv = ExecuteEnv
    { eeEnvOverride    :: !EnvOverride
    , eeConfigureLock  :: !(MVar ())
    , eeInstallLock    :: !(MVar ())
    , eeBuildOpts      :: !BuildOpts
    , eeBuildOptsCLI   :: !BuildOptsCLI
    , eeBaseConfigOpts :: !BaseConfigOpts
    , eeGhcPkgIds      :: !(TVar (Map PackageIdentifier Installed))
    , eeTempDir        :: !(Path Abs Dir)
    , eeSetupHs        :: !(Path Abs File)
    -- ^ Temporary Setup.hs for simple builds
    , eeSetupExe       :: !(Maybe (Path Abs File))
    -- ^ Compiled version of eeSetupHs
    , eeCabalPkgVer    :: !Version
    , eeTotalWanted    :: !Int
    , eeWanted         :: !(Set PackageName)
    , eeLocals         :: ![LocalPackage]
    , eeGlobalDB       :: !(Path Abs Dir)
    , eeGlobalDumpPkgs :: !(Map GhcPkgId (DumpPackage () ()))
    , eeSnapshotDumpPkgs :: !(TVar (Map GhcPkgId (DumpPackage () ())))
    , eeLocalDumpPkgs  :: !(TVar (Map GhcPkgId (DumpPackage () ())))
    }

-- | Get a compiled Setup exe
getSetupExe :: M env m
            => Path Abs File -- ^ Setup.hs input file
            -> Path Abs Dir -- ^ temporary directory
            -> m (Maybe (Path Abs File))
getSetupExe setupHs tmpdir = do
    wc <- getWhichCompiler
    econfig <- asks getEnvConfig
    platformDir <- platformGhcRelDir
    let config = getConfig econfig
        baseNameS = concat
            [ "setup-Simple-Cabal-"
            , versionString $ envConfigCabalVersion econfig
            , "-"
            , compilerVersionString $ envConfigCompilerVersion econfig
            ]
        exeNameS = baseNameS ++
            case configPlatform config of
                Platform _ Windows -> ".exe"
                _ -> ""
        outputNameS =
            case wc of
                Ghc -> exeNameS
                Ghcjs -> baseNameS ++ ".jsexe"
        jsExeNameS =
            baseNameS ++ ".jsexe"
        setupDir =
            configStackRoot config </>
            $(mkRelDir "setup-exe-cache") </>
            platformDir

    exePath <- fmap (setupDir </>) $ parseRelFile exeNameS
    jsExePath <- fmap (setupDir </>) $ parseRelDir jsExeNameS

    exists <- liftIO $ D.doesFileExist $ toFilePath exePath

    if exists
        then return $ Just exePath
        else do
            tmpExePath <- fmap (setupDir </>) $ parseRelFile $ "tmp-" ++ exeNameS
            tmpOutputPath <- fmap (setupDir </>) $ parseRelFile $ "tmp-" ++ outputNameS
            tmpJsExePath <- fmap (setupDir </>) $ parseRelDir $ "tmp-" ++ jsExeNameS

            liftIO $ D.createDirectoryIfMissing True $ toFilePath setupDir

            menv <- getMinimalEnvOverride
            let args =
                    [ "-clear-package-db"
                    , "-global-package-db"
                    , "-hide-all-packages"
                    , "-package"
                    , "base"
                    , "-package"
                    , "Cabal-" ++ versionString (envConfigCabalVersion econfig)
                    , toFilePath setupHs
                    , "-o"
                    , toFilePath tmpOutputPath
                    , "-rtsopts"
                    ] ++
                    ["-build-runner" | wc == Ghcjs]
            runCmd' (\cp -> cp { std_out = UseHandle stderr }) (Cmd (Just tmpdir) (compilerExeName wc) menv args) Nothing
            when (wc == Ghcjs) $ renameDir tmpJsExePath jsExePath
            renameFile tmpExePath exePath
            return $ Just exePath

-- | Execute a callback that takes an 'ExecuteEnv'.
withExecuteEnv :: M env m
               => EnvOverride
               -> BuildOpts
               -> BuildOptsCLI
               -> BaseConfigOpts
               -> [LocalPackage]
               -> [DumpPackage () ()] -- ^ global packages
               -> [DumpPackage () ()] -- ^ snapshot packages
               -> [DumpPackage () ()] -- ^ local packages
               -> (ExecuteEnv -> m a)
               -> m a
withExecuteEnv menv bopts boptsCli baseConfigOpts locals globalPackages snapshotPackages localPackages inner = do
    withSystemTempDir stackProgName $ \tmpdir -> do
        configLock <- newMVar ()
        installLock <- newMVar ()
        idMap <- liftIO $ newTVarIO Map.empty
        let setupHs = tmpdir </> $(mkRelFile "Setup.hs")
        liftIO $ writeFile (toFilePath setupHs) "import Distribution.Simple\nmain = defaultMain"
        setupExe <- getSetupExe setupHs tmpdir
        cabalPkgVer <- asks (envConfigCabalVersion . getEnvConfig)
        globalDB <- getGlobalDB menv =<< getWhichCompiler
        snapshotPackagesTVar <- liftIO $ newTVarIO (toDumpPackagesByGhcPkgId snapshotPackages)
        localPackagesTVar <- liftIO $ newTVarIO (toDumpPackagesByGhcPkgId localPackages)
        inner ExecuteEnv
            { eeEnvOverride = menv
            , eeBuildOpts = bopts
            , eeBuildOptsCLI = boptsCli
             -- Uncertain as to why we cannot run configures in parallel. This appears
             -- to be a Cabal library bug. Original issue:
             -- https://github.com/fpco/stack/issues/84. Ideally we'd be able to remove
             -- this.
            , eeConfigureLock = configLock
            , eeInstallLock = installLock
            , eeBaseConfigOpts = baseConfigOpts
            , eeGhcPkgIds = idMap
            , eeTempDir = tmpdir
            , eeSetupHs = setupHs
            , eeSetupExe = setupExe
            , eeCabalPkgVer = cabalPkgVer
            , eeTotalWanted = length $ filter lpWanted locals
            , eeWanted = wantedLocalPackages locals
            , eeLocals = locals
            , eeGlobalDB = globalDB
            , eeGlobalDumpPkgs = toDumpPackagesByGhcPkgId globalPackages
            , eeSnapshotDumpPkgs = snapshotPackagesTVar
            , eeLocalDumpPkgs = localPackagesTVar
            }
  where
    toDumpPackagesByGhcPkgId = Map.fromList . map (\dp -> (dpGhcPkgId dp, dp))

-- | Perform the actual plan
executePlan :: M env m
            => EnvOverride
            -> BuildOptsCLI
            -> BaseConfigOpts
            -> [LocalPackage]
            -> [DumpPackage () ()] -- ^ global packages
            -> [DumpPackage () ()] -- ^ snapshot packages
            -> [DumpPackage () ()] -- ^ local packages
            -> InstalledMap
            -> Map PackageName SimpleTarget
            -> Plan
            -> m ()
executePlan menv boptsCli baseConfigOpts locals globalPackages snapshotPackages localPackages installedMap targets plan = do
    bopts <- asks (configBuild . getConfig)
    withExecuteEnv menv bopts boptsCli baseConfigOpts locals globalPackages snapshotPackages localPackages (executePlan' installedMap targets plan)

    unless (Map.null $ planInstallExes plan) $ do
        snapBin <- (</> bindirSuffix) `liftM` installationRootDeps
        localBin <- (</> bindirSuffix) `liftM` installationRootLocal
        destDir <- asks $ configLocalBin . getConfig
        ensureDir destDir

        destDir' <- liftIO . D.canonicalizePath . toFilePath $ destDir

        platform <- asks getPlatform
        let ext =
                case platform of
                    Platform _ Windows -> ".exe"
                    _ -> ""

        currExe <- liftIO getExecutablePath -- needed for windows, see below

        installed <- forMaybeM (Map.toList $ planInstallExes plan) $ \(name, loc) -> do
            let bindir =
                    case loc of
                        Snap -> snapBin
                        Local -> localBin
            mfp <- forgivingAbsence (resolveFile bindir $ T.unpack name ++ ext)
              >>= rejectMissingFile
            case mfp of
                Nothing -> do
                    $logWarn $ T.concat
                        [ "Couldn't find executable "
                        , name
                        , " in directory "
                        , T.pack $ toFilePath bindir
                        ]
                    return Nothing
                Just file -> do
                    let destFile = destDir' FP.</> T.unpack name ++ ext
                    $logInfo $ T.concat
                        [ "Copying from "
                        , T.pack $ toFilePath file
                        , " to "
                        , T.pack destFile
                        ]

                    liftIO $ case platform of
                        Platform _ Windows | FP.equalFilePath destFile currExe ->
                            windowsRenameCopy (toFilePath file) destFile
                        _ -> D.copyFile (toFilePath file) destFile
                    return $ Just (name <> T.pack ext)

        unless (null installed) $ do
            $logInfo ""
            $logInfo $ T.concat
                [ "Copied executables to "
                , T.pack destDir'
                , ":"]
        forM_ installed $ \exe -> $logInfo ("- " <> exe)

        searchPath <- liftIO FP.getSearchPath
        destDirIsInPATH <- liftIO $
            anyM (\dir -> D.doesDirectoryExist dir &&^ fmap (FP.equalFilePath destDir') (D.canonicalizePath dir)) searchPath
        if destDirIsInPATH
            then forM_ installed $ \exe -> do
                mexePath <- (liftIO . D.findExecutable . T.unpack) exe
                case mexePath of
                    Just exePath -> do
                        exeDir <- (liftIO . fmap FP.takeDirectory . D.canonicalizePath) exePath
                        unless (exeDir `FP.equalFilePath` destDir') $ do
                            $logWarn ""
                            $logWarn $ T.concat
                                [ "WARNING: The \""
                                , exe
                                , "\" executable found on the PATH environment variable is "
                                , T.pack exePath
                                , ", and not the version that was just installed."
                                ]
                            $logWarn $ T.concat
                                [ "This means that \""
                                , exe
                                , "\" calls on the command line will not use this version."
                                ]
                    Nothing -> do
                        $logWarn ""
                        $logWarn $ T.concat
                            [ "WARNING: Installation path "
                            , T.pack destDir'
                            , " is on the PATH but the \""
                            , exe
                            , "\" executable that was just installed could not be found on the PATH."
                            ]
            else do
                $logWarn ""
                $logWarn $ T.concat
                    [ "WARNING: Installation path "
                    , T.pack destDir'
                    , " not found on the PATH environment variable"
                    ]

    config <- asks getConfig
    menv' <- liftIO $ configEnvOverride config EnvSettings
                    { esIncludeLocals = True
                    , esIncludeGhcPackagePath = True
                    , esStackExe = True
                    , esLocaleUtf8 = False
                    }
    forM_ (boptsCLIExec boptsCli) $ \(cmd, args) -> do
        $logProcessRun cmd args
        callProcess (Cmd Nothing cmd menv' args)

-- | Windows can't write over the current executable. Instead, we rename the
-- current executable to something else and then do the copy.
windowsRenameCopy :: FilePath -> FilePath -> IO ()
windowsRenameCopy src dest = do
    D.copyFile src new
    D.renameFile dest old
    D.renameFile new dest
  where
    new = dest ++ ".new"
    old = dest ++ ".old"

-- | Perform the actual plan (internal)
executePlan' :: M env m
             => InstalledMap
             -> Map PackageName SimpleTarget
             -> Plan
             -> ExecuteEnv
             -> m ()
executePlan' installedMap0 targets plan ee@ExecuteEnv {..} = do
    when (toCoverage $ boptsTestOpts eeBuildOpts) deleteHpcReports
    wc <- getWhichCompiler
    cv <- asks $ envConfigCompilerVersion . getEnvConfig
    case Map.toList $ planUnregisterLocal plan of
        [] -> return ()
        ids -> do
            localDB <- packageDatabaseLocal
            forM_ ids $ \(id', (ident, mreason)) -> do
                $logInfo $ T.concat
                    [ T.pack $ packageIdentifierString ident
                    , ": unregistering"
                    , case mreason of
                        Nothing -> ""
                        Just reason -> T.concat
                            [ " ("
                            , reason
                            , ")"
                            ]
                    ]
                unregisterGhcPkgId eeEnvOverride wc cv localDB id' ident

    liftIO $ atomically $ modifyTVar' eeLocalDumpPkgs $ \initMap ->
        foldl' (flip Map.delete) initMap $ Map.keys (planUnregisterLocal plan)

    -- Yes, we're explicitly discarding result values, which in general would
    -- be bad. monad-unlift does this all properly at the type system level,
    -- but I don't want to pull it in for this one use case, when we know that
    -- stack always using transformer stacks that are safe for this use case.
    runInBase <- liftBaseWith $ \run -> return (void . run)

    let actions = concatMap (toActions installedMap' runInBase ee) $ Map.elems $ Map.mergeWithKey
            (\_ b f -> Just (Just b, Just f))
            (fmap (\b -> (Just b, Nothing)))
            (fmap (\f -> (Nothing, Just f)))
            (planTasks plan)
            (planFinals plan)
    threads <- asks $ configJobs . getConfig
    concurrentTests <- asks $ configConcurrentTests . getConfig
    let keepGoing =
            case boptsKeepGoing eeBuildOpts of
                Just kg -> kg
                Nothing -> boptsTests eeBuildOpts || boptsBenchmarks eeBuildOpts
        concurrentFinal =
            -- TODO it probably makes more sense to use a lock for test suites
            -- and just have the execution blocked. Turning off all concurrency
            -- on finals based on the --test option doesn't fit in well.
            if boptsTests eeBuildOpts
                then concurrentTests
                else True
    terminal <- asks getTerminal
    errs <- liftIO $ runActions threads keepGoing concurrentFinal actions $ \doneVar -> do
        let total = length actions
            loop prev
                | prev == total =
                    runInBase $ $logStickyDone ("Completed " <> T.pack (show total) <> " action(s).")
                | otherwise = do
                    when terminal $ runInBase $
                        $logSticky ("Progress: " <> T.pack (show prev) <> "/" <> T.pack (show total))
                    done <- atomically $ do
                        done <- readTVar doneVar
                        check $ done /= prev
                        return done
                    loop done
        if total > 1
            then loop 0
            else return ()
    when (toCoverage $ boptsTestOpts eeBuildOpts) $ do
        generateHpcUnifiedReport
        generateHpcMarkupIndex
    unless (null errs) $ throwM $ ExecutionFailure errs
    when (boptsHaddock eeBuildOpts) $ do
        snapshotDumpPkgs <- liftIO (readTVarIO eeSnapshotDumpPkgs)
        localDumpPkgs <- liftIO (readTVarIO eeLocalDumpPkgs)
        generateLocalHaddockIndex eeEnvOverride wc eeBaseConfigOpts localDumpPkgs eeLocals
        generateDepsHaddockIndex eeEnvOverride wc eeBaseConfigOpts eeGlobalDumpPkgs snapshotDumpPkgs localDumpPkgs eeLocals
        generateSnapHaddockIndex eeEnvOverride wc eeBaseConfigOpts eeGlobalDumpPkgs snapshotDumpPkgs
        when (boptsOpenHaddocks eeBuildOpts) $ do
            let planPkgs, localPkgs, installedPkgs, availablePkgs
                    :: Map PackageName (PackageIdentifier, InstallLocation)
                planPkgs = Map.map (taskProvides &&& taskLocation) (planTasks plan)
                localPkgs =
                    Map.fromList
                        [(packageName p, (packageIdentifier p, Local)) | p <- map lpPackage eeLocals]
                installedPkgs = Map.map (swap . second installedPackageIdentifier) installedMap'
                availablePkgs = Map.unions [planPkgs, localPkgs, installedPkgs]
            openHaddocksInBrowser eeBaseConfigOpts availablePkgs (Map.keysSet targets)
  where
    installedMap' = Map.difference installedMap0
                  $ Map.fromList
                  $ map (\(ident, _) -> (packageIdentifierName ident, ()))
                  $ Map.elems
                  $ planUnregisterLocal plan

toActions :: M env m
          => InstalledMap
          -> (m () -> IO ())
          -> ExecuteEnv
          -> (Maybe Task, Maybe Task) -- build and final
          -> [Action]
toActions installedMap runInBase ee (mbuild, mfinal) =
    abuild ++ afinal
  where
    abuild =
        case mbuild of
            Nothing -> []
            Just task@Task {..} ->
                [ Action
                    { actionId = ActionId taskProvides ATBuild
                    , actionDeps =
                        (Set.map (\ident -> ActionId ident ATBuild) (tcoMissing taskConfigOpts))
                    , actionDo = \ac -> runInBase $ singleBuild runInBase ac ee task installedMap False
                    }
                ]
    afinal =
        case mfinal of
            Nothing -> []
            Just task@Task {..} ->
                (if taskAllInOne then [] else
                    [Action
                        { actionId = ActionId taskProvides ATBuildFinal
                        , actionDeps = addBuild ATBuild
                            (Set.map (\ident -> ActionId ident ATBuild) (tcoMissing taskConfigOpts))
                        , actionDo = \ac -> runInBase $ singleBuild runInBase ac ee task installedMap True
                        }]) ++
                [ Action
                    { actionId = ActionId taskProvides ATFinal
                    , actionDeps = addBuild (if taskAllInOne then ATBuild else ATBuildFinal) Set.empty
                    , actionDo = \ac -> runInBase $ do
                        let comps = taskComponents task
                            tests = testComponents comps
                            benches = benchComponents comps
                        unless (Set.null tests) $ do
                            singleTest runInBase topts (Set.toList tests) ac ee task installedMap
                        unless (Set.null benches) $ do
                            singleBench runInBase beopts (Set.toList benches) ac ee task installedMap
                    }
                ]
              where
                addBuild aty =
                    case mbuild of
                        Nothing -> id
                        Just _ -> Set.insert $ ActionId taskProvides aty
    bopts = eeBuildOpts ee
    topts = boptsTestOpts bopts
    beopts = boptsBenchmarkOpts bopts

-- | Generate the ConfigCache
getConfigCache :: M env m
               => ExecuteEnv -> Task -> InstalledMap -> Bool -> Bool
               -> m (Map PackageIdentifier GhcPkgId, ConfigCache)
getConfigCache ExecuteEnv {..} Task {..} installedMap enableTest enableBench = do
    useExactConf <- asks (configAllowNewer . getConfig)
    let extra =
            -- We enable tests if the test suite dependencies are already
            -- installed, so that we avoid unnecessary recompilation based on
            -- cabal_macros.h changes when switching between 'stack build' and
            -- 'stack test'. See:
            -- https://github.com/commercialhaskell/stack/issues/805
            case taskType of
                TTLocal lp -> concat
                    -- FIXME: make this work with exact-configuration.
                    -- Not sure how to plumb the info atm. See
                    -- https://github.com/commercialhaskell/stack/issues/2049
                    [ [ "--enable-tests"
                      | enableTest ||
                        (not useExactConf && depsPresent installedMap (lpTestDeps lp))]
                    , [ "--enable-benchmarks"
                      | enableBench ||
                        (not useExactConf && depsPresent installedMap (lpBenchDeps lp))]
                    ]
                _ -> []
    idMap <- liftIO $ readTVarIO eeGhcPkgIds
    let getMissing ident =
            case Map.lookup ident idMap of
                Nothing -> error "singleBuild: invariant violated, missing package ID missing"
                Just (Library ident' x) -> assert (ident == ident') $ Just (ident, x)
                Just (Executable _) -> Nothing
        missing' = Map.fromList $ mapMaybe getMissing $ Set.toList missing
        TaskConfigOpts missing mkOpts = taskConfigOpts
        opts = mkOpts missing'
        allDeps = Set.fromList $ Map.elems missing' ++ Map.elems taskPresent
        cache = ConfigCache
            { configCacheOpts = opts
                { coNoDirs = coNoDirs opts ++ map T.unpack extra
                }
            , configCacheDeps = allDeps
            , configCacheComponents =
                case taskType of
                    TTLocal lp -> Set.map renderComponent $ lpComponents lp
                    TTUpstream _ _ -> Set.empty
            , configCacheHaddock =
                shouldHaddockPackage eeBuildOpts eeWanted (packageIdentifierName taskProvides)
            }
        allDepsMap = Map.union missing' taskPresent
    return (allDepsMap, cache)

-- | Ensure that the configuration for the package matches what is given
ensureConfig :: M env m
             => ConfigCache -- ^ newConfigCache
             -> Path Abs Dir -- ^ package directory
             -> ExecuteEnv
             -> m () -- ^ announce
             -> (Bool -> [String] -> m ()) -- ^ cabal
             -> Path Abs File -- ^ .cabal file
             -> m Bool
ensureConfig newConfigCache pkgDir ExecuteEnv {..} announce cabal cabalfp = do
    newCabalMod <- liftIO (fmap modTime (D.getModificationTime (toFilePath cabalfp)))
    needConfig <-
        if boptsReconfigure eeBuildOpts
            then return True
            else do
                -- We can ignore the components portion of the config
                -- cache, because it's just used to inform 'construct
                -- plan that we need to plan to build additional
                -- components. These components don't affect the actual
                -- package configuration.
                let ignoreComponents cc = cc { configCacheComponents = Set.empty }
                -- Determine the old and new configuration in the local directory, to
                -- determine if we need to reconfigure.
                mOldConfigCache <- tryGetConfigCache pkgDir

                mOldCabalMod <- tryGetCabalMod pkgDir

                return $ fmap ignoreComponents mOldConfigCache /= Just (ignoreComponents newConfigCache)
                      || mOldCabalMod /= Just newCabalMod
    let ConfigureOpts dirs nodirs = configCacheOpts newConfigCache
    when needConfig $ withMVar eeConfigureLock $ \_ -> do
        deleteCaches pkgDir
        announce
        menv <- getMinimalEnvOverride
        let programNames =
                if eeCabalPkgVer < $(mkVersion "1.22")
                    then ["ghc", "ghc-pkg"]
                    else ["ghc", "ghc-pkg", "ghcjs", "ghcjs-pkg"]
        exes <- forM programNames $ \name -> do
            mpath <- findExecutable menv name
            return $ case mpath of
                Nothing -> []
                Just x -> return $ concat ["--with-", name, "=", toFilePath x]
        -- Configure cabal with arguments determined by
        -- Stack.Types.Build.configureOpts
        cabal False $ "configure" : concat
            [ concat exes
            , dirs
            , nodirs
            ]
        writeConfigCache pkgDir newConfigCache
        writeCabalMod pkgDir newCabalMod

    return needConfig

announceTask :: MonadLogger m => Task -> Text -> m ()
announceTask task x = $logInfo $ T.concat
    [ T.pack $ packageIdentifierString $ taskProvides task
    , ": "
    , x
    ]

withSingleContext :: M env m
                  => (m () -> IO ())
                  -> ActionContext
                  -> ExecuteEnv
                  -> Task
                  -> Maybe (Map PackageIdentifier GhcPkgId)
                  -- ^ All dependencies' package ids to provide to Setup.hs. If
                  -- Nothing, just provide global and snapshot package
                  -- databases.
                  -> Maybe String
                  -> (  Package
                     -> Path Abs File
                     -> Path Abs Dir
                     -> (Bool -> [String] -> m ())
                     -> (Text -> m ())
                     -> Bool
                     -> Maybe (Path Abs File, Handle)
                     -> m a)
                  -> m a
withSingleContext runInBase ActionContext {..} ExecuteEnv {..} task@Task {..} mdeps msuffix inner0 =
    withPackage $ \package cabalfp pkgDir ->
    withLogFile package $ \mlogFile ->
    withCabal package pkgDir mlogFile $ \cabal ->
    inner0 package cabalfp pkgDir cabal announce console mlogFile
  where
    announce = announceTask task

    wanted =
        case taskType of
            TTLocal lp -> lpWanted lp
            TTUpstream _ _ -> False

    console = wanted
           && all (\(ActionId ident _) -> ident == taskProvides) (Set.toList acRemaining)
           && eeTotalWanted == 1

    withPackage inner =
        case taskType of
            TTLocal lp -> inner (lpPackage lp) (lpCabalFile lp) (lpDir lp)
            TTUpstream package _ -> do
                mdist <- liftM Just distRelativeDir
                m <- unpackPackageIdents eeEnvOverride eeTempDir mdist $ Set.singleton taskProvides
                case Map.toList m of
                    [(ident, dir)]
                        | ident == taskProvides -> do
                            let name = packageIdentifierName taskProvides
                            cabalfpRel <- parseRelFile $ packageNameString name ++ ".cabal"
                            let cabalfp = dir </> cabalfpRel
                            inner package cabalfp dir
                    _ -> error $ "withPackage: invariant violated: " ++ show m

    withLogFile package inner
        | console = inner Nothing
        | otherwise = do
            logPath <- buildLogPath package msuffix
            ensureDir (parent logPath)
            let fp = toFilePath logPath
            bracket
                (liftIO $ openBinaryFile fp WriteMode)
                (liftIO . hClose)
                $ \h -> inner (Just (logPath, h))

    withCabal package pkgDir mlogFile inner = do
        config <- asks getConfig

        unless (configAllowDifferentUser config) $
            checkOwnership (pkgDir </> configWorkDir config)

        let envSettings = EnvSettings
                { esIncludeLocals = taskLocation task == Local
                , esIncludeGhcPackagePath = False
                , esStackExe = False
                , esLocaleUtf8 = True
                }
        menv <- liftIO $ configEnvOverride config envSettings
        getGhcPath <- runOnce $ getCompilerPath Ghc
        getGhcjsPath <- runOnce $ getCompilerPath Ghcjs
        distRelativeDir' <- distRelativeDir
        esetupexehs <-
            -- Avoid broken Setup.hs files causing problems for simple build
            -- types, see:
            -- https://github.com/commercialhaskell/stack/issues/370
            case (packageSimpleType package, eeSetupExe) of
                (True, Just setupExe) -> return $ Left setupExe
                _ -> liftIO $ fmap Right $ getSetupHs pkgDir
        inner $ \stripTHLoading args -> do
            let cabalPackageArg =
                    "-package=" ++ packageIdentifierString
                                       (PackageIdentifier cabalPackageName
                                                          eeCabalPkgVer)
                packageArgs =
                    case mdeps of
                        -- This branch is taken when
                        -- 'explicit-setup-deps' is requested in your
                        -- stack.yaml file.
                        Just deps | explicitSetupDeps (packageName package) config ->
                            -- Stack always builds with the global Cabal for various
                            -- reproducibility issues.
                            let depsMinusCabal
                                 = map ghcPkgIdString
                                 $ Set.toList
                                 $ addGlobalPackages deps (Map.elems eeGlobalDumpPkgs)
                            in
                                ( "-clear-package-db"
                                : "-global-package-db"
                                : map (("-package-db=" ++) . toFilePathNoTrailingSep) (bcoExtraDBs eeBaseConfigOpts)
                                ) ++
                                ( ("-package-db=" ++ toFilePathNoTrailingSep (bcoSnapDB eeBaseConfigOpts))
                                : ("-package-db=" ++ toFilePathNoTrailingSep (bcoLocalDB eeBaseConfigOpts))
                                : "-hide-all-packages"
                                : cabalPackageArg
                                : map ("-package-id=" ++) depsMinusCabal
                                )
                        -- This branch is usually taken for builds, and
                        -- is always taken for `stack sdist`.
                        --
                        -- This approach is debatable. It adds access to the
                        -- snapshot package database for Cabal. There are two
                        -- possible objections:
                        --
                        -- 1. This doesn't isolate the build enough; arbitrary
                        -- other packages available could cause the build to
                        -- succeed or fail.
                        --
                        -- 2. This doesn't provide enough packages: we should also
                        -- include the local database when building local packages.
                        --
                        -- Currently, this branch is only taken via `stack
                        -- sdist` or when explicitly requested in the
                        -- stack.yaml file.
                        _ ->
                              cabalPackageArg
                            : "-clear-package-db"
                            : "-global-package-db"
                            : map (("-package-db=" ++) . toFilePathNoTrailingSep) (bcoExtraDBs eeBaseConfigOpts)
                           ++ ["-package-db=" ++ toFilePathNoTrailingSep (bcoSnapDB eeBaseConfigOpts)]

                setupArgs = ("--builddir=" ++ toFilePathNoTrailingSep distRelativeDir') : args
                runExe exeName fullArgs =
                    runAndOutput `catch` \(ProcessExitedUnsuccessfully _ ec) -> do
                        bss <-
                            case mlogFile of
                                Nothing -> return []
                                Just (logFile, h) -> do
                                    liftIO $ hClose h
                                    runResourceT
                                        $ CB.sourceFile (toFilePath logFile)
                                        =$= CT.decodeUtf8Lenient
                                        $$ mungeBuildOutput stripTHLoading makeAbsolute pkgDir
                                        =$ CL.consume
                        throwM $ CabalExitedUnsuccessfully
                            ec
                            taskProvides
                            exeName
                            fullArgs
                            (fmap fst mlogFile)
                            bss
                  where
                    runAndOutput = case mlogFile of
                        Just (_, h) ->
                            sinkProcessStderrStdoutHandle (Just pkgDir) menv (toFilePath exeName) fullArgs h h
                        Nothing ->
                            void $ sinkProcessStderrStdout (Just pkgDir) menv (toFilePath exeName) fullArgs
                                (outputSink False LevelWarn)
                                (outputSink stripTHLoading LevelInfo)
                    outputSink excludeTH level =
                        CT.decodeUtf8Lenient
                        =$ mungeBuildOutput excludeTH makeAbsolute pkgDir
                        =$ CL.mapM_ (runInBase . monadLoggerLog $(TH.location >>= liftLoc) "" level)
                    -- If users want control, we should add a config option for this
                    makeAbsolute = stripTHLoading

            wc <- getWhichCompiler
            (exeName, fullArgs) <- case (esetupexehs, wc) of
                (Left setupExe, _) -> return (setupExe, setupArgs)
                (Right setuphs, compiler) -> do
                    distDir <- distDirFromDir pkgDir
                    let setupDir = distDir </> $(mkRelDir "setup")
                        outputFile = setupDir </> $(mkRelFile "setup")
                    ensureDir setupDir
                    compilerPath <-
                        case compiler of
                            Ghc -> getGhcPath
                            Ghcjs -> getGhcjsPath
                    runExe compilerPath $
                        [ "--make"
                        , "-odir", toFilePathNoTrailingSep setupDir
                        , "-hidir", toFilePathNoTrailingSep setupDir
                        , "-i", "-i."
                        ] ++ packageArgs ++
                        [ toFilePath setuphs
                        , "-o", toFilePath outputFile
                        ] ++
                        (case compiler of
                            Ghc -> []
                            Ghcjs -> ["-build-runner"])
                    return (outputFile, setupArgs)
            runExe exeName $ (if boptsCabalVerbose eeBuildOpts then ("--verbose":) else id) fullArgs

singleBuild :: M env m
            => (m () -> IO ())
            -> ActionContext
            -> ExecuteEnv
            -> Task
            -> InstalledMap
            -> Bool             -- ^ Is this a final build?
            -> m ()
singleBuild runInBase ac@ActionContext {..} ee@ExecuteEnv {..} task@Task {..} installedMap isFinalBuild = do
    (allDepsMap, cache) <- getConfigCache ee task installedMap enableTests enableBenchmarks
    mprecompiled <- getPrecompiled cache
    minstalled <-
        case mprecompiled of
            Just precompiled -> copyPreCompiled precompiled
            Nothing -> realConfigAndBuild cache allDepsMap
    case minstalled of
        Nothing -> return ()
        Just installed -> do
            writeFlagCache installed cache
            liftIO $ atomically $ modifyTVar eeGhcPkgIds $ Map.insert taskProvides installed
  where
    pname = packageIdentifierName taskProvides
    shouldHaddockPackage' = shouldHaddockPackage eeBuildOpts eeWanted pname
    doHaddock package = shouldHaddockPackage' &&
                        not isFinalBuild &&
                        -- Works around haddock failing on bytestring-builder since it has no modules
                        -- when bytestring is new enough.
                        packageHasExposedModules package

    buildingFinals = isFinalBuild || taskAllInOne
    enableTests = buildingFinals && any isCTest (taskComponents task)
    enableBenchmarks = buildingFinals && any isCBench (taskComponents task)

    annSuffix = if result == "" then "" else " (" <> result <> ")"
      where
        result = T.intercalate " + " $ concat $
            [ ["lib" | taskAllInOne && hasLib]
            , ["exe" | taskAllInOne && hasExe]
            , ["test" | enableTests]
            , ["bench" | enableBenchmarks]
            ]
        (hasLib, hasExe) = case taskType of
            TTLocal lp -> (packageHasLibrary (lpPackage lp), not (Set.null (exesToBuild lp)))
            -- This isn't true, but we don't want to have this info for
            -- upstream deps.
            TTUpstream{} -> (False, False)

    getPrecompiled cache =
        case taskLocation task of
            Snap | not shouldHaddockPackage' -> do
                mpc <- readPrecompiledCache taskProvides
                    (configCacheOpts cache)
                    (configCacheDeps cache)
                case mpc of
                    Nothing -> return Nothing
                    Just pc | maybe False
                                    (bcoSnapInstallRoot eeBaseConfigOpts `isParentOf`)
                                    (parseAbsFile =<< (pcLibrary pc)) ->
                        -- If old precompiled cache files are left around but snapshots are deleted,
                        -- it is possible for the precompiled file to refer to the very library
                        -- we're building, and if flags are changed it may try to copy the library
                        -- to itself. This check prevents that from happening.
                        return Nothing
                    Just pc | otherwise -> do
                        let allM _ [] = return True
                            allM f (x:xs) = do
                                b <- f x
                                if b then allM f xs else return False
                        b <- liftIO $ allM D.doesFileExist $ maybe id (:) (pcLibrary pc) $ pcExes pc
                        return $ if b then Just pc else Nothing
            _ -> return Nothing

    copyPreCompiled (PrecompiledCache mlib exes) = do
        wc <- getWhichCompiler
        announceTask task "using precompiled package"
        forM_ mlib $ \libpath -> do
            menv <- getMinimalEnvOverride
            withMVar eeInstallLock $ \() -> do
                -- We want to ignore the global and user databases.
                -- Unfortunately, ghc-pkg doesn't take such arguments on the
                -- command line. Instead, we'll set GHC_PACKAGE_PATH. See:
                -- https://github.com/commercialhaskell/stack/issues/1146

                menv' <- modifyEnvOverride menv
                       $ Map.insert
                            "GHC_PACKAGE_PATH"
                            (T.pack $ toFilePathNoTrailingSep $ bcoSnapDB eeBaseConfigOpts)

                -- In case a build of the library with different flags already exists, unregister it
                -- before copying.
                let ghcPkgExe = ghcPkgExeName wc
                catch
                    (readProcessNull Nothing menv' ghcPkgExe
                        [ "unregister"
                        , "--force"
                        , packageIdentifierString taskProvides
                        ])
                    (\ex -> case ex of
                        ReadProcessException{} -> return ()
                        _ -> throwM ex)

                readProcessNull Nothing menv' ghcPkgExe
                    [ "register"
                    , "--force"
                    , libpath
                    ]
        liftIO $ forM_ exes $ \exe -> do
            D.createDirectoryIfMissing True bindir
            let dst = bindir FP.</> FP.takeFileName exe
            createLink exe dst `catchIO` \_ -> D.copyFile exe dst
        case (mlib, exes) of
            (Nothing, _:_) -> markExeInstalled (taskLocation task) taskProvides
            _ -> return ()

        -- Find the package in the database
        let pkgDbs = [bcoSnapDB eeBaseConfigOpts]

        case mlib of
            Nothing -> return $ Just $ Executable taskProvides
            Just _ -> do
                mpkgid <- loadInstalledPkg eeEnvOverride wc pkgDbs eeSnapshotDumpPkgs pname

                return $ Just $
                    case mpkgid of
                        Nothing -> assert False $ Executable taskProvides
                        Just pkgid -> Library taskProvides pkgid
      where
        bindir = toFilePath $ bcoSnapInstallRoot eeBaseConfigOpts </> bindirSuffix

    realConfigAndBuild cache allDepsMap = withSingleContext runInBase ac ee task (Just allDepsMap) Nothing
        $ \package cabalfp pkgDir cabal announce _console _mlogFile -> do
            _neededConfig <- ensureConfig cache pkgDir ee (announce ("configure" <> annSuffix)) cabal cabalfp

            if boptsCLIOnlyConfigure eeBuildOptsCLI
                then return Nothing
                else liftM Just $ realBuild cache package pkgDir cabal announce

    realBuild cache package pkgDir cabal announce = do
        wc <- getWhichCompiler

        markExeNotInstalled (taskLocation task) taskProvides
        case taskType of
            TTLocal lp -> do
                when enableTests $ unsetTestSuccess pkgDir
                writeBuildCache pkgDir $ lpNewBuildCache lp
            TTUpstream _ _ -> return ()

        () <- announce ("build" <> annSuffix)
        config <- asks getConfig
        extraOpts <- extraBuildOptions eeBuildOpts
        preBuildTime <- modTime <$> liftIO getCurrentTime
        cabal (configHideTHLoading config) $ ("build" :) $ (++ extraOpts) $
            case (taskType, taskAllInOne, isFinalBuild) of
                (_, True, True) -> fail "Invariant violated: cannot have an all-in-one build that also has a final build step."
                (TTLocal lp, False, False) -> primaryComponentOptions lp
                (TTLocal lp, False, True) -> finalComponentOptions lp
                (TTLocal lp, True, False) -> primaryComponentOptions lp ++ finalComponentOptions lp
                (TTUpstream{}, _, _) -> []
        checkForUnlistedFiles taskType preBuildTime pkgDir

        when (doHaddock package) $ do
            announce "haddock"
            sourceFlag <- do
                hyped <- tryProcessStdout Nothing eeEnvOverride "haddock" ["--hyperlinked-source"]
                case hyped of
                    -- Fancy crosslinked source
                    Right _ -> do
                        return ["--haddock-option=--hyperlinked-source"]
                    -- Older hscolour colouring
                    Left _  -> do
                        hscolourExists <- doesExecutableExist eeEnvOverride "HsColour"
                        unless hscolourExists $ $logWarn
                            ("Warning: haddock not generating hyperlinked sources because 'HsColour' not\n" <>
                             "found on PATH (use 'stack install hscolour' to install).")
                        return ["--hyperlink-source" | hscolourExists]
            cabal False (concat [["haddock", "--html", "--hoogle", "--html-location=../$pkg-$version/"]
                                ,sourceFlag])

        unless isFinalBuild $ withMVar eeInstallLock $ \() -> do
            announce "copy/register"
            eres <- try $ cabal False ["copy"]
            case eres of
                Left err@CabalExitedUnsuccessfully{} ->
                    throwM $ CabalCopyFailed (packageSimpleType package) (show err)
                _ -> return ()
            when (packageHasLibrary package) $ cabal False ["register"]

        let (installedPkgDb, installedDumpPkgsTVar) =
                case taskLocation task of
                    Snap ->
                         ( bcoSnapDB eeBaseConfigOpts
                         , eeSnapshotDumpPkgs )
                    Local ->
                        ( bcoLocalDB eeBaseConfigOpts
                        , eeLocalDumpPkgs )
        let ident = PackageIdentifier (packageName package) (packageVersion package)
        mpkgid <- if packageHasLibrary package
            then do
                mpkgid <- loadInstalledPkg eeEnvOverride wc [installedPkgDb] installedDumpPkgsTVar (packageName package)
                case mpkgid of
                    Nothing -> throwM $ Couldn'tFindPkgId $ packageName package
                    Just pkgid -> return $ Library ident pkgid
            else do
                markExeInstalled (taskLocation task) taskProvides -- TODO unify somehow with writeFlagCache?
                return $ Executable ident

        case taskLocation task of
            Snap -> writePrecompiledCache eeBaseConfigOpts taskProvides
                (configCacheOpts cache)
                (configCacheDeps cache)
                mpkgid (packageExes package)
            Local -> return ()

        return mpkgid

    loadInstalledPkg menv wc pkgDbs tvar name = do
        dps <- ghcPkgDescribe name menv wc pkgDbs $ conduitDumpPackage =$ CL.consume
        case dps of
            [] -> return Nothing
            [dp] -> do
                liftIO $ atomically $ modifyTVar' tvar (Map.insert (dpGhcPkgId dp) dp)
                return $ Just (dpGhcPkgId dp)
            _ -> error "singleBuild: invariant violated: multiple results when describing installed package"

-- | Check if any unlisted files have been found, and add them to the build cache.
checkForUnlistedFiles :: M env m => TaskType -> ModTime -> Path Abs Dir -> m ()
checkForUnlistedFiles (TTLocal lp) preBuildTime pkgDir = do
    (addBuildCache,warnings) <-
        addUnlistedToBuildCache
            preBuildTime
            (lpPackage lp)
            (lpCabalFile lp)
            (lpNewBuildCache lp)
    mapM_ ($logWarn . ("Warning: " <>) . T.pack . show) warnings
    unless (null addBuildCache) $
        writeBuildCache pkgDir $
        Map.unions (lpNewBuildCache lp : addBuildCache)
checkForUnlistedFiles (TTUpstream _ _) _ _ = return ()

-- | Determine if all of the dependencies given are installed
depsPresent :: InstalledMap -> Map PackageName VersionRange -> Bool
depsPresent installedMap deps = all
    (\(name, range) ->
        case Map.lookup name installedMap of
            Just (_, installed) -> installedVersion installed `withinRange` range
            Nothing -> False)
    (Map.toList deps)

singleTest :: M env m
           => (m () -> IO ())
           -> TestOpts
           -> [Text]
           -> ActionContext
           -> ExecuteEnv
           -> Task
           -> InstalledMap
           -> m ()
singleTest runInBase topts testsToRun ac ee task installedMap = do
    -- FIXME: Since this doesn't use cabal, we should be able to avoid using a
    -- fullblown 'withSingleContext'.
    (allDepsMap, _cache) <- getConfigCache ee task installedMap True False
    withSingleContext runInBase ac ee task (Just allDepsMap) (Just "test") $ \package _cabalfp pkgDir _cabal announce _console mlogFile -> do
        config <- asks getConfig
        let needHpc = toCoverage topts

        toRun <-
            if toDisableRun topts
              then do
                  announce "Test running disabled by --no-run-tests flag."
                  return False
              else if toRerunTests topts
                  then return True
                  else do
                      success <- checkTestSuccess pkgDir
                      if success
                          then do
                              unless (null testsToRun) $ announce "skipping already passed test"
                              return False
                          else return True

        when toRun $ do
            buildDir <- distDirFromDir pkgDir
            hpcDir <- hpcDirFromDir pkgDir
            when needHpc (ensureDir hpcDir)

            let suitesToRun
                  = [ testSuitePair
                    | testSuitePair <- Map.toList $ packageTests package
                    , let testName = fst testSuitePair
                    , testName `elem` testsToRun
                    ]

            errs <- liftM Map.unions $ forM suitesToRun $ \(testName, suiteInterface) -> do
                let stestName = T.unpack testName
                (testName', isTestTypeLib) <-
                    case suiteInterface of
                        C.TestSuiteLibV09{} -> return (stestName ++ "Stub", True)
                        C.TestSuiteExeV10{} -> return (stestName, False)
                        interface -> throwM (TestSuiteTypeUnsupported interface)

                let exeName = testName' ++
                        case configPlatform config of
                            Platform _ Windows -> ".exe"
                            _ -> ""
                tixPath <- liftM (pkgDir </>) $ parseRelFile $ exeName ++ ".tix"
                exePath <- liftM (buildDir </>) $ parseRelFile $ "build/" ++ testName' ++ "/" ++ exeName
                exists <- doesFileExist exePath
                menv <- liftIO $    configEnvOverride config EnvSettings
                    { esIncludeLocals = taskLocation task == Local
                    , esIncludeGhcPackagePath = True
                    , esStackExe = True
                    , esLocaleUtf8 = False
                    }
                if exists
                    then do
                        -- We clear out the .tix files before doing a run.
                        when needHpc $ do
                            tixexists <- doesFileExist tixPath
                            when tixexists $
                                $logWarn ("Removing HPC file " <> T.pack (toFilePath tixPath))
                            ignoringAbsence (removeFile tixPath)

                        let args = toAdditionalArgs topts
                            argsDisplay = case args of
                                            [] -> ""
                                            _ -> ", args: " <> T.intercalate " " (map showProcessArgDebug args)
                        announce $ "test (suite: " <> testName <> argsDisplay <> ")"

                        -- Clear "Progress: ..." message before
                        -- redirecting output.
                        when (isNothing mlogFile) $ do
                            $logStickyDone ""
                            liftIO $ hFlush stdout
                            liftIO $ hFlush stderr

                        let output =
                                case mlogFile of
                                    Nothing -> Inherit
                                    Just (_, h) -> UseHandle h

                        -- Use createProcess_ to avoid the log file being closed afterwards
                        (Just inH, Nothing, Nothing, ph) <- createProcess'
                            stestName
                            (\cp -> cp { std_in = CreatePipe, std_out = output, std_err = output })
                            (Cmd (Just pkgDir) (toFilePath exePath) menv args)
                        when isTestTypeLib $ do
                            logPath <- buildLogPath package (Just stestName)
                            ensureDir (parent logPath)
                            liftIO $ hPutStr inH $ show (logPath, testName)
                        liftIO $ hClose inH
                        ec <- liftIO $ waitForProcess ph
                        -- Add a trailing newline, incase the test
                        -- output didn't finish with a newline.
                        when (isNothing mlogFile) ($logInfo "")
                        -- Move the .tix file out of the package
                        -- directory into the hpc work dir, for
                        -- tidiness.
                        when needHpc $
                            updateTixFile (packageName package) tixPath testName'
                        return $ case ec of
                            ExitSuccess -> Map.empty
                            _ -> Map.singleton testName $ Just ec
                    else do
                        $logError $ T.pack $ show $ TestSuiteExeMissing
                            (packageSimpleType package)
                            exeName
                            (packageNameString (packageName package))
                            (T.unpack testName)
                        return $ Map.singleton testName Nothing

            when needHpc $ do
                let testsToRun' = map f testsToRun
                    f tName =
                        case Map.lookup tName (packageTests package) of
                            Just C.TestSuiteLibV09{} -> tName <> "Stub"
                            _ -> tName
                generateHpcReport pkgDir package testsToRun'

            bs <- liftIO $
                case mlogFile of
                    Nothing -> return ""
                    Just (logFile, h) -> do
                        hClose h
                        S.readFile $ toFilePath logFile

            unless (Map.null errs) $ throwM $ TestSuiteFailure
                (taskProvides task)
                errs
                (fmap fst mlogFile)
                bs

singleBench :: M env m
            => (m () -> IO ())
            -> BenchmarkOpts
            -> [Text]
            -> ActionContext
            -> ExecuteEnv
            -> Task
            -> InstalledMap
            -> m ()
singleBench runInBase beopts benchesToRun ac ee task installedMap = do
    (allDepsMap, _cache) <- getConfigCache ee task installedMap False True
    withSingleContext runInBase ac ee task (Just allDepsMap) (Just "bench") $ \_package _cabalfp _pkgDir cabal announce _console _mlogFile -> do
        let args = map T.unpack benchesToRun <> maybe []
                         ((:[]) . ("--benchmark-options=" <>))
                         (beoAdditionalArgs beopts)

        toRun <-
            if beoDisableRun beopts
              then do
                  announce "Benchmark running disabled by --no-run-benchmarks flag."
                  return False
              else do
                  return True

        when toRun $ do
          announce "benchmarks"
          cabal False ("bench" : args)

-- | Strip Template Haskell "Loading package" lines and making paths absolute.
mungeBuildOutput :: (MonadIO m, MonadCatch m)
                 => Bool -- ^ exclude TH loading?
                 -> Bool -- ^ convert paths to absolute?
                 -> Path Abs Dir -- ^ package's root directory
                 -> ConduitM Text Text m ()
mungeBuildOutput excludeTHLoading makeAbsolute pkgDir = void $
    CT.lines
    =$ CL.map stripCarriageReturn
    =$ CL.filter (not . isTHLoading)
    =$ CL.mapM toAbsolutePath
  where
    -- | Is this line a Template Haskell "Loading package" line
    -- ByteString
    isTHLoading :: Text -> Bool
    isTHLoading _ | not excludeTHLoading = False
    isTHLoading bs =
        "Loading package " `T.isPrefixOf` bs &&
        ("done." `T.isSuffixOf` bs || "done.\r" `T.isSuffixOf` bs)

    -- | Convert GHC error lines with file paths to have absolute file paths
    toAbsolutePath bs | not makeAbsolute = return bs
    toAbsolutePath bs = do
        let (x, y) = T.break (== ':') bs
        mabs <-
            if isValidSuffix y
                then liftM (fmap ((T.takeWhile isSpace x <>) . T.pack . toFilePath)) $
                        forgivingAbsence (resolveFile pkgDir (T.unpack $ T.dropWhile isSpace x))
                else return Nothing
        case mabs of
            Nothing -> return bs
            Just fp -> return $ fp `T.append` y

    -- | Match the line:column format at the end of lines
    isValidSuffix = isRight . parseOnly lineCol
    lineCol = char ':' >> (decimal :: Parser Int)
           >> char ':' >> (decimal :: Parser Int)
           >> char ':'
           >> return ()

    -- | Strip @\r@ characters from the byte vector. Used because Windows.
    stripCarriageReturn :: Text -> Text
    stripCarriageReturn = T.filter (/= '\r')

-- | Find the Setup.hs or Setup.lhs in the given directory. If none exists,
-- throw an exception.
getSetupHs :: Path Abs Dir -- ^ project directory
           -> IO (Path Abs File)
getSetupHs dir = do
    exists1 <- doesFileExist fp1
    if exists1
        then return fp1
        else do
            exists2 <- doesFileExist fp2
            if exists2
                then return fp2
                else throwM $ NoSetupHsFound dir
  where
    fp1 = dir </> $(mkRelFile "Setup.hs")
    fp2 = dir </> $(mkRelFile "Setup.lhs")

-- Do not pass `-hpcdir` as GHC option if the coverage is not enabled.
-- This helps running stack-compiled programs with dynamic interpreters like `hint`.
-- Cfr: https://github.com/commercialhaskell/stack/issues/997
extraBuildOptions :: M env m => BuildOpts -> m [String]
extraBuildOptions bopts = do
    let ddumpOpts = " -ddump-hi -ddump-to-file"
    case toCoverage (boptsTestOpts bopts) of
      True -> do
        hpcIndexDir <- toFilePathNoTrailingSep <$> hpcRelativeDir
        return ["--ghc-options", "-hpcdir " ++ hpcIndexDir ++ ddumpOpts]
      False -> return ["--ghc-options", ddumpOpts]

-- Library and executable build components.
primaryComponentOptions :: LocalPackage -> [String]
primaryComponentOptions lp = concat
    [ ["lib:" ++ packageNameString (packageName (lpPackage lp))
      -- TODO: get this information from target parsing instead,
      -- which will allow users to turn off library building if
      -- desired
      | packageHasLibrary (lpPackage lp)]
    , map (T.unpack . T.append "exe:") $ Set.toList $ exesToBuild lp
    ]

exesToBuild :: LocalPackage -> Set Text
exesToBuild lp = packageExes (lpPackage lp)
    -- NOTE: Ideally we'd do something like the following code, allowing
    -- the user to control which executables get built. However, due to
    -- https://github.com/haskell/cabal/issues/2780 we must build all
    -- exes...
    --
    -- if lpWanted lp
    --     then exeComponents (lpComponents lp)
    --     -- Build all executables in the event that no
    --     -- specific list is provided (as happens with
    --     -- extra-deps).
    --     else packageExes (lpPackage lp)

-- Test-suite and benchmark build components.
finalComponentOptions :: LocalPackage -> [String]
finalComponentOptions lp =
    map (T.unpack . decodeUtf8 . renderComponent) $
    Set.toList $
    Set.filter (\c -> isCTest c || isCBench c) (lpComponents lp)

taskComponents :: Task -> Set NamedComponent
taskComponents task =
    case taskType task of
        TTLocal lp -> lpComponents lp
        TTUpstream{} -> Set.empty

-- | Take the given list of package dependencies and the contents of the global
-- package database, and construct a set of installed package IDs that:
--
-- * Excludes the Cabal library (it's added later)
--
-- * Includes all packages depended on by this package
--
-- * Includes all global packages, unless: (1) it's hidden, (2) it's shadowed
--   by a depended-on package, or (3) one of its dependencies is not met.
--
-- See:
--
-- * https://github.com/commercialhaskell/stack/issues/941
--
-- * https://github.com/commercialhaskell/stack/issues/944
--
-- * https://github.com/commercialhaskell/stack/issues/949
addGlobalPackages :: Map PackageIdentifier GhcPkgId -- ^ dependencies of the package
                  -> [DumpPackage () ()] -- ^ global packages
                  -> Set GhcPkgId
addGlobalPackages deps globals0 =
    res
  where
    -- Initial set of packages: the installed IDs of all dependencies
    res0 = Map.elems $ Map.filterWithKey (\ident _ -> not $ isCabal ident) deps

    -- First check on globals: it's not shadowed by a dep, it's not Cabal, and
    -- it's exposed
    goodGlobal1 dp = not (isDep dp)
                  && not (isCabal $ dpPackageIdent dp)
                  && dpIsExposed dp
    globals1 = filter goodGlobal1 globals0

    -- Create a Map of unique package names in the global database
    globals2 = Map.fromListWith chooseBest
             $ map (packageIdentifierName . dpPackageIdent &&& id) globals1

    -- Final result: add in globals that have their dependencies met
    res = loop id (Map.elems globals2) $ Set.fromList res0

    ----------------------------------
    -- Some auxiliary helper functions
    ----------------------------------

    -- Is the given package identifier for any version of Cabal
    isCabal (PackageIdentifier name _) = name == $(mkPackageName "Cabal")

    -- Is the given package name provided by the package dependencies?
    isDep dp = packageIdentifierName (dpPackageIdent dp) `Set.member` depNames
    depNames = Set.map packageIdentifierName $ Map.keysSet deps

    -- Choose the best of two competing global packages (the newest version)
    chooseBest dp1 dp2
        | getVer dp1 < getVer dp2 = dp2
        | otherwise               = dp1
      where
        getVer = packageIdentifierVersion . dpPackageIdent

    -- Are all dependencies of the given package met by the given Set of
    -- installed packages
    depsMet dp gids = all (`Set.member` gids) (dpDepends dp)

    -- Find all globals that have all of their dependencies met
    loop front (dp:dps) gids
        -- This package has its deps met. Add it to the list of dependencies
        -- and then traverse the list from the beginning (this package may have
        -- been a dependency of an earlier one).
        | depsMet dp gids = loop id (front dps) (Set.insert (dpGhcPkgId dp) gids)
        -- Deps are not met, keep going
        | otherwise = loop (front . (dp:)) dps gids
    -- None of the packages we checked can be added, therefore drop them all
    -- and return our results
    loop _ [] gids = gids
