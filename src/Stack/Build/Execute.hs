{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
-- Perform a build
module Stack.Build.Execute
    ( printPlan
    , preFetch
    , executePlan
    ) where

import           Control.Applicative            ((<$>), (<*>))
import           Control.Concurrent.Lifted (fork)
import           Control.Concurrent.Execute
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Catch            (MonadCatch, MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader           (MonadReader, asks)
import           Control.Monad.Trans.Control    (liftBaseWith)
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import qualified Data.ByteString                as S
import qualified Data.ByteString.Char8          as S8
import           Data.Conduit
import qualified Data.Conduit.Binary            as CB
import qualified Data.Conduit.List              as CL
import           Data.Function
import           Data.List
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as M
import qualified Data.Map.Strict                as Map
import           Data.Maybe
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Streaming.Process         hiding (callProcess, env)
import qualified Data.Streaming.Process         as Process
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Text.Encoding             (encodeUtf8)
import           Distribution.System            (OS (Windows),
                                                 Platform (Platform))
import           Network.HTTP.Client.Conduit    (HasHttpManager)
import           Path
import           Path.IO
import           Prelude                        hiding (FilePath, writeFile)
import           Stack.Build.Cache
import           Stack.Build.Installed
import           Stack.Build.Types
import           Stack.Fetch                    as Fetch
import           Stack.GhcPkg
import           Stack.Package
import           Stack.Constants
import           Stack.Types
import           Stack.Types.StackT
import           Stack.Types.Internal
import           System.Directory               hiding (findExecutable,
                                                 findFiles)
import           System.Environment             (getExecutablePath)
import           System.Exit                    (ExitCode (ExitSuccess))
import qualified System.FilePath                as FP
import           System.IO
import           System.IO.Temp                 (withSystemTempDirectory)
import           System.Process.Internals       (createProcess_)
import           System.Process.Read
import           System.Process.Log             (showProcessArgDebug)

type M env m = (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m,HasLogLevel env,HasEnvConfig env,HasTerminal env)

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

printPlan :: M env m
          => FinalAction
          -> Plan
          -> m ()
printPlan finalAction plan = do
    case Set.toList $ planUnregisterLocal plan of
        [] -> $logInfo "Nothing to unregister"
        xs -> do
            $logInfo "Would unregister locally:"
            mapM_ ($logInfo . T.pack . ghcPkgIdString) xs

    $logInfo ""

    case Map.elems $ planTasks plan of
        [] -> $logInfo "Nothing to build"
        xs -> do
            $logInfo "Would build:"
            mapM_ ($logInfo . displayTask) xs

    let mfinalLabel =
            case finalAction of
                DoNothing -> Nothing
                DoBenchmarks -> Just "benchmark"
                DoTests -> Just "test"
    case mfinalLabel of
        Nothing -> return ()
        Just finalLabel -> do
            $logInfo ""

            case Map.toList $ planFinals plan of
                [] -> $logInfo $ "Nothing to " <> finalLabel
                xs -> do
                    $logInfo $ "Would " <> finalLabel <> ":"
                    forM_ xs $ \(name, _) -> $logInfo $ T.pack $ packageNameString name

    $logInfo ""

    case Map.toList $ planInstallExes plan of
        [] -> $logInfo "No executables to be installed"
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
    , eeBaseConfigOpts :: !BaseConfigOpts
    , eeGhcPkgIds      :: !(TVar (Map PackageIdentifier Installed))
    , eeTempDir        :: !(Path Abs Dir)
    , eeSetupHs        :: !(Path Abs File)
    , eeCabalPkgVer    :: !Version
    , eeTotalWanted    :: !Int
    , eeWanted         :: !(Set PackageName)
    , eeLocals         :: ![LocalPackage]
    }

-- | Perform the actual plan
executePlan :: M env m
            => EnvOverride
            -> BuildOpts
            -> BaseConfigOpts
            -> [LocalPackage]
            -> Plan
            -> m ()
executePlan menv bopts baseConfigOpts locals plan = do
    withSystemTempDirectory stackProgName $ \tmpdir -> do
        tmpdir' <- parseAbsDir tmpdir
        configLock <- newMVar ()
        installLock <- newMVar ()
        idMap <- liftIO $ newTVarIO M.empty
        let setupHs = tmpdir' </> $(mkRelFile "Setup.hs")
        liftIO $ writeFile (toFilePath setupHs) "import Distribution.Simple\nmain = defaultMain"
        cabalPkgVer <- asks (envConfigCabalVersion . getEnvConfig)
        executePlan' plan ExecuteEnv
            { eeEnvOverride = menv
            , eeBuildOpts = bopts
             -- Uncertain as to why we cannot run configures in parallel. This appears
             -- to be a Cabal library bug. Original issue:
             -- https://github.com/fpco/stack/issues/84. Ideally we'd be able to remove
             -- this.
            , eeConfigureLock = configLock
            , eeInstallLock = installLock
            , eeBaseConfigOpts = baseConfigOpts
            , eeGhcPkgIds = idMap
            , eeTempDir = tmpdir'
            , eeSetupHs = setupHs
            , eeCabalPkgVer = cabalPkgVer
            , eeTotalWanted = length $ filter lpWanted locals
            , eeWanted = wantedLocalPackages locals
            , eeLocals = locals
            }

    unless (Map.null $ planInstallExes plan) $ do
        snapBin <- (</> bindirSuffix) `liftM` installationRootDeps
        localBin <- (</> bindirSuffix) `liftM` installationRootLocal
        destDir <- asks $ configLocalBin . getConfig
        let destDir' = toFilePath destDir
        liftIO $ createDirectoryIfMissing True destDir'

        when (not $ any (FP.equalFilePath destDir') (envSearchPath menv)) $
            $logWarn $ T.concat
                [ "Installation path "
                , T.pack destDir'
                , " not found in PATH environment variable"
                ]

        platform <- asks getPlatform
        let ext =
                case platform of
                    Platform _ Windows -> ".exe"
                    _ -> ""

        currExe <- liftIO getExecutablePath -- needed for windows, see below

        installed <- forM (Map.toList $ planInstallExes plan) $ \(name, loc) -> do
            let bindir =
                    case loc of
                        Snap -> snapBin
                        Local -> localBin
            mfp <- resolveFileMaybe bindir $ T.unpack name ++ ext
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
                        _ -> copyFile (toFilePath file) destFile
                    return $ Just (destDir', [T.append name (T.pack ext)])

        let destToInstalled = Map.fromListWith (++) (catMaybes installed)
        unless (Map.null destToInstalled) $ $logInfo ""
        forM_ (Map.toList destToInstalled) $ \(dest, executables) -> do
            $logInfo $ T.concat
                [ "Installed executables to "
                , T.pack dest
                , ":"]
            forM_ executables $ \exe -> $logInfo $ T.append "- " exe

-- | Windows can't write over the current executable. Instead, we rename the
-- current executable to something else and then do the copy.
windowsRenameCopy :: FilePath -> FilePath -> IO ()
windowsRenameCopy src dest = do
    copyFile src new
    renameFile dest old
    renameFile new dest
  where
    new = dest ++ ".new"
    old = dest ++ ".old"

-- | Perform the actual plan (internal)
executePlan' :: M env m
             => Plan
             -> ExecuteEnv
             -> m ()
executePlan' plan ee@ExecuteEnv {..} = do
    case Set.toList $ planUnregisterLocal plan of
        [] -> return ()
        ids -> do
            localDB <- packageDatabaseLocal
            forM_ ids $ \id' -> do
                $logInfo $ T.concat
                    [ T.pack $ ghcPkgIdString id'
                    , ": unregistering"
                    ]
                unregisterGhcPkgId eeEnvOverride localDB id'

    -- Yes, we're explicitly discarding result values, which in general would
    -- be bad. monad-unlift does this all properly at the type system level,
    -- but I don't want to pull it in for this one use case, when we know that
    -- stack always using transformer stacks that are safe for this use case.
    runInBase <- liftBaseWith $ \run -> return (void . run)

    let actions = concatMap (toActions runInBase ee) $ Map.elems $ Map.mergeWithKey
            (\_ b f -> Just (Just b, Just f))
            (fmap (\b -> (Just b, Nothing)))
            (fmap (\f -> (Nothing, Just f)))
            (planTasks plan)
            (planFinals plan)
    threads <- asks $ configJobs . getConfig
    terminal <- asks getTerminal
    errs <- liftIO $ runActions threads actions $ \doneVar -> do
        let total = length actions
            loop prev
                | prev == total =
                    runInBase $ $logStickyDone ("Completed all " <> T.pack (show total) <> " actions.")
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
    unless (null errs) $ throwM $ ExecutionFailure errs
    when (boptsHaddock eeBuildOpts && not (null actions))
        (generateHaddockIndex ee)

toActions :: M env m
          => (m () -> IO ())
          -> ExecuteEnv
          -> (Maybe Task, Maybe Task) -- build and final
          -> [Action]
toActions runInBase ee (mbuild, mfinal) =
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
                    , actionDo = \ac -> runInBase $ singleBuild ac ee task
                    }
                ]
    afinal =
        case (,) <$> mfinal <*> mfunc of
            Just (task@Task {..}, (func, checkTask)) | checkTask task ->
                [ Action
                    { actionId = ActionId taskProvides ATFinal
                    , actionDeps = addBuild taskProvides $
                        (Set.map (\ident -> ActionId ident ATBuild) (tcoMissing taskConfigOpts))
                    , actionDo = \ac -> runInBase $ func ac ee task
                    }
                ]
            _ -> []
      where
        addBuild ident =
            case mbuild of
                Nothing -> id
                Just _ -> Set.insert $ ActionId ident ATBuild

    mfunc =
        case boptsFinalAction $ eeBuildOpts ee of
            DoNothing -> Nothing
            DoTests -> Just (singleTest, checkTest)
            DoBenchmarks -> Just (singleBench, checkBench)

    checkTest task =
        case taskType task of
            TTLocal lp -> not $ Set.null $ packageTests $ lpPackage lp
            _ -> assert False False

    checkBench task =
        case taskType task of
            TTLocal lp -> not $ Set.null $ packageBenchmarks $ lpPackage lp
            _ -> assert False False

-- | Ensure that the configuration for the package matches what is given
ensureConfig :: M env m
             => Path Abs Dir -- ^ package directory
             -> ExecuteEnv
             -> Task
             -> m () -- ^ announce
             -> (Bool -> [String] -> m ()) -- ^ cabal
             -> Path Abs File -- ^ .cabal file
             -> [Text]
             -> m (ConfigCache, Bool)
ensureConfig pkgDir ExecuteEnv {..} Task {..} announce cabal cabalfp extra = do
    -- Determine the old and new configuration in the local directory, to
    -- determine if we need to reconfigure.
    mOldConfigCache <- tryGetConfigCache pkgDir

    mOldCabalMod <- tryGetCabalMod pkgDir
    newCabalMod <- liftIO (fmap modTime (getModificationTime (toFilePath cabalfp)))

    idMap <- liftIO $ readTVarIO eeGhcPkgIds
    let getMissing ident =
            case Map.lookup ident idMap of
                Nothing -> error "singleBuild: invariant violated, missing package ID missing"
                Just (Library x) -> Just x
                Just (Executable _) -> Nothing
        missing' = Set.fromList $ mapMaybe getMissing $ Set.toList missing
        TaskConfigOpts missing mkOpts = taskConfigOpts
        configOpts = mkOpts missing' ++ extra
        allDeps = Set.union missing' taskPresent
        newConfigCache = ConfigCache
            { configCacheOpts = map encodeUtf8 configOpts
            , configCacheDeps = allDeps
            , configCacheComponents =
                case taskType of
                    TTLocal lp -> Set.map encodeUtf8 $ lpComponents lp
                    TTUpstream _ _ -> Set.empty
            , configCacheHaddock =
                shouldBuildHaddock eeBuildOpts eeWanted (packageIdentifierName taskProvides)
            }

    let needConfig = mOldConfigCache /= Just newConfigCache
                  || mOldCabalMod /= Just newCabalMod
    when needConfig $ withMVar eeConfigureLock $ \_ -> do
        deleteCaches pkgDir
        announce
        cabal False $ "configure" : map T.unpack configOpts
        writeConfigCache pkgDir newConfigCache
        writeCabalMod pkgDir newCabalMod

    return (newConfigCache, needConfig)

withSingleContext :: M env m
                  => ActionContext
                  -> ExecuteEnv
                  -> Task
                  -> (  Package
                     -> Path Abs File
                     -> Path Abs Dir
                     -> (Bool -> [String] -> m ())
                     -> (Text -> m ())
                     -> Bool
                     -> Maybe (Path Abs File, Handle)
                     -> m a)
                  -> m a
withSingleContext ActionContext {..} ExecuteEnv {..} task@Task {..} inner0 =
    withPackage $ \package cabalfp pkgDir ->
    withLogFile package $ \mlogFile ->
    withCabal pkgDir mlogFile $ \cabal ->
    inner0 package cabalfp pkgDir cabal announce console mlogFile
  where
    announce x = $logInfo $ T.concat
        [ T.pack $ packageIdentifierString taskProvides
        , ": "
        , x
        ]

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
                case M.toList m of
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
            logPath <- buildLogPath package -- TODO give a difference suffix for test, bench, etc?
            liftIO $ createDirectoryIfMissing True $ toFilePath $ parent logPath
            let fp = toFilePath logPath
            bracket
                (liftIO $ openBinaryFile fp WriteMode)
                (liftIO . hClose)
                $ \h -> inner (Just (logPath, h))

    withCabal pkgDir mlogFile inner = do
        config <- asks getConfig
        menv <- liftIO $ configEnvOverride config EnvSettings
            { esIncludeLocals = taskLocation task == Local
            , esIncludeGhcPackagePath = False
            }
        exeName <- liftIO $ join $ findExecutable menv "runhaskell"
        distRelativeDir' <- distRelativeDir
        msetuphs <- liftIO $ getSetupHs pkgDir
        let setuphs = fromMaybe eeSetupHs msetuphs
        inner $ \stripTHLoading args -> do
            let fullArgs =
                      ("-package=" ++
                       packageIdentifierString
                           (PackageIdentifier cabalPackageName
                                              eeCabalPkgVer))
                    : "-clear-package-db"
                    : "-global-package-db"

                    -- This next line is debatable. It adds access to the
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
                    -- One possible solution to these points would be to use
                    -- -hide-all-packages and explicitly list which packages
                    -- can be used by Setup.hs, and have that based on the
                    -- dependencies of the package itself.
                    : ("-package-db=" ++ toFilePath (bcoSnapDB eeBaseConfigOpts))

                    : toFilePath setuphs
                    : ("--builddir=" ++ toFilePath distRelativeDir')
                    : args
                cp0 = proc (toFilePath exeName) fullArgs
                cp = cp0
                    { cwd = Just $ toFilePath pkgDir
                    , Process.env = envHelper menv
                    , std_in = CreatePipe
                    , std_out =
                        case mlogFile of
                                Nothing -> CreatePipe
                                Just (_, h) -> UseHandle h
                    , std_err =
                        case mlogFile of
                            Nothing -> Inherit
                            Just (_, h) -> UseHandle h
                    }
            $logProcessRun (toFilePath exeName) fullArgs

            -- Use createProcess_ to avoid the log file being closed afterwards
            (Just inH, moutH, Nothing, ph) <- liftIO $ createProcess_ "singleBuild" cp
            liftIO $ hClose inH
            case moutH of
                Just outH ->
                    case mlogFile of
                      Just{} -> return ()
                      Nothing -> printBuildOutput stripTHLoading outH
                Nothing -> return ()
            ec <- liftIO $ waitForProcess ph
            case ec of
                ExitSuccess -> return ()
                _ -> do
                    bs <- liftIO $
                        case mlogFile of
                            Nothing -> return ""
                            Just (logFile, h) -> do
                                hClose h
                                S.readFile $ toFilePath logFile
                    throwM $ CabalExitedUnsuccessfully
                        ec
                        taskProvides
                        exeName
                        fullArgs
                        (fmap fst mlogFile)
                        bs

singleBuild :: M env m
            => ActionContext
            -> ExecuteEnv
            -> Task
            -> m ()
singleBuild ac@ActionContext {..} ee@ExecuteEnv {..} task@Task {..} =
  withSingleContext ac ee task $ \package cabalfp pkgDir cabal announce console _mlogFile -> do
    (cache, _neededConfig) <- ensureConfig pkgDir ee task (announce "configure") cabal cabalfp []

    fileModTimes <- getPackageFileModTimes package cabalfp
    writeBuildCache pkgDir fileModTimes

    announce "build"
    config <- asks getConfig
    cabal (console && configHideTHLoading config) $
        case taskType of
            TTLocal lp -> "build" : map T.unpack (Set.toList $ lpComponents lp)
            TTUpstream _ _ -> ["build"]

    when (shouldBuildHaddock eeBuildOpts eeWanted (packageName package) &&
          -- Works around haddock failing on bytestring-builder since it has no modules when
          -- bytestring is new enough.
          packageHasExposedModules package) $ do
        announce "haddock"
        hscolourExists <- doesExecutableExist eeEnvOverride "hscolour"
        cabal False (concat [["haddock", "--html", "--hoogle"]
                            ,["--hyperlink-source" | hscolourExists]])

    withMVar eeInstallLock $ \() -> do
        announce "install"
        cabal False ["install"]

    let pkgDbs =
            case taskLocation task of
                Snap -> [bcoSnapDB eeBaseConfigOpts]
                Local ->
                    [ bcoSnapDB eeBaseConfigOpts
                    , bcoLocalDB eeBaseConfigOpts
                    ]
    mpkgid <- findGhcPkgId eeEnvOverride pkgDbs (packageName package)
    mpkgid' <- case (packageHasLibrary package, mpkgid) of
        (False, _) -> assert (isNothing mpkgid) $ do
            markExeInstalled (taskLocation task) taskProvides -- TODO unify somehow with writeFlagCache?
            return $ Executable $ PackageIdentifier
                (packageName package)
                (packageVersion package)
        (True, Nothing) -> throwM $ Couldn'tFindPkgId $ packageName package
        (True, Just pkgid) -> return $ Library pkgid
    writeFlagCache mpkgid' cache
    liftIO $ atomically $ modifyTVar eeGhcPkgIds $ Map.insert taskProvides mpkgid'

singleTest :: M env m
           => ActionContext
           -> ExecuteEnv
           -> Task
           -> m ()
singleTest ac ee task =
    withSingleContext ac ee task $ \package cabalfp pkgDir cabal announce console mlogFile -> do
        (_cache, neededConfig) <- ensureConfig pkgDir ee task (announce "configure (test)") cabal cabalfp ["--enable-tests"]
        config <- asks getConfig

        let needBuild = neededConfig ||
                (case taskType task of
                    TTLocal lp -> lpDirtyFiles lp
                    _ -> assert False True)
                || True -- FIXME above logic is incorrect, see: https://github.com/commercialhaskell/stack/issues/319
        when needBuild $ do
            announce "build (test)"
            fileModTimes <- getPackageFileModTimes package cabalfp
            writeBuildCache pkgDir fileModTimes
            cabal (console && configHideTHLoading config) ["build"]

        bconfig <- asks getBuildConfig
        distRelativeDir' <- distRelativeDir
        let buildDir = pkgDir </> distRelativeDir'
        let exeExtension =
                case configPlatform $ getConfig bconfig of
                    Platform _ Windows -> ".exe"
                    _ -> ""

        errs <- liftM Map.unions $ forM (Set.toList $ packageTests package) $ \testName -> do
            nameDir <- liftIO $ parseRelDir $ T.unpack testName
            nameExe <- liftIO $ parseRelFile $ T.unpack testName ++ exeExtension
            let exeName = buildDir </> $(mkRelDir "build") </> nameDir </> nameExe
            exists <- fileExists exeName
            menv <- liftIO $ configEnvOverride config EnvSettings
                { esIncludeLocals = taskLocation task == Local
                , esIncludeGhcPackagePath = True
                }
            if exists
                then do
                    let args = boptsTestArgs (eeBuildOpts ee)
                        argsDisplay =
                            case args of
                              [] -> ""
                              _ -> ", args: " <> T.intercalate " " (map showProcessArgDebug args)
                    announce $ "test (suite: " <> testName <> argsDisplay <> ")"
                    let cp = (proc (toFilePath exeName) args)
                            { cwd = Just $ toFilePath pkgDir
                            , Process.env = envHelper menv
                            , std_in = CreatePipe
                            , std_out =
                                case mlogFile of
                                    Nothing -> Inherit
                                    Just (_, h) -> UseHandle h
                            , std_err =
                                case mlogFile of
                                    Nothing -> Inherit
                                    Just (_, h) -> UseHandle h
                            }

                    -- Use createProcess_ to avoid the log file being closed afterwards
                    (Just inH, Nothing, Nothing, ph) <- liftIO $ createProcess_ "singleBuild.runTests" cp
                    liftIO $ hClose inH
                    ec <- liftIO $ waitForProcess ph
                    return $ case ec of
                        ExitSuccess -> M.empty
                        _ -> M.singleton testName $ Just ec
                else do
                    $logError $ T.concat
                        [ "Test suite "
                        , testName
                        , " executable not found for "
                        , T.pack $ packageNameString $ packageName package
                        ]
                    return $ Map.singleton testName Nothing
        unless (Map.null errs) $ throwM $ TestSuiteFailure
            (taskProvides task)
            errs
            (fmap fst mlogFile)

singleBench :: M env m
            => ActionContext
            -> ExecuteEnv
            -> Task
            -> m ()
singleBench ac ee task =
    withSingleContext ac ee task $ \package cabalfp pkgDir cabal announce console _mlogFile -> do
        (_cache, neededConfig) <- ensureConfig pkgDir ee task (announce "configure (benchmarks)") cabal cabalfp ["--enable-benchmarks"]

        let needBuild = neededConfig ||
                (case taskType task of
                    TTLocal lp -> lpDirtyFiles lp
                    _ -> assert False True)
                || True -- FIXME above logic is incorrect, see: https://github.com/commercialhaskell/stack/issues/319
        when needBuild $ do
            announce "build (benchmarks)"
            fileModTimes <- getPackageFileModTimes package cabalfp
            writeBuildCache pkgDir fileModTimes
            config <- asks getConfig
            cabal (console && configHideTHLoading config) ["build"]

        announce "benchmarks"
        cabal False ["bench"]

-- | Generate Haddock index and contents for local packages.
generateHaddockIndex :: M env m
                     => ExecuteEnv
                     -> m ()
generateHaddockIndex ExecuteEnv {..} = do
    $logInfo ("Generating Haddock index/contents in\n" <>
              T.pack (toFilePath (docDir </> $(mkRelFile "index.html"))))
    interfaceArgs <- mapM (\LocalPackage {lpPackage = Package {..}} ->
                              toInterfaceOpt (PackageIdentifier packageName packageVersion))
                          eeLocals
    readProcessNull
        (Just docDir)
        eeEnvOverride
        "haddock"
        (["--gen-contents", "--gen-index"] ++ concat interfaceArgs)
  where
    docDir = bcoLocalInstallRoot eeBaseConfigOpts </> docdirSuffix
    toInterfaceOpt pid@(PackageIdentifier name _) = do
        interfaceRelFile <- parseRelFile (packageIdentifierString pid FP.</>
                                          packageNameString name FP.<.>
                                          "haddock")
        interfaceExists <- fileExists (docDir </> interfaceRelFile)
        return $ if interfaceExists
            then [ "-i"
                 , concat
                     [ packageIdentifierString pid
                     , ","
                     , toFilePath interfaceRelFile ] ]
            else []

-- | Grab all output from the given @Handle@ and print it to stdout, stripping
-- Template Haskell "Loading package" lines. Does work in a separate thread.
printBuildOutput :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
                 => Bool -> Handle -> m ()
printBuildOutput excludeTHLoading outH = void $ fork $
         CB.sourceHandle outH
    $$ CB.lines
    =$ CL.filter (not . isTHLoading)
    =$ CL.mapM_ ($logInfo . T.decodeUtf8)
  where
    -- | Is this line a Template Haskell "Loading package" line
    -- ByteString
    isTHLoading :: S8.ByteString -> Bool
    isTHLoading _ | not excludeTHLoading = False
    isTHLoading bs =
        "Loading package " `S8.isPrefixOf` bs &&
        ("done." `S8.isSuffixOf` bs || "done.\r" `S8.isSuffixOf` bs)

taskLocation :: Task -> Location
taskLocation task =
    case taskType task of
        TTLocal _ -> Local
        TTUpstream _ loc -> loc

-- | Ensure Setup.hs exists in the given directory. Returns an action
-- to remove it later.
getSetupHs :: Path Abs Dir -- ^ project directory
           -> IO (Maybe (Path Abs File))
getSetupHs dir = do
    exists1 <- fileExists fp1
    if exists1
        then return $ Just fp1
        else do
            exists2 <- fileExists fp2
            if exists2
                then return $ Just fp2
                else return Nothing
  where
    fp1 = dir </> $(mkRelFile "Setup.hs")
    fp2 = dir </> $(mkRelFile "Setup.lhs")
