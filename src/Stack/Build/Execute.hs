{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
-- Perform a build
module Stack.Build.Execute
    ( printPlan
    , executePlan
    ) where

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

type M env m = (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m,HasLogLevel env,HasEnvConfig env)

printPlan :: M env m => Plan -> m ()
printPlan plan = do
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
    , eeInstallLock    :: !(MVar (Int,Int))
    , eeBuildOpts      :: !BuildOpts
    , eeBaseConfigOpts :: !BaseConfigOpts
    , eeGhcPkgIds      :: !(TVar (Map PackageIdentifier Installed))
    , eeTempDir        :: !(Path Abs Dir)
    , eeSetupHs        :: !(Path Abs File)
    , eeCabalPkgVer    :: !Version
    , eeTotalWanted    :: !Int
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
        installLock <-
            newMVar (0,installStepCount plan)
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

        forM_ (Map.toList $ planInstallExes plan) $ \(name, loc) -> do
            let bindir =
                    case loc of
                        Snap -> snapBin
                        Local -> localBin
            mfp <- resolveFileMaybe bindir $ T.unpack name ++ ext
            case mfp of
                Nothing -> $logWarn $ T.concat
                    [ "Couldn't find executable "
                    , name
                    , " in directory "
                    , T.pack $ toFilePath bindir
                    ]
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

-- | Calculate how many actual install steps are going to happen for
-- the build.
installStepCount :: Plan -> Int
installStepCount = M.size . planTasks

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
executePlan' plan ee = do
    case Set.toList $ planUnregisterLocal plan of
        [] -> return ()
        ids -> do
            localDB <- packageDatabaseLocal
            forM_ ids $ \id' -> do
                $logInfo $ T.concat
                    [ T.pack $ ghcPkgIdString id'
                    , ": unregistering"
                    ]
                unregisterGhcPkgId (eeEnvOverride ee) localDB id'

    -- Yes, we're explicitly discarding result values, which in general would
    -- be bad. monad-unlift does this all properly at the type system level,
    -- but I don't want to pull it in for this one use case, when we know that
    -- stack always using transformer stacks that are safe for this use case.
    runInBase <- liftBaseWith $ \run -> return (void . run)

    let actions = concatMap (toActions runInBase ee) $ Map.elems $ planTasks plan
    threads <- asks $ configJobs . getConfig
    errs <- liftIO $ runActions threads actions
    unless (null errs) $ throwM $ ExecutionFailure errs

toActions :: M env m
          => (m () -> IO ())
          -> ExecuteEnv
          -> Task
          -> [Action]
toActions runInBase ee task@Task {..} =
    -- TODO in the future, we need to have proper support for cyclic
    -- dependencies from test suites, in which case we'll need more than one
    -- Action here

    [ Action
        { actionId = ActionId taskProvides ATBuild
        , actionDeps =
            (Set.map (\ident -> ActionId ident ATBuild) (tcoMissing taskConfigOpts))
        , actionDo = \ac -> runInBase $ singleBuild ac ee task
        }
    ]

singleBuild :: M env m
            => ActionContext
            -> ExecuteEnv
            -> Task
            -> m ()
singleBuild ActionContext {..} ExecuteEnv {..} task@Task {..} =
  withPackage $ \package cabalfp pkgDir ->
  withLogFile package $ \mlogFile ->
  withCabal pkgDir mlogFile $ \cabal -> do
    -- Determine the old and new configuration in the local directory, to
    -- determine if we need to reconfigure.
    mOldConfigCache <- tryGetConfigCache pkgDir

    idMap <- liftIO $ readTVarIO eeGhcPkgIds
    now <- liftIO (getModificationTime (toFilePath cabalfp))
    let getMissing ident =
            case Map.lookup ident idMap of
                Nothing -> error "singleBuild: invariant violated, missing package ID missing"
                Just (Library x) -> Just x
                Just Executable -> Nothing
        missing' = Set.fromList $ mapMaybe getMissing $ Set.toList missing
        TaskConfigOpts missing mkOpts = taskConfigOpts
        configOpts = mkOpts missing'
        allDeps = Set.union missing' taskPresent
        newConfigCache = ConfigCache
            { configCacheOpts = map encodeUtf8 configOpts
            , configCacheDeps = allDeps
            , configCabalFileModTime = Just $ modTime now
            }

    when (mOldConfigCache /= Just newConfigCache) $ withMVar eeConfigureLock $ \_ -> do
        deleteCaches pkgDir
        withMVar eeInstallLock $ \(done,total) ->
            $logSticky ("Progress: " <> T.pack (show done) <> "/" <> T.pack (show total))
        announce "configure"
        cabal False $ "configure" : map T.unpack configOpts
        $logDebug $ T.pack $ show configOpts
        writeConfigCache pkgDir newConfigCache

    fileModTimes <- getPackageFileModTimes package cabalfp
    writeBuildCache pkgDir fileModTimes

    announce "build"
    config <- asks getConfig
    cabal (console && configHideTHLoading config) ["build"]

    modifyMVar_ eeInstallLock $ \(done,total) -> do
        announce "install"
        cabal False ["install"]
        unless (total == 1) $ do
            let done' = done + 1
            $logSticky ("Progress: " <> T.pack (show done') <> "/" <> T.pack (show total))
            when (done' == total)
                 ($logStickyDone ("Completed all " <> T.pack (show total) <> " packages."))
        return (done + 1,total)

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
            markExeInstalled (taskLocation task) taskProvides -- FIXME this should also take the options, deps, etc
            return Executable
        (True, Nothing) -> throwM $ Couldn'tFindPkgId $ packageName package
        (True, Just pkgid) -> do
            writeFlagCache
                pkgid
                (map encodeUtf8 configOpts)
                allDeps
            return $ Library pkgid
    liftIO $ atomically $ modifyTVar eeGhcPkgIds $ Map.insert taskProvides mpkgid'
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

    console = wanted && acRemaining == 0 && eeTotalWanted == 1

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
            logPath <- buildLogPath package
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
                    -- TODO: Perhaps we want to include the snapshot package database here
                    -- as well
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
            $logDebug $ "Running: " <> T.pack (show $ toFilePath exeName : fullArgs)

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

    {- FIXME move to a separate step
    runTests package pkgDir mlogFile = do
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
            exists <- liftIO $ doesFileExist $ toFilePath exeName
            config <- asks getConfig
            menv <- liftIO $ configEnvOverride config EnvSettings
                { esIncludeLocals = taskLocation task == Local
                , esIncludeGhcPackagePath = True
                }
            if exists
                then do
                    announce $ "test " <> testName
                    let cp = (proc (toFilePath exeName) [])
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
        unless (Map.null errs) $ throwM $ TestSuiteFailure taskProvides errs (fmap fst mlogFile)

    case boptsFinalAction eeBuildOpts of
        DoTests -> when wanted $ do
            announce "test"
            runTests package pkgDir mlogFile
        DoBenchmarks -> when wanted $ do
            announce "benchmarks"
            cabal False ["bench"]
        DoHaddock -> do
            announce "haddock"
            hscolourExists <- doesExecutableExist eeEnvOverride "hscolour"
              {- EKB TODO: doc generation for stack-doc-server
 #ifndef mingw32_HOST_OS
              liftIO (removeDocLinks docLoc package)
 #endif
              ifcOpts <- liftIO (haddockInterfaceOpts docLoc package packages)
              -}
            cabal False (concat [["haddock", "--html"]
                                ,["--hyperlink-source" | hscolourExists]])
              {- EKB TODO: doc generation for stack-doc-server
                         ,"--hoogle"
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
                             "hoogle"
                             ["convert"
                             ,"--haddock"
                             ,hoogleTxtPath
                             ,hoogleDbPath])
                        -}
                 {- EKB TODO: doc generation for stack-doc-server
             #ifndef mingw32_HOST_OS
                 case setupAction of
                   DoHaddock -> liftIO (createDocLinks docLoc package)
                   _ -> return ()
             #endif

 -- | Package's documentation directory.
 packageDocDir :: (MonadThrow m, MonadReader env m, HasPlatform env)
               => PackageIdentifier -- ^ Cabal version
               -> Package
               -> m (Path Abs Dir)
 packageDocDir cabalPkgVer package' = do
   dist <- distDirFromDir cabalPkgVer (packageDir package')
   return (dist </> $(mkRelDir "doc/"))
                 --}
        DoNothing -> return ()
    -}

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
    exists1 <- doesFileExist (toFilePath fp1)
    if exists1
        then return $ Just fp1
        else do
            exists2 <- doesFileExist (toFilePath fp2)
            if exists2
                then return $ Just fp2
                else return Nothing
  where
    fp1 = dir </> $(mkRelFile "Setup.hs")
    fp2 = dir </> $(mkRelFile "Setup.lhs")
