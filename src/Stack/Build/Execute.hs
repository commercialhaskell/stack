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

import           Control.Concurrent             (forkIO, getNumCapabilities)
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
import           Data.Text.Encoding             (encodeUtf8)
import           Distribution.System            (OS (Windows),
                                                 Platform (Platform))
import           Network.HTTP.Client.Conduit    (HasHttpManager)
import           Path
import           Prelude                        hiding (FilePath, writeFile)
import           Stack.Build.Cache
import           Stack.Build.Installed
import           Stack.Build.Types
import           Stack.Constants
import           Stack.Fetch                    as Fetch
import           Stack.GhcPkg
import           Stack.Package
import           Stack.Types
import           Stack.Types.Internal
import           System.Directory               hiding (findExecutable,
                                                 findFiles)
import           System.Exit                    (ExitCode (ExitSuccess))
import           System.IO
import           System.IO.Temp                 (withSystemTempDirectory)
import           System.Process.Internals       (createProcess_)
import           System.Process.Read

type M env m = (MonadIO m,MonadReader env m,HasHttpManager env,HasBuildConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m,HasLogLevel env)

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
        TTLocal lp steps -> concat
            [ toFilePath $ lpDir lp
            , case steps of
                AllSteps -> " (configure)"
                SkipConfig -> " (build)"
                JustFinal -> " (already built)"
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
    , eeCabalPkgVer    :: !PackageIdentifier
    , eeTotalWanted    :: !Int
    }

-- | Perform the actual plan
executePlan :: M env m
            => EnvOverride
            -> BuildOpts
            -> BaseConfigOpts
            -> PackageIdentifier -- ^ cabal version
            -> [LocalPackage]
            -> Plan
            -> m ()
executePlan menv bopts baseConfigOpts cabalPkgVer locals plan =
    withSystemTempDirectory stackProgName $ \tmpdir -> do
        tmpdir' <- parseAbsDir tmpdir
        configLock <- newMVar ()
        installLock <- newMVar ()
        idMap <- liftIO $ newTVarIO M.empty
        let setupHs = tmpdir' </> $(mkRelFile "Setup.hs")
        liftIO $ writeFile (toFilePath setupHs) "import Distribution.Simple\nmain = defaultMain"
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
    threads <- liftIO getNumCapabilities -- TODO make a build opt to override this
    liftIO $ runActions threads actions

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
    mconfigOpts <- if needsConfig
        then withMVar eeConfigureLock $ \_ -> do
            deleteCaches pkgDir
            idMap <- liftIO $ readTVarIO eeGhcPkgIds
            let getMissing ident =
                    case Map.lookup ident idMap of
                        Nothing -> error "singleBuild: invariant violated, missing package ID missing"
                        Just (Library x) -> Just x
                        Just Executable -> Nothing
                TaskConfigOpts missing mkOpts = taskConfigOpts
                configOpts = mkOpts
                           $ Set.fromList
                           $ mapMaybe getMissing
                           $ Set.toList missing
            announce "configure"
            cabal False $ "configure" : map T.unpack configOpts
            $logDebug $ T.pack $ show configOpts
            writeConfigCache pkgDir configOpts
            return $ Just configOpts
        else return Nothing

    fileModTimes <- getPackageFileModTimes package cabalfp
    writeBuildCache pkgDir fileModTimes

    unless justFinal $ do
        announce "build"
        config <- asks getConfig
        cabal (console && configHideTHLoading config) ["build"]

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

    unless justFinal $ withMVar eeInstallLock $ \_ -> do
        announce "install"
        cabal False ["install"]

    -- It seems correct to leave this outside of the "justFinal" check above,
    -- in case another package depends on a justFinal target
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
            markExeInstalled (taskLocation task) taskProvides
            return Executable
        (True, Nothing) -> throwM $ Couldn'tFindPkgId $ packageName package
        (True, Just pkgid) -> do
            case mconfigOpts of
                Nothing -> return ()
                Just configOpts -> writeFlagCache pkgid $ map encodeUtf8 configOpts
            return $ Library pkgid
    liftIO $ atomically $ modifyTVar eeGhcPkgIds $ Map.insert taskProvides mpkgid'
  where
    announce x = $logInfo $ T.concat
        [ T.pack $ packageIdentifierString taskProvides
        , ": "
        , x
        ]

    needsConfig =
        case taskType of
            TTLocal _ y -> y == AllSteps
            TTUpstream _ _ -> True
    justFinal =
        case taskType of
            TTLocal _ JustFinal -> True
            _ -> False

    wanted =
        case taskType of
            TTLocal lp _ -> lpWanted lp
            TTUpstream _ _ -> False

    console = wanted && acRemaining == 0 && eeTotalWanted == 1

    withPackage inner =
        case taskType of
            TTLocal lp _ -> inner (lpPackage lp) (lpCabalFile lp) (lpDir lp)
            TTUpstream package _ -> do
                mdist <- liftM Just $ distRelativeDir eeCabalPkgVer
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
        distRelativeDir' <- distRelativeDir eeCabalPkgVer
        msetuphs <- liftIO $ getSetupHs pkgDir
        let setuphs = fromMaybe eeSetupHs msetuphs
        inner $ \stripTHLoading args -> do
            let fullArgs =
                      ("-package=" ++ packageIdentifierString eeCabalPkgVer)
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
                        if stripTHLoading
                            then CreatePipe
                            else case mlogFile of
                                Nothing -> Inherit
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
                Just outH -> assert stripTHLoading $ printWithoutTHLoading outH
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

    runTests package pkgDir mlogFile = do
        bconfig <- asks getBuildConfig
        distRelativeDir' <- distRelativeDir eeCabalPkgVer
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
        unless (Map.null errs) $ throwM $ TestSuiteFailure2 taskProvides errs (fmap fst mlogFile)

-- | Grab all output from the given @Handle@ and print it to stdout, stripping
-- Template Haskell "Loading package" lines. Does work in a separate thread.
printWithoutTHLoading :: MonadIO m => Handle -> m ()
printWithoutTHLoading outH = liftIO $ void $ forkIO $
       CB.sourceHandle outH
    $$ CB.lines
    =$ CL.filter (not . isTHLoading)
    =$ CL.mapM_ S8.putStrLn
  where
    -- | Is this line a Template Haskell "Loading package" line
    -- ByteString
    isTHLoading :: S8.ByteString -> Bool
    isTHLoading bs =
        "Loading package " `S8.isPrefixOf` bs &&
        ("done." `S8.isSuffixOf` bs || "done.\r" `S8.isSuffixOf` bs)

taskLocation :: Task -> Location
taskLocation task =
    case taskType task of
        TTLocal _ _ -> Local
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
