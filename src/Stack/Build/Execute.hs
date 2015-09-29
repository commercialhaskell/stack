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
import           Control.Arrow ((&&&))
import           Control.Concurrent.Execute
import           Control.Concurrent.Async       (withAsync, wait)
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.STM
import           Control.Exception.Enclosed     (catchIO, tryIO)
import           Control.Exception.Lifted
import           Control.Monad                  (liftM, when, unless, void, join, guard, filterM, (<=<))
import           Control.Monad.Catch            (MonadCatch, MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader           (MonadReader, asks)
import           Control.Monad.Trans.Control    (liftBaseWith)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString                as S
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as S8
import           Data.Conduit
import qualified Data.Conduit.Binary            as CB
import qualified Data.Conduit.List              as CL
import           Data.Foldable                  (forM_)
import           Data.Function
import           Data.IORef.RunOnce             (runOnce)
import           Data.List
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Maybe
import           Data.Monoid                    ((<>))
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Streaming.Process         hiding (callProcess, env)
import qualified Data.Streaming.Process         as Process
import           Data.Traversable               (forM)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time.Clock                (getCurrentTime)
import           Data.Word8                     (_colon)
import           Distribution.System            (OS (Windows),
                                                 Platform (Platform))
import qualified Distribution.Text
import           Language.Haskell.TH            as TH (location)
import           Network.HTTP.Client.Conduit    (HasHttpManager)
import           Path
import           Path.IO
import           Prelude                        hiding (FilePath, writeFile)
import           Stack.Build.Cache
import           Stack.Build.Coverage
import           Stack.Build.Haddock
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Types.Build
import           Stack.Fetch                    as Fetch
import           Stack.GhcPkg
import           Stack.Package
import           Stack.PackageDump
import           Stack.Constants
import           Stack.Types
import           Stack.Types.StackT
import           Stack.Types.Internal
import qualified System.Directory               as D
import           System.Environment             (getExecutablePath)
import           System.Exit                    (ExitCode (ExitSuccess))
import qualified System.FilePath                as FP
import           System.IO
import           System.PosixCompat.Files       (createLink)
import           System.Process.Read
import           System.Process.Run
import           System.Process.Log             (showProcessArgDebug)

#if !MIN_VERSION_process(1,2,1)
import           System.Process.Internals       (createProcess_)
#endif

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

    let hasTests = not . Set.null . lptbTests
        hasBenches = not . Set.null . lptbBenches
        tests = Map.elems $ fmap fst $ Map.filter (hasTests . snd) $ planFinals plan
        benches = Map.elems $ fmap fst $ Map.filter (hasBenches . snd) $ planFinals plan

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
    , eeSourceMap      :: !SourceMap
    , eeGlobalDB       :: !(Path Abs Dir)
    , eeGlobalPackages :: ![DumpPackage () ()]
    }

-- | Get a compiled Setup exe
getSetupExe :: M env m
            => Path Abs File -- ^ Setup.hs input file
            -> Path Abs Dir -- ^ temporary directory
            -> m (Maybe (Path Abs File))
getSetupExe setupHs tmpdir = do
    wc <- getWhichCompiler
    econfig <- asks getEnvConfig
    let config = getConfig econfig
        baseNameS = concat
            [ "setup-Simple-Cabal-"
            , versionString $ envConfigCabalVersion econfig
            , "-"
            , Distribution.Text.display $ configPlatform config
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
            $(mkRelDir "setup-exe-cache")

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
                    ] ++
                    ["-build-runner" | wc == Ghcjs]
            runIn tmpdir (compilerExeName wc) menv args Nothing
            when (wc == Ghcjs) $ renameDir tmpJsExePath jsExePath
            renameFile tmpExePath exePath
            return $ Just exePath

withExecuteEnv :: M env m
               => EnvOverride
               -> BuildOpts
               -> BaseConfigOpts
               -> [LocalPackage]
               -> [DumpPackage () ()] -- ^ global packages
               -> SourceMap
               -> (ExecuteEnv -> m a)
               -> m a
withExecuteEnv menv bopts baseConfigOpts locals globals sourceMap inner = do
    withCanonicalizedSystemTempDirectory stackProgName $ \tmpdir -> do
        configLock <- newMVar ()
        installLock <- newMVar ()
        idMap <- liftIO $ newTVarIO Map.empty
        let setupHs = tmpdir </> $(mkRelFile "Setup.hs")
        liftIO $ writeFile (toFilePath setupHs) "import Distribution.Simple\nmain = defaultMain"
        setupExe <- getSetupExe setupHs tmpdir
        cabalPkgVer <- asks (envConfigCabalVersion . getEnvConfig)
        globalDB <- getGlobalDB menv =<< getWhichCompiler
        inner ExecuteEnv
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
            , eeTempDir = tmpdir
            , eeSetupHs = setupHs
            , eeSetupExe = setupExe
            , eeCabalPkgVer = cabalPkgVer
            , eeTotalWanted = length $ filter lpWanted locals
            , eeWanted = wantedLocalPackages locals
            , eeLocals = locals
            , eeSourceMap = sourceMap
            , eeGlobalDB = globalDB
            , eeGlobalPackages = globals
            }

-- | Perform the actual plan
executePlan :: M env m
            => EnvOverride
            -> BuildOpts
            -> BaseConfigOpts
            -> [LocalPackage]
            -> [DumpPackage () ()] -- ^ globals
            -> SourceMap
            -> InstalledMap
            -> Plan
            -> m ()
executePlan menv bopts baseConfigOpts locals globals sourceMap installedMap plan = do
    withExecuteEnv menv bopts baseConfigOpts locals globals sourceMap (executePlan' installedMap plan)

    unless (Map.null $ planInstallExes plan) $ do
        snapBin <- (</> bindirSuffix) `liftM` installationRootDeps
        localBin <- (</> bindirSuffix) `liftM` installationRootLocal
        destDir <- asks $ configLocalBin . getConfig
        createTree destDir

        destDir' <- liftIO . D.canonicalizePath . toFilePath $ destDir
        isInPATH <- liftIO . fmap (any (FP.equalFilePath destDir')) . (mapM D.canonicalizePath <=< filterM D.doesDirectoryExist) $ (envSearchPath menv)
        when (not isInPATH) $
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
                        _ -> D.copyFile (toFilePath file) destFile
                    return $ Just (destDir', [T.append name (T.pack ext)])

        let destToInstalled = Map.fromListWith (++) (catMaybes installed)
        unless (Map.null destToInstalled) $ $logInfo ""
        forM_ (Map.toList destToInstalled) $ \(dest, executables) -> do
            $logInfo $ T.concat
                [ "Copied executables to "
                , T.pack dest
                , ":"]
            forM_ executables $ \exe -> $logInfo $ T.append "- " exe

    config <- asks getConfig
    menv' <- liftIO $ configEnvOverride config EnvSettings
                    { esIncludeLocals = True
                    , esIncludeGhcPackagePath = True
                    , esStackExe = True
                    , esLocaleUtf8 = False
                    }
    forM_ (boptsExec bopts) $ \(cmd, args) -> do
        $logProcessRun cmd args
        callProcess Nothing menv' cmd args

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
             -> Plan
             -> ExecuteEnv
             -> m ()
executePlan' installedMap plan ee@ExecuteEnv {..} = do
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
    when (boptsHaddock eeBuildOpts) $ do
        generateLocalHaddockIndex eeEnvOverride wc eeBaseConfigOpts eeLocals
        generateDepsHaddockIndex eeEnvOverride wc eeBaseConfigOpts eeLocals
        generateSnapHaddockIndex eeEnvOverride wc eeBaseConfigOpts eeGlobalDB
    when (toCoverage $ boptsTestOpts eeBuildOpts) generateHpcMarkupIndex
  where
    installedMap' = Map.difference installedMap
                  $ Map.fromList
                  $ map (\(ident, _) -> (packageIdentifierName ident, ()))
                  $ Map.elems
                  $ planUnregisterLocal plan

toActions :: M env m
          => InstalledMap
          -> (m () -> IO ())
          -> ExecuteEnv
          -> (Maybe Task, Maybe (Task, LocalPackageTB)) -- build and final
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
                    , actionDo = \ac -> runInBase $ singleBuild runInBase ac ee task installedMap
                    }
                ]
    afinal =
        case mfinal of
            Nothing -> []
            Just (task@Task {..}, lptb) ->
                [ Action
                    { actionId = ActionId taskProvides ATFinal
                    , actionDeps = addBuild taskProvides $
                        (Set.map (\ident -> ActionId ident ATBuild) (tcoMissing taskConfigOpts))
                    , actionDo = \ac -> runInBase $ do
                        unless (Set.null $ lptbTests lptb) $ do
                            singleTest runInBase topts lptb ac ee task installedMap
                        unless (Set.null $ lptbBenches lptb) $ do
                            singleBench runInBase beopts lptb ac ee task installedMap
                    }
                ]
      where
        addBuild ident =
            case mbuild of
                Nothing -> id
                Just _ -> Set.insert $ ActionId ident ATBuild

    bopts = eeBuildOpts ee
    topts = boptsTestOpts bopts
    beopts = boptsBenchmarkOpts bopts

-- | Generate the ConfigCache
getConfigCache :: MonadIO m
               => ExecuteEnv -> Task -> [Text]
               -> m (Map PackageIdentifier GhcPkgId, ConfigCache)
getConfigCache ExecuteEnv {..} Task {..} extra = do
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
                -- Determine the old and new configuration in the local directory, to
                -- determine if we need to reconfigure.
                mOldConfigCache <- tryGetConfigCache pkgDir

                mOldCabalMod <- tryGetCabalMod pkgDir

                return $ mOldConfigCache /= Just newConfigCache
                      || mOldCabalMod /= Just newCabalMod
    let ConfigureOpts dirs nodirs = configCacheOpts newConfigCache
    when needConfig $ withMVar eeConfigureLock $ \_ -> do
        deleteCaches pkgDir
        announce
        menv <- getMinimalEnvOverride
        exes <- forM (words "ghc ghcjs") $ \name -> do
            mpath <- findExecutable menv name
            return $ case mpath of
                Nothing -> []
                Just x -> return $ concat ["--with-", name, "=", toFilePath x]
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
            createTree (parent logPath)
            let fp = toFilePath logPath
            bracket
                (liftIO $ openBinaryFile fp WriteMode)
                (liftIO . hClose)
                $ \h -> inner (Just (logPath, h))

    withCabal package pkgDir mlogFile inner = do
        config <- asks getConfig
        menv <- liftIO $ configEnvOverride config EnvSettings
            { esIncludeLocals = taskLocation task == Local
            , esIncludeGhcPackagePath = False
            , esStackExe = False
            , esLocaleUtf8 = True
            }
        getRunhaskellPath <- runOnce $ liftIO $ join $ findExecutable menv "runhaskell"
        getGhcjsPath <- runOnce $ liftIO $ join $ findExecutable menv "ghcjs"
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
                        Just deps ->
                            -- Stack always builds with the global Cabal for various
                            -- reproducibility issues.
                            let depsMinusCabal
                                 = map ghcPkgIdString
                                 $ Set.toList
                                 $ addGlobalPackages deps eeGlobalPackages
                            in
                              "-clear-package-db"
                            : "-global-package-db"
                            : ("-package-db=" ++ toFilePath (bcoSnapDB eeBaseConfigOpts))
                            : ("-package-db=" ++ toFilePath (bcoLocalDB eeBaseConfigOpts))
                            : "-hide-all-packages"
                            : cabalPackageArg
                            : map ("-package-id=" ++) depsMinusCabal
                        -- This branch is debatable. It adds access to the
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
                        -- Currently, this branch is only taken via `stack sdist`.
                        Nothing ->
                            [ cabalPackageArg
                            , "-clear-package-db"
                            , "-global-package-db"
                            , "-package-db=" ++ toFilePath (bcoSnapDB eeBaseConfigOpts)
                            ]

                setupArgs = ("--builddir=" ++ toFilePath distRelativeDir') : args
                runExe exeName fullArgs = do
                    $logProcessRun (toFilePath exeName) fullArgs

                    -- Use createProcess_ to avoid the log file being closed afterwards
                    (Nothing, moutH, merrH, ph) <- liftIO $ createProcess_ "singleBuild" cp

                    let makeAbsolute = stripTHLoading -- If users want control, we should add a config option for this

                    ec <-
                        liftIO $
                        withAsync (runInBase $ maybePrintBuildOutput stripTHLoading makeAbsolute LevelInfo mlogFile moutH) $ \outThreadID ->
                        withAsync (runInBase $ maybePrintBuildOutput False makeAbsolute LevelWarn mlogFile merrH) $ \errThreadID -> do
                            ec <- waitForProcess ph
                            wait errThreadID
                            wait outThreadID
                            return ec
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
                  where
                    cp0 = proc (toFilePath exeName) fullArgs
                    cp = cp0
                        { cwd = Just $ toFilePath pkgDir
                        , Process.env = envHelper menv
                        -- Ideally we'd create a new pipe here and then close it
                        -- below to avoid the child process from taking from our
                        -- stdin. However, if we do this, the child process won't
                        -- be able to get the codepage on Windows that we want.
                        -- See:
                        -- https://github.com/commercialhaskell/stack/issues/738
                        -- , std_in = CreatePipe
                        , std_out =
                            case mlogFile of
                                    Nothing -> CreatePipe
                                    Just (_, h) -> UseHandle h
                        , std_err =
                            case mlogFile of
                                Nothing -> CreatePipe
                                Just (_, h) -> UseHandle h
                        }

            wc <- getWhichCompiler
            (exeName, fullArgs) <- case (esetupexehs, wc) of
                (Left setupExe, _) -> return (setupExe, setupArgs)
                (Right setuphs, Ghc) -> do
                    exeName <- getRunhaskellPath
                    let fullArgs = packageArgs ++ (toFilePath setuphs : setupArgs)
                    return (exeName, fullArgs)
                (Right setuphs, Ghcjs) -> do
                    distDir <- distDirFromDir pkgDir
                    let setupDir = distDir </> $(mkRelDir "setup")
                        outputFile = setupDir </> $(mkRelFile "setup")
                    createTree setupDir
                    ghcjsPath <- getGhcjsPath
                    runExe ghcjsPath $
                        [ "--make"
                        , "-odir", toFilePath setupDir
                        , "-hidir", toFilePath setupDir
                        , "-i", "-i."
                        ] ++ packageArgs ++
                        [ toFilePath setuphs
                        , "-o", toFilePath outputFile
                        , "-build-runner"
                        ]
                    return (outputFile, setupArgs)
            runExe exeName $ (if boptsCabalVerbose eeBuildOpts then ("--verbose":) else id) fullArgs

    maybePrintBuildOutput stripTHLoading makeAbsolute level mlogFile mh =
        case mh of
            Just h ->
                case mlogFile of
                  Just{} -> return ()
                  Nothing -> printBuildOutput stripTHLoading makeAbsolute level h
            Nothing -> return ()

singleBuild :: M env m
            => (m () -> IO ())
            -> ActionContext
            -> ExecuteEnv
            -> Task
            -> InstalledMap
            -> m ()
singleBuild runInBase ac@ActionContext {..} ee@ExecuteEnv {..} task@Task {..} installedMap = do
    (allDepsMap, cache) <- getCache
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
                        -- Works around haddock failing on bytestring-builder since it has no modules
                        -- when bytestring is new enough.
                        packageHasExposedModules package

    getCache = do
        let extra =
              -- We enable tests if the test suite dependencies are already
              -- installed, so that we avoid unnecessary recompilation based on
              -- cabal_macros.h changes when switching between 'stack build' and
              -- 'stack test'. See:
              -- https://github.com/commercialhaskell/stack/issues/805
              case taskType of
                  TTLocal lp -> concat
                      [ ["--enable-tests" | depsPresent installedMap $ lpTestDeps lp]
                      , ["--enable-benchmarks" | depsPresent installedMap $ lpBenchDeps lp]
                      ]
                  _ -> []
        getConfigCache ee task extra

    getPrecompiled cache =
        case taskLocation task of
            Snap | not shouldHaddockPackage' -> do
                mpc <- readPrecompiledCache taskProvides $ configCacheOpts cache
                case mpc of
                    Nothing -> return Nothing
                    Just pc -> do
                        let allM _ [] = return True
                            allM f (x:xs) = do
                                b <- f x
                                if b then allM f xs else return False
                        b <- liftIO $ allM D.doesFileExist $ maybe id (:) (pcLibrary pc) $ pcExes pc
                        return $ if b then Just pc else Nothing
            _ -> return Nothing

    copyPreCompiled (PrecompiledCache mlib exes) = do
        announceTask task "copying precompiled package"
        forM_ mlib $ \libpath -> do
            menv <- getMinimalEnvOverride
            withMVar eeInstallLock $ \() ->
                readProcessNull Nothing menv "ghc-pkg"
                    [ "register"
                    , "--no-user-package-db"
                    , "--package-db=" ++ toFilePath (bcoSnapDB eeBaseConfigOpts)
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
        wc <- getWhichCompiler
        let pkgDbs = [bcoSnapDB eeBaseConfigOpts]
        mpkgid <- findGhcPkgId eeEnvOverride wc pkgDbs pname

        return $ Just $
            case mpkgid of
                Nothing -> Executable taskProvides
                Just pkgid -> Library taskProvides pkgid
      where
        bindir = toFilePath $ bcoSnapInstallRoot eeBaseConfigOpts </> bindirSuffix

    realConfigAndBuild cache allDepsMap = withSingleContext runInBase ac ee task (Just allDepsMap) Nothing
        $ \package cabalfp pkgDir cabal announce console _mlogFile -> do
            _neededConfig <- ensureConfig cache pkgDir ee (announce "configure") cabal cabalfp

            if boptsOnlyConfigure eeBuildOpts
                then return Nothing
                else liftM Just $ realBuild cache package pkgDir cabal announce console

    realBuild cache package pkgDir cabal announce console = do
        wc <- getWhichCompiler

        markExeNotInstalled (taskLocation task) taskProvides
        case taskType of
            TTLocal lp -> writeBuildCache pkgDir $ lpNewBuildCache lp
            TTUpstream _ _ -> return ()

        () <- announce "build"
        config <- asks getConfig
        extraOpts <- extraBuildOptions eeBuildOpts
        preBuildTime <- modTime <$> liftIO getCurrentTime
        cabal (console && configHideTHLoading config) $
            (case taskType of
                TTLocal lp -> concat
                    [ ["build"]
                    , ["lib:" ++ packageNameString (packageName package)
                      -- TODO: get this information from target parsing instead,
                      -- which will allow users to turn off library building if
                      -- desired
                      | packageHasLibrary package]
                    , map (T.unpack . T.append "exe:") $ Set.toList $
                        case lpExeComponents lp of
                            Just exes -> exes
                            -- Build all executables in the event that no
                            -- specific list is provided (as happens with
                            -- extra-deps).
                            Nothing -> packageExes package
                    ]
                TTUpstream _ _ -> ["build"]) ++ extraOpts

        case taskType of
            TTLocal lp -> do
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
            TTUpstream _ _ -> return ()

        when (doHaddock package) $ do
            announce "haddock"
            hscolourExists <- doesExecutableExist eeEnvOverride "HsColour"
            unless hscolourExists $ $logWarn
                ("Warning: haddock not generating hyperlinked sources because 'HsColour' not\n" <>
                 "found on PATH (use 'stack install hscolour' to install).")
            cabal False (concat [["haddock", "--html", "--hoogle", "--html-location=../$pkg-$version/"]
                                ,["--hyperlink-source" | hscolourExists]
                                ,["--ghcjs" | wc == Ghcjs]])

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
        mpkgid <- findGhcPkgId eeEnvOverride wc pkgDbs (packageName package)
        let ident = PackageIdentifier (packageName package) (packageVersion package)
        mpkgid' <- case (packageHasLibrary package, mpkgid) of
            (False, _) -> assert (isNothing mpkgid) $ do
                markExeInstalled (taskLocation task) taskProvides -- TODO unify somehow with writeFlagCache?
                return $ Executable ident
            (True, Nothing) -> throwM $ Couldn'tFindPkgId $ packageName package
            (True, Just pkgid) -> return $ Library ident pkgid

        when (doHaddock package && shouldHaddockDeps eeBuildOpts) $
            withMVar eeInstallLock $ \() ->
                copyDepHaddocks
                    eeEnvOverride
                    wc
                    eeBaseConfigOpts
                    (pkgDbs ++ [eeGlobalDB])
                    (PackageIdentifier (packageName package) (packageVersion package))
                    Set.empty

        case taskLocation task of
            Snap -> writePrecompiledCache eeBaseConfigOpts taskProvides (configCacheOpts cache) mpkgid (packageExes package)
            Local -> return ()

        return mpkgid'

-- | Determine if all of the dependencies given are installed
depsPresent :: InstalledMap -> Map PackageName VersionRange -> Bool
depsPresent installedMap deps = all
    (\(name, range) ->
        case Map.lookup name installedMap of
            Just (version, _, _) -> version `withinRange` range
            Nothing -> False)
    (Map.toList deps)

singleTest :: M env m
           => (m () -> IO ())
           -> TestOpts
           -> LocalPackageTB
           -> ActionContext
           -> ExecuteEnv
           -> Task
           -> InstalledMap
           -> m ()
singleTest runInBase topts lptb ac ee task installedMap = do
    (allDepsMap, cache) <- getConfigCache ee task $
        case taskType task of
            TTLocal lp -> concat
                [ ["--enable-tests"]
                , ["--enable-benchmarks" | depsPresent installedMap $ lpBenchDeps lp]
                ]
            _ -> []
    withSingleContext runInBase ac ee task (Just allDepsMap) (Just "test") $ \package cabalfp pkgDir cabal announce console mlogFile -> do
        neededConfig <- ensureConfig cache pkgDir ee (announce "configure (test)") cabal cabalfp
        config <- asks getConfig

        testBuilt <- checkTestBuilt pkgDir

        let needBuild = neededConfig ||
                (case taskType task of
                    TTLocal lp -> lpDirtyFiles lp
                    _ -> assert False True) ||
                not testBuilt

            needHpc = toCoverage topts

            testsToRun = Set.toList $ lptbTests lptb
            components = map (T.unpack . T.append "test:") testsToRun

        when needBuild $ do
            announce "build (test)"
            unsetTestBuilt pkgDir
            unsetTestSuccess pkgDir
            case taskType task of
                TTLocal lp -> writeBuildCache pkgDir $ lpNewBuildCache lp
                TTUpstream _ _ -> assert False $ return ()
            extraOpts <- extraBuildOptions (eeBuildOpts ee)
            cabal (console && configHideTHLoading config) $
                "build" : (components ++ extraOpts)
            setTestBuilt pkgDir

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
            bconfig <- asks getBuildConfig
            buildDir <- distDirFromDir pkgDir
            hpcDir <- hpcDirFromDir pkgDir
            when needHpc (createTree hpcDir)
            let exeExtension =
                    case configPlatform $ getConfig bconfig of
                        Platform _ Windows -> ".exe"
                        _ -> ""

            errs <- liftM Map.unions $ forM testsToRun $ \testName -> do
                nameDir <- parseRelDir $ T.unpack testName
                nameExe <- parseRelFile $ T.unpack testName ++ exeExtension
                nameTix <- liftM (pkgDir </>) $ parseRelFile $ T.unpack testName ++ ".tix"
                let exeName = buildDir </> $(mkRelDir "build") </> nameDir </> nameExe
                exists <- fileExists exeName
                menv <- liftIO $ configEnvOverride config EnvSettings
                    { esIncludeLocals = taskLocation task == Local
                    , esIncludeGhcPackagePath = True
                    , esStackExe = True
                    , esLocaleUtf8 = False
                    }
                if exists
                    then do
                        -- We clear out the .tix files before doing a run.
                        when needHpc $ do
                            tixexists <- fileExists nameTix
                            when tixexists $
                                $logWarn ("Removing HPC file " <> T.pack (toFilePath nameTix))
                            removeFileIfExists nameTix

                        let args = toAdditionalArgs topts
                            argsDisplay = case args of
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
                        -- Move the .tix file out of the package directory
                        -- into the hpc work dir, for tidiness.
                        when needHpc $
                            moveFileIfExists nameTix hpcDir
                        return $ case ec of
                            ExitSuccess -> Map.empty
                            _ -> Map.singleton testName $ Just ec
                    else do
                        $logError $ T.concat
                            [ "Test suite "
                            , testName
                            , " executable not found for "
                            , packageNameText $ packageName package
                            ]
                        return $ Map.singleton testName Nothing

            when needHpc $ do
                wc <- getWhichCompiler
                let pkgDbs =
                        [ bcoSnapDB (eeBaseConfigOpts ee)
                        , bcoLocalDB (eeBaseConfigOpts ee)
                        ]
                generateHpcReport pkgDir package testsToRun (findGhcPkgKey (eeEnvOverride ee) wc pkgDbs)

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

            setTestSuccess pkgDir

singleBench :: M env m
            => (m () -> IO ())
            -> BenchmarkOpts
            -> LocalPackageTB
            -> ActionContext
            -> ExecuteEnv
            -> Task
            -> InstalledMap
            -> m ()
singleBench runInBase beopts _lptb ac ee task installedMap = do
    (allDepsMap, cache) <- getConfigCache ee task $
        case taskType task of
            TTLocal lp -> concat
                [ ["--enable-tests" | depsPresent installedMap $ lpTestDeps lp]
                , ["--enable-benchmarks"]
                ]
            _ -> []
    withSingleContext runInBase ac ee task (Just allDepsMap) (Just "bench") $ \_package cabalfp pkgDir cabal announce console _mlogFile -> do
        neededConfig <- ensureConfig cache pkgDir ee (announce "configure (benchmarks)") cabal cabalfp

        benchBuilt <- checkBenchBuilt pkgDir

        let needBuild = neededConfig ||
                (case taskType task of
                    TTLocal lp -> lpDirtyFiles lp
                    _ -> assert False True) ||
                not benchBuilt
        when needBuild $ do
            announce "build (benchmarks)"
            unsetBenchBuilt pkgDir
            case taskType task of
                TTLocal lp -> writeBuildCache pkgDir $ lpNewBuildCache lp
                TTUpstream _ _ -> assert False $ return ()
            config <- asks getConfig
            extraOpts <- extraBuildOptions (eeBuildOpts ee)
            cabal (console && configHideTHLoading config) ("build" : extraOpts)
            setBenchBuilt pkgDir
        let args = maybe []
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

-- | Grab all output from the given @Handle@ and print it to stdout, stripping
-- Template Haskell "Loading package" lines. Does work in a separate thread.
printBuildOutput :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
                 => Bool -- ^ exclude TH loading?
                 -> Bool -- ^ convert paths to absolute?
                 -> LogLevel
                 -> Handle -> m ()
printBuildOutput excludeTHLoading makeAbsolute level outH = void $
         CB.sourceHandle outH
    $$ CB.lines
    =$ CL.map stripCarriageReturn
    =$ CL.filter (not . isTHLoading)
    =$ CL.mapM toAbsolutePath
    =$ CL.mapM_ (monadLoggerLog $(TH.location >>= liftLoc) "" level)
  where
    -- | Is this line a Template Haskell "Loading package" line
    -- ByteString
    isTHLoading :: S8.ByteString -> Bool
    isTHLoading _ | not excludeTHLoading = False
    isTHLoading bs =
        "Loading package " `S8.isPrefixOf` bs &&
        ("done." `S8.isSuffixOf` bs || "done.\r" `S8.isSuffixOf` bs)

    -- | Convert GHC error lines with file paths to have absolute file paths
    toAbsolutePath bs | not makeAbsolute = return bs
    toAbsolutePath bs = do
        let (x, y) = S.break (== _colon) bs
        mabs <-
            if isValidSuffix y
                then do
                    efp <- liftIO $ tryIO $ D.canonicalizePath $ S8.unpack x
                    case efp of
                        Left _ -> return Nothing
                        Right fp -> return $ Just $ S8.pack fp
                else return Nothing
        case mabs of
            Nothing -> return bs
            Just fp -> return $ fp `S.append` y

    -- | Match the line:column format at the end of lines
    isValidSuffix bs0 = maybe False (const True) $ do
        guard $ not $ S.null bs0
        guard $ S.head bs0 == _colon
        (_, bs1) <- S8.readInt $ S.drop 1 bs0

        guard $ not $ S.null bs1
        guard $ S.head bs1 == _colon
        (_, bs2) <- S8.readInt $ S.drop 1 bs1

        guard $ bs2 == ":"

    -- | Strip @\r@ characters from the byte vector. Used because Windows.
    stripCarriageReturn :: ByteString -> ByteString
    stripCarriageReturn = S8.filter (not . (=='\r'))

-- | Find the Setup.hs or Setup.lhs in the given directory. If none exists,
-- throw an exception.
getSetupHs :: Path Abs Dir -- ^ project directory
           -> IO (Path Abs File)
getSetupHs dir = do
    exists1 <- fileExists fp1
    if exists1
        then return fp1
        else do
            exists2 <- fileExists fp2
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
        hpcIndexDir <- toFilePath . (</> dotHpc) <$> hpcRelativeDir
        return ["--ghc-options", "-hpcdir " ++ hpcIndexDir ++ ddumpOpts]
      False -> return ["--ghc-options", ddumpOpts]

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
