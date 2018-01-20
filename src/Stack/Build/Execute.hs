{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
-- | Perform a build
module Stack.Build.Execute
    ( printPlan
    , preFetch
    , executePlan
    -- * Running Setup.hs
    , ExecuteEnv
    , withExecuteEnv
    , withSingleContext
    , ExcludeTHLoading(..)
    ) where

import           Control.Concurrent.Execute
import           Control.Concurrent.STM (check)
import           Stack.Prelude hiding (Display (..))
import           Crypto.Hash
import           Data.Attoparsec.Text hiding (try)
import qualified Data.ByteArray as Mem (convert)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64.URL as B64URL
import           Data.Char (isSpace)
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process.Typed
                    (ExitCodeException (..), waitExitCode,
                     useHandleOpen, setStdin, setStdout, setStderr, getStdin,
                     createPipe, runProcess_, getStdout,
                     getStderr, createSource)
import qualified Data.Conduit.Text as CT
import           Data.FileEmbed (embedFile, makeRelativeToProject)
import           Data.IORef.RunOnce (runOnce)
import           Data.List hiding (any)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Time.Clock (getCurrentTime)
import           Data.Tuple
import qualified Distribution.PackageDescription as C
import qualified Distribution.Simple.Build.Macros as C
import           Distribution.System            (OS (Windows),
                                                 Platform (Platform))
import qualified Distribution.Text as C
import           Path
import           Path.CheckInstall
import           Path.Extra (toFilePathNoTrailingSep, rejectMissingFile)
import           Path.IO hiding (findExecutable, makeAbsolute, withSystemTempDir)
import qualified RIO
import           Stack.Build.Cache
import           Stack.Build.Haddock
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Build.Target
import           Stack.Config
import           Stack.Constants
import           Stack.Constants.Config
import           Stack.Coverage
import           Stack.Fetch as Fetch
import           Stack.GhcPkg
import           Stack.Package
import           Stack.PackageDump
import           Stack.PrettyPrint
import           Stack.Types.Build
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.GhcPkgId
import           Stack.Types.NamedComponent
import           Stack.Types.Package
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Runner
import           Stack.Types.Version
import qualified System.Directory as D
import           System.Environment (getExecutablePath)
import           System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import           System.IO (hPutStr, stderr, stdout)
import           System.PosixCompat.Files (createLink)
import           RIO.Process

-- | Has an executable been built or not?
data ExecutableBuildStatus
    = ExecutableBuilt
    | ExecutableNotBuilt
  deriving (Show, Eq, Ord)

-- | Fetch the packages necessary for a build, for example in combination with a dry run.
preFetch :: HasEnvConfig env => Plan -> RIO env ()
preFetch plan
    | Set.null idents = logDebug "Nothing to fetch"
    | otherwise = do
        logDebug $
            "Prefetching: " <>
            mconcat (intersperse ", " (RIO.display <$> Set.toList idents))
        fetchPackages idents
  where
    idents = Set.unions $ map toIdent $ Map.elems $ planTasks plan

    toIdent task =
        case taskType task of
            TTFiles{} -> Set.empty
            TTIndex _ _ (PackageIdentifierRevision ident _) -> Set.singleton ident

-- | Print a description of build plan for human consumption.
printPlan :: HasRunner env => Plan -> RIO env ()
printPlan plan = do
    case Map.elems $ planUnregisterLocal plan of
        [] -> logInfo "No packages would be unregistered."
        xs -> do
            logInfo "Would unregister locally:"
            forM_ xs $ \(ident, reason) -> logInfo $
                RIO.display ident <>
                if T.null reason
                  then ""
                  else " (" <> RIO.display reason <> ")"

    logInfo ""

    case Map.elems $ planTasks plan of
        [] -> logInfo "Nothing to build."
        xs -> do
            logInfo "Would build:"
            mapM_ (logInfo . displayTask) xs

    let hasTests = not . Set.null . testComponents . taskComponents
        hasBenches = not . Set.null . benchComponents . taskComponents
        tests = Map.elems $ Map.filter hasTests $ planFinals plan
        benches = Map.elems $ Map.filter hasBenches $ planFinals plan

    unless (null tests) $ do
        logInfo ""
        logInfo "Would test:"
        mapM_ (logInfo . displayTask) tests
    unless (null benches) $ do
        logInfo ""
        logInfo "Would benchmark:"
        mapM_ (logInfo . displayTask) benches

    logInfo ""

    case Map.toList $ planInstallExes plan of
        [] -> logInfo "No executables to be installed."
        xs -> do
            logInfo "Would install executables:"
            forM_ xs $ \(name, loc) -> logInfo $
                RIO.display name <>
                " from " <>
                (case loc of
                   Snap -> "snapshot"
                   Local -> "local") <>
                " database"

-- | For a dry run
displayTask :: Task -> Utf8Builder
displayTask task =
    RIO.display (taskProvides task) <>
    ": database=" <>
    (case taskLocation task of
        Snap -> "snapshot"
        Local -> "local") <>
    ", source=" <>
    (case taskType task of
        TTFiles lp _ -> fromString $ toFilePath $ lpDir lp
        TTIndex{} -> "package index") <>
    (if Set.null missing
        then ""
        else ", after: " <>
             mconcat (intersperse "," (RIO.display <$> Set.toList missing)))
  where
    missing = tcoMissing $ taskConfigOpts task

data ExecuteEnv = ExecuteEnv
    { eeConfigureLock  :: !(MVar ())
    , eeInstallLock    :: !(MVar ())
    , eeBuildOpts      :: !BuildOpts
    , eeBuildOptsCLI   :: !BuildOptsCLI
    , eeBaseConfigOpts :: !BaseConfigOpts
    , eeGhcPkgIds      :: !(TVar (Map PackageIdentifier Installed))
    , eeTempDir        :: !(Path Abs Dir)
    , eeSetupHs        :: !(Path Abs File)
    -- ^ Temporary Setup.hs for simple builds
    , eeSetupShimHs    :: !(Path Abs File)
    -- ^ Temporary SetupShim.hs, to provide access to initial-build-steps
    , eeSetupExe       :: !(Maybe (Path Abs File))
    -- ^ Compiled version of eeSetupHs
    , eeCabalPkgVer    :: !Version
    , eeTotalWanted    :: !Int
    , eeWanted         :: !(Set PackageName)
    , eeLocals         :: ![LocalPackage]
    , eeGlobalDB       :: !(Path Abs Dir)
    , eeGlobalDumpPkgs :: !(Map GhcPkgId (DumpPackage () () ()))
    , eeSnapshotDumpPkgs :: !(TVar (Map GhcPkgId (DumpPackage () () ())))
    , eeLocalDumpPkgs  :: !(TVar (Map GhcPkgId (DumpPackage () () ())))
    , eeLogFiles       :: !(TChan (Path Abs Dir, Path Abs File))
    , eeGetGhcPath     :: !(forall m. MonadIO m => m (Path Abs File))
    , eeGetGhcjsPath   :: !(forall m. MonadIO m => m (Path Abs File))
    , eeCustomBuilt    :: !(IORef (Set PackageName))
    -- ^ Stores which packages with custom-setup have already had their
    -- Setup.hs built.
    }

buildSetupArgs :: [String]
buildSetupArgs =
     [ "-rtsopts"
     , "-threaded"
     , "-clear-package-db"
     , "-global-package-db"
     , "-hide-all-packages"
     , "-package"
     , "base"
     , "-main-is"
     , "StackSetupShim.mainOverride"
     ]

setupGhciShimCode :: S.ByteString
setupGhciShimCode = $(do
    path <- makeRelativeToProject "src/setup-shim/StackSetupShim.hs"
    embedFile path)

simpleSetupCode :: S.ByteString
simpleSetupCode = "import Distribution.Simple\nmain = defaultMain"

simpleSetupHash :: String
simpleSetupHash =
    T.unpack $ decodeUtf8 $ S.take 8 $ B64URL.encode $ Mem.convert $ hashWith SHA256 $
    encodeUtf8 (T.pack (unwords buildSetupArgs)) <> setupGhciShimCode <> simpleSetupCode

-- | Get a compiled Setup exe
getSetupExe :: HasEnvConfig env
            => Path Abs File -- ^ Setup.hs input file
            -> Path Abs File -- ^ SetupShim.hs input file
            -> Path Abs Dir -- ^ temporary directory
            -> RIO env (Maybe (Path Abs File))
getSetupExe setupHs setupShimHs tmpdir = do
    wc <- view $ actualCompilerVersionL.whichCompilerL
    platformDir <- platformGhcRelDir
    config <- view configL
    cabalVersionString <- view $ cabalVersionL.to versionString
    actualCompilerVersionString <- view $ actualCompilerVersionL.to compilerVersionString
    platform <- view platformL
    let baseNameS = concat
            [ "Cabal-simple_"
            , simpleSetupHash
            , "_"
            , cabalVersionString
            , "_"
            , actualCompilerVersionString
            ]
        exeNameS = baseNameS ++
            case platform of
                Platform _ Windows -> ".exe"
                _ -> ""
        outputNameS =
            case wc of
                Ghc -> exeNameS
                Ghcjs -> baseNameS ++ ".jsexe"
        jsExeNameS =
            baseNameS ++ ".jsexe"
        setupDir =
            view stackRootL config </>
            $(mkRelDir "setup-exe-cache") </>
            platformDir

    exePath <- (setupDir </>) <$> parseRelFile exeNameS
    jsExePath <- (setupDir </>) <$> parseRelDir jsExeNameS

    exists <- liftIO $ D.doesFileExist $ toFilePath exePath

    if exists
        then return $ Just exePath
        else do
            tmpExePath <- fmap (setupDir </>) $ parseRelFile $ "tmp-" ++ exeNameS
            tmpOutputPath <- fmap (setupDir </>) $ parseRelFile $ "tmp-" ++ outputNameS
            tmpJsExePath <- fmap (setupDir </>) $ parseRelDir $ "tmp-" ++ jsExeNameS
            ensureDir setupDir
            let args = buildSetupArgs ++
                    [ "-package"
                    , "Cabal-" ++ cabalVersionString
                    , toFilePath setupHs
                    , toFilePath setupShimHs
                    , "-o"
                    , toFilePath tmpOutputPath
                    ] ++
                    ["-build-runner" | wc == Ghcjs]
            withWorkingDir (toFilePath tmpdir) (proc (compilerExeName wc) args $ \pc0 -> do
              let pc = setStdout (useHandleOpen stderr) pc0
              runProcess_ pc)
                `catch` \ece -> do
                    compilerPath <- getCompilerPath wc
                    throwM $ SetupHsBuildFailure (eceExitCode ece) Nothing compilerPath args Nothing []
            when (wc == Ghcjs) $ renameDir tmpJsExePath jsExePath
            renameFile tmpExePath exePath
            return $ Just exePath

-- | Execute a function that takes an 'ExecuteEnv'.
withExecuteEnv :: forall env a. HasEnvConfig env
               => BuildOpts
               -> BuildOptsCLI
               -> BaseConfigOpts
               -> [LocalPackage]
               -> [DumpPackage () () ()] -- ^ global packages
               -> [DumpPackage () () ()] -- ^ snapshot packages
               -> [DumpPackage () () ()] -- ^ local packages
               -> (ExecuteEnv -> RIO env a)
               -> RIO env a
withExecuteEnv bopts boptsCli baseConfigOpts locals globalPackages snapshotPackages localPackages inner =
    createTempDirFunction stackProgName $ \tmpdir -> do
        configLock <- liftIO $ newMVar ()
        installLock <- liftIO $ newMVar ()
        idMap <- liftIO $ newTVarIO Map.empty
        config <- view configL

        getGhcPath <- runOnce $ getCompilerPath Ghc
        getGhcjsPath <- runOnce $ getCompilerPath Ghcjs
        customBuiltRef <- newIORef Set.empty

        -- Create files for simple setup and setup shim, if necessary
        let setupSrcDir =
                view stackRootL config </>
                $(mkRelDir "setup-exe-src")
        ensureDir setupSrcDir
        setupFileName <- parseRelFile ("setup-" ++ simpleSetupHash ++ ".hs")
        let setupHs = setupSrcDir </> setupFileName
        setupHsExists <- doesFileExist setupHs
        unless setupHsExists $ liftIO $ S.writeFile (toFilePath setupHs) simpleSetupCode
        setupShimFileName <- parseRelFile ("setup-shim-" ++ simpleSetupHash ++ ".hs")
        let setupShimHs = setupSrcDir </> setupShimFileName
        setupShimHsExists <- doesFileExist setupShimHs
        unless setupShimHsExists $ liftIO $ S.writeFile (toFilePath setupShimHs) setupGhciShimCode
        setupExe <- getSetupExe setupHs setupShimHs tmpdir

        cabalPkgVer <- view cabalVersionL
        globalDB <- getGlobalDB =<< view (actualCompilerVersionL.whichCompilerL)
        snapshotPackagesTVar <- liftIO $ newTVarIO (toDumpPackagesByGhcPkgId snapshotPackages)
        localPackagesTVar <- liftIO $ newTVarIO (toDumpPackagesByGhcPkgId localPackages)
        logFilesTChan <- liftIO $ atomically newTChan
        let totalWanted = length $ filter lpWanted locals
        env <- ask
        inner ExecuteEnv
            { eeBuildOpts = bopts
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
            , eeSetupShimHs = setupShimHs
            , eeSetupExe = setupExe
            , eeCabalPkgVer = cabalPkgVer
            , eeTotalWanted = totalWanted
            , eeWanted = wantedLocalPackages locals
            , eeLocals = locals
            , eeGlobalDB = globalDB
            , eeGlobalDumpPkgs = toDumpPackagesByGhcPkgId globalPackages
            , eeSnapshotDumpPkgs = snapshotPackagesTVar
            , eeLocalDumpPkgs = localPackagesTVar
            , eeLogFiles = logFilesTChan
            , eeGetGhcPath = runRIO env getGhcPath
            , eeGetGhcjsPath = runRIO env getGhcjsPath
            , eeCustomBuilt = customBuiltRef
            } `finally` dumpLogs logFilesTChan totalWanted
  where
    toDumpPackagesByGhcPkgId = Map.fromList . map (\dp -> (dpGhcPkgId dp, dp))

    createTempDirFunction
        | Just True <- boptsKeepTmpFiles bopts = withKeepSystemTempDir
        | otherwise = withSystemTempDir

    dumpLogs :: TChan (Path Abs Dir, Path Abs File) -> Int -> RIO env ()
    dumpLogs chan totalWanted = do
        allLogs <- fmap reverse $ liftIO $ atomically drainChan
        case allLogs of
            -- No log files generated, nothing to dump
            [] -> return ()
            firstLog:_ -> do
                toDump <- view $ configL.to configDumpLogs
                case toDump of
                    DumpAllLogs -> mapM_ (dumpLog "") allLogs
                    DumpWarningLogs -> mapM_ dumpLogIfWarning allLogs
                    DumpNoLogs
                        | totalWanted > 1 ->
                            logInfo $
                                "Build output has been captured to log files, use " <>
                                "--dump-logs to see it on the console"
                        | otherwise -> return ()
                logInfo $ "Log files have been written to: " <>
                          fromString (toFilePath (parent (snd firstLog)))

        -- We only strip the colors /after/ we've dumped logs, so that
        -- we get pretty colors in our dump output on the terminal.
        colors <- shouldForceGhcColorFlag
        when colors $ liftIO $ mapM_ (stripColors . snd) allLogs
      where
        drainChan :: STM [(Path Abs Dir, Path Abs File)]
        drainChan = do
            mx <- tryReadTChan chan
            case mx of
                Nothing -> return []
                Just x -> do
                    xs <- drainChan
                    return $ x:xs

    dumpLogIfWarning :: (Path Abs Dir, Path Abs File) -> RIO env ()
    dumpLogIfWarning (pkgDir, filepath) = do
      firstWarning <- withSourceFile (toFilePath filepath) $ \src ->
            runConduit
          $ src
         .| CT.decodeUtf8Lenient
         .| CT.lines
         .| CL.map stripCR
         .| CL.filter isWarning
         .| CL.take 1
      unless (null firstWarning) $ dumpLog " due to warnings" (pkgDir, filepath)

    isWarning :: Text -> Bool
    isWarning t = ": Warning:" `T.isSuffixOf` t -- prior to GHC 8
               || ": warning:" `T.isInfixOf` t -- GHC 8 is slightly different

    dumpLog :: String -> (Path Abs Dir, Path Abs File) -> RIO env ()
    dumpLog msgSuffix (pkgDir, filepath) = do
        logInfo $
          "\n--  Dumping log file" <>
          fromString msgSuffix <>
          ": " <>
          fromString (toFilePath filepath) <>
          "\n"
        compilerVer <- view actualCompilerVersionL
        withSourceFile (toFilePath filepath) $ \src ->
              runConduit
            $ src
           .| CT.decodeUtf8Lenient
           .| mungeBuildOutput ExcludeTHLoading ConvertPathsToAbsolute pkgDir compilerVer
           .| CL.mapM_ (logInfo . RIO.display)
        logInfo $ "\n--  End of log file: " <> fromString (toFilePath filepath) <> "\n"

    stripColors :: Path Abs File -> IO ()
    stripColors fp = do
      let colorfp = toFilePath fp ++ "-color"
      withSourceFile (toFilePath fp) $ \src ->
        withSinkFile colorfp $ \sink ->
        runConduit $ src .| sink
      withSourceFile colorfp $ \src ->
        withSinkFile (toFilePath fp) $ \sink ->
        runConduit $ src .| noColors .| sink

      where
        noColors = do
          CB.takeWhile (/= 27) -- ESC
          mnext <- CB.head
          case mnext of
            Nothing -> return ()
            Just x -> assert (x == 27) $ do
              -- Color sequences always end with an m
              CB.dropWhile (/= 109) -- m
              CB.drop 1 -- drop the m itself
              noColors

-- | Perform the actual plan
executePlan :: HasEnvConfig env
            => BuildOptsCLI
            -> BaseConfigOpts
            -> [LocalPackage]
            -> [DumpPackage () () ()] -- ^ global packages
            -> [DumpPackage () () ()] -- ^ snapshot packages
            -> [DumpPackage () () ()] -- ^ local packages
            -> InstalledMap
            -> Map PackageName Target
            -> Plan
            -> RIO env ()
executePlan boptsCli baseConfigOpts locals globalPackages snapshotPackages localPackages installedMap targets plan = do
    logDebug "Executing the build plan"
    bopts <- view buildOptsL
    withExecuteEnv bopts boptsCli baseConfigOpts locals globalPackages snapshotPackages localPackages (executePlan' installedMap targets plan)

    copyExecutables (planInstallExes plan)

    config <- view configL
    menv' <- liftIO $ configProcessContextSettings config EnvSettings
                    { esIncludeLocals = True
                    , esIncludeGhcPackagePath = True
                    , esStackExe = True
                    , esLocaleUtf8 = False
                    , esKeepGhcRts = False
                    }
    withProcessContext menv' $
      forM_ (boptsCLIExec boptsCli) $ \(cmd, args) ->
      proc cmd args runProcess_

copyExecutables
    :: HasEnvConfig env
    => Map Text InstallLocation
    -> RIO env ()
copyExecutables exes | Map.null exes = return ()
copyExecutables exes = do
    snapBin <- (</> bindirSuffix) `liftM` installationRootDeps
    localBin <- (</> bindirSuffix) `liftM` installationRootLocal
    compilerSpecific <- boptsInstallCompilerTool <$> view buildOptsL
    destDir <- if compilerSpecific
                   then bindirCompilerTools
                   else view $ configL.to configLocalBin
    ensureDir destDir

    destDir' <- liftIO . D.canonicalizePath . toFilePath $ destDir

    platform <- view platformL
    let ext =
            case platform of
                Platform _ Windows -> ".exe"
                _ -> ""

    currExe <- liftIO getExecutablePath -- needed for windows, see below

    installed <- forMaybeM (Map.toList exes) $ \(name, loc) -> do
        let bindir =
                case loc of
                    Snap -> snapBin
                    Local -> localBin
        mfp <- liftIO $ forgivingAbsence (resolveFile bindir $ T.unpack name ++ ext)
          >>= rejectMissingFile
        case mfp of
            Nothing -> do
                logWarn $
                    "Couldn't find executable " <>
                    RIO.display name <>
                    " in directory " <>
                    fromString (toFilePath bindir)
                return Nothing
            Just file -> do
                let destFile = destDir' FP.</> T.unpack name ++ ext
                logInfo $
                    "Copying from " <>
                    fromString (toFilePath file) <>
                    " to " <>
                    fromString destFile

                liftIO $ case platform of
                    Platform _ Windows | FP.equalFilePath destFile currExe ->
                        windowsRenameCopy (toFilePath file) destFile
                    _ -> D.copyFile (toFilePath file) destFile
                return $ Just (name <> T.pack ext)

    unless (null installed) $ do
        logInfo ""
        logInfo $
            "Copied executables to " <>
            fromString destDir' <>
            ":"
    forM_ installed $ \exe -> logInfo ("- " <> RIO.display exe)
    unless compilerSpecific $ warnInstallSearchPathIssues destDir' installed


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
executePlan' :: HasEnvConfig env
             => InstalledMap
             -> Map PackageName Target
             -> Plan
             -> ExecuteEnv
             -> RIO env ()
executePlan' installedMap0 targets plan ee@ExecuteEnv {..} = do
    when (toCoverage $ boptsTestOpts eeBuildOpts) deleteHpcReports
    cv <- view actualCompilerVersionL
    let wc = view whichCompilerL cv
    case Map.toList $ planUnregisterLocal plan of
        [] -> return ()
        ids -> do
            localDB <- packageDatabaseLocal
            forM_ ids $ \(id', (ident, reason)) -> do
                logInfo $
                    RIO.display ident <>
                    ": unregistering" <>
                    if T.null reason
                        then ""
                        else " (" <> RIO.display reason <> ")"
                unregisterGhcPkgId wc cv localDB id' ident

    liftIO $ atomically $ modifyTVar' eeLocalDumpPkgs $ \initMap ->
        foldl' (flip Map.delete) initMap $ Map.keys (planUnregisterLocal plan)

    run <- askRunInIO

    -- If running tests concurrently with eachother, then create an MVar
    -- which is empty while each test is being run.
    concurrentTests <- view $ configL.to configConcurrentTests
    mtestLock <- if concurrentTests then return Nothing else Just <$> liftIO (newMVar ())

    let actions = concatMap (toActions installedMap' mtestLock run ee) $ Map.elems $ Map.mergeWithKey
            (\_ b f -> Just (Just b, Just f))
            (fmap (\b -> (Just b, Nothing)))
            (fmap (\f -> (Nothing, Just f)))
            (planTasks plan)
            (planFinals plan)
    threads <- view $ configL.to configJobs
    let keepGoing =
            fromMaybe (not (M.null (planFinals plan))) (boptsKeepGoing eeBuildOpts)
    terminal <- view terminalL
    errs <- liftIO $ runActions threads keepGoing actions $ \doneVar actionsVar -> do
        let total = length actions
            loop prev
                | prev == total =
                    run $ logStickyDone ("Completed " <> RIO.display total <> " action(s).")
                | otherwise = do
                    inProgress <- readTVarIO actionsVar
                    let packageNames = map (\(ActionId pkgID _) -> packageIdentifierText pkgID) (toList inProgress)
                        nowBuilding []    = ""
                        nowBuilding names = mconcat $ ": " : intersperse ", " (map RIO.display names)
                    when terminal $ run $
                        logSticky $
                            "Progress " <> RIO.display prev <> "/" <> RIO.display total <>
                                nowBuilding packageNames
                    done <- atomically $ do
                        done <- readTVar doneVar
                        check $ done /= prev
                        return done
                    loop done
        when (total > 1) $ loop 0
    when (toCoverage $ boptsTestOpts eeBuildOpts) $ do
        generateHpcUnifiedReport
        generateHpcMarkupIndex
    unless (null errs) $ throwM $ ExecutionFailure errs
    when (boptsHaddock eeBuildOpts) $ do
        snapshotDumpPkgs <- liftIO (readTVarIO eeSnapshotDumpPkgs)
        localDumpPkgs <- liftIO (readTVarIO eeLocalDumpPkgs)
        generateLocalHaddockIndex wc eeBaseConfigOpts localDumpPkgs eeLocals
        generateDepsHaddockIndex wc eeBaseConfigOpts eeGlobalDumpPkgs snapshotDumpPkgs localDumpPkgs eeLocals
        generateSnapHaddockIndex wc eeBaseConfigOpts eeGlobalDumpPkgs snapshotDumpPkgs
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

toActions :: HasEnvConfig env
          => InstalledMap
          -> Maybe (MVar ())
          -> (RIO env () -> IO ())
          -> ExecuteEnv
          -> (Maybe Task, Maybe Task) -- build and final
          -> [Action]
toActions installedMap mtestLock runInBase ee (mbuild, mfinal) =
    abuild ++ afinal
  where
    abuild =
        case mbuild of
            Nothing -> []
            Just task@Task {..} ->
                [ Action
                    { actionId = ActionId taskProvides ATBuild
                    , actionDeps =
                        Set.map (\ident -> ActionId ident ATBuild) (tcoMissing taskConfigOpts)
                    , actionDo = \ac -> runInBase $ singleBuild ac ee task installedMap False
                    , actionConcurrency = ConcurrencyAllowed
                    }
                ]
    afinal =
        case mfinal of
            Nothing -> []
            Just task@Task {..} ->
                (if taskAllInOne then id else (:)
                    Action
                        { actionId = ActionId taskProvides ATBuildFinal
                        , actionDeps = addBuild
                            (Set.map (\ident -> ActionId ident ATBuild) (tcoMissing taskConfigOpts))
                        , actionDo = \ac -> runInBase $ singleBuild ac ee task installedMap True
                        , actionConcurrency = ConcurrencyAllowed
                        }) $
                -- These are the "final" actions - running tests and benchmarks.
                (if Set.null tests then id else (:)
                    Action
                        { actionId = ActionId taskProvides ATRunTests
                        , actionDeps = finalDeps
                        , actionDo = \ac -> withLock mtestLock $ runInBase $ do
                            singleTest topts (Set.toList tests) ac ee task installedMap
                        -- Always allow tests tasks to run concurrently with
                        -- other tasks, particularly build tasks. Note that
                        -- 'mtestLock' can optionally make it so that only
                        -- one test is run at a time.
                        , actionConcurrency = ConcurrencyAllowed
                        }) $
                (if Set.null benches then id else (:)
                    Action
                        { actionId = ActionId taskProvides ATRunBenchmarks
                        , actionDeps = finalDeps
                        , actionDo = \ac -> runInBase $ do
                            singleBench beopts (Set.toList benches) ac ee task installedMap
                        -- Never run benchmarks concurrently with any other task, see #3663
                        , actionConcurrency = ConcurrencyDisallowed
                        })
                []
              where
                comps = taskComponents task
                tests = testComponents comps
                benches = benchComponents comps
                finalDeps =
                    if taskAllInOne
                        then addBuild mempty
                        else Set.singleton (ActionId taskProvides ATBuildFinal)
                addBuild =
                    case mbuild of
                        Nothing -> id
                        Just _ -> Set.insert $ ActionId taskProvides ATBuild
    withLock Nothing f = f
    withLock (Just lock) f = withMVar lock $ \() -> f
    bopts = eeBuildOpts ee
    topts = boptsTestOpts bopts
    beopts = boptsBenchmarkOpts bopts

-- | Generate the ConfigCache
getConfigCache :: HasEnvConfig env
               => ExecuteEnv -> Task -> InstalledMap -> Bool -> Bool
               -> RIO env (Map PackageIdentifier GhcPkgId, ConfigCache)
getConfigCache ExecuteEnv {..} task@Task {..} installedMap enableTest enableBench = do
    useExactConf <- view $ configL.to configAllowNewer
    let extra =
            -- We enable tests if the test suite dependencies are already
            -- installed, so that we avoid unnecessary recompilation based on
            -- cabal_macros.h changes when switching between 'stack build' and
            -- 'stack test'. See:
            -- https://github.com/commercialhaskell/stack/issues/805
            case taskType of
                TTFiles lp _ ->
                  -- FIXME: make this work with exact-configuration.
                  -- Not sure how to plumb the info atm. See
                  -- https://github.com/commercialhaskell/stack/issues/2049
                  [ "--enable-tests" | enableTest || (not useExactConf && depsPresent installedMap (lpTestDeps lp))] ++
                  [ "--enable-benchmarks" | enableBench || (not useExactConf && depsPresent installedMap (lpBenchDeps lp))]
                _ -> []
    idMap <- liftIO $ readTVarIO eeGhcPkgIds
    let getMissing ident =
            case Map.lookup ident idMap of
                Nothing
                    -- Expect to instead find it in installedMap if it's
                    -- an initialBuildSteps target.
                    | boptsCLIInitialBuildSteps eeBuildOptsCLI && taskIsTarget task,
                      Just (_, installed) <- Map.lookup (packageIdentifierName ident) installedMap
                        -> installedToGhcPkgId ident installed
                Just installed -> installedToGhcPkgId ident installed
                _ -> error "singleBuild: invariant violated, missing package ID missing"
        installedToGhcPkgId ident (Library ident' x _) = assert (ident == ident') $ Just (ident, x)
        installedToGhcPkgId _ (Executable _) = Nothing
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
                    TTFiles lp _ -> Set.map (encodeUtf8 . renderComponent) $ lpComponents lp
                    TTIndex{} -> Set.empty
            , configCacheHaddock =
                shouldHaddockPackage eeBuildOpts eeWanted (packageIdentifierName taskProvides)
            , configCachePkgSrc = taskCachePkgSrc
            }
        allDepsMap = Map.union missing' taskPresent
    return (allDepsMap, cache)

-- | Ensure that the configuration for the package matches what is given
ensureConfig :: HasEnvConfig env
             => ConfigCache -- ^ newConfigCache
             -> Path Abs Dir -- ^ package directory
             -> ExecuteEnv
             -> RIO env () -- ^ announce
             -> (ExcludeTHLoading -> [String] -> RIO env ()) -- ^ cabal
             -> Path Abs File -- ^ .cabal file
             -> Task
             -> RIO env Bool
ensureConfig newConfigCache pkgDir ExecuteEnv {..} announce cabal cabalfp task = do
    newCabalMod <- liftIO (fmap modTime (D.getModificationTime (toFilePath cabalfp)))
    needConfig <-
        if boptsReconfigure eeBuildOpts || taskAnyMissing task
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

    when (taskBuildTypeConfig task) ensureConfigureScript

    when needConfig $ withMVar eeConfigureLock $ \_ -> do
        deleteCaches pkgDir
        announce
        let programNames =
                if eeCabalPkgVer < $(mkVersion "1.22")
                    then ["ghc", "ghc-pkg"]
                    else ["ghc", "ghc-pkg", "ghcjs", "ghcjs-pkg"]
        exes <- forM programNames $ \name -> do
            mpath <- findExecutable name
            return $ case mpath of
                Left _ -> []
                Right x -> return $ concat ["--with-", name, "=", x]
        -- Configure cabal with arguments determined by
        -- Stack.Types.Build.configureOpts
        cabal KeepTHLoading $ "configure" : concat
            [ concat exes
            , dirs
            , nodirs
            ]
        writeConfigCache pkgDir newConfigCache
        writeCabalMod pkgDir newCabalMod

    return needConfig
  where
    -- When build-type is Configure, we need to have a configure
    -- script in the local directory. If it doesn't exist, build it
    -- with autoreconf -i. See:
    -- https://github.com/commercialhaskell/stack/issues/3534
    ensureConfigureScript = do
      let fp = pkgDir </> $(mkRelFile "configure")
      exists <- doesFileExist fp
      unless exists $ do
        logInfo $ "Trying to generate configure with autoreconf in " <> fromString (toFilePath pkgDir)
        withWorkingDir (toFilePath pkgDir) $ readProcessNull "autoreconf" ["-i"] `catchAny` \ex ->
          logWarn $ "Unable to run autoreconf: " <> displayShow ex

announceTask :: HasLogFunc env => Task -> Text -> RIO env ()
announceTask task x = logInfo $
    RIO.display (taskProvides task) <>
    ": " <>
    RIO.display x

-- | This sets up a context for executing build steps which need to run
-- Cabal (via a compiled Setup.hs).  In particular it does the following:
--
-- * Ensures the package exists in the file system, downloading if necessary.
--
-- * Opens a log file if the built output shouldn't go to stderr.
--
-- * Ensures that either a simple Setup.hs is built, or the package's
--   custom setup is built.
--
-- * Provides the user a function with which run the Cabal process.
withSingleContext :: forall env a. HasEnvConfig env
                  => ActionContext
                  -> ExecuteEnv
                  -> Task
                  -> Maybe (Map PackageIdentifier GhcPkgId)
                  -- ^ All dependencies' package ids to provide to Setup.hs. If
                  -- Nothing, just provide global and snapshot package
                  -- databases.
                  -> Maybe String
                  -> (  Package                                -- Package info
                     -> Path Abs File                          -- Cabal file path
                     -> Path Abs Dir                           -- Package root directory file path
                     -> (ExcludeTHLoading -> [String] -> RIO env ())
                                                               -- Function to run Cabal with args
                     -> (Text -> RIO env ())             -- An 'announce' function, for different build phases
                     -> Bool                                   -- Whether output should be directed to the console
                     -> Maybe (Path Abs File, Handle)          -- Log file
                     -> RIO env a)
                  -> RIO env a
withSingleContext ActionContext {..} ExecuteEnv {..} task@Task {..} mdeps msuffix inner0 =
    withPackage $ \package cabalfp pkgDir ->
    withLogFile pkgDir package $ \mlogFile ->
    withCabal package pkgDir mlogFile $ \cabal ->
    inner0 package cabalfp pkgDir cabal announce console mlogFile
  where
    announce = announceTask task

    wanted =
        case taskType of
            TTFiles lp _ -> lpWanted lp
            TTIndex{} -> False

    -- Output to the console if this is the last task, and the user
    -- asked to build it specifically. When the action is a
    -- 'ConcurrencyDisallowed' action (benchmarks), then we can also be
    -- sure to have excluse access to the console, so output is also
    -- sent to the console in this case.
    --
    -- See the discussion on #426 for thoughts on sending output to the
    -- console from concurrent tasks.
    console =
      (wanted &&
       all (\(ActionId ident _) -> ident == taskProvides) (Set.toList acRemaining) &&
       eeTotalWanted == 1
      ) || (acConcurrency == ConcurrencyDisallowed)

    withPackage inner =
        case taskType of
            TTFiles lp _ -> inner (lpPackage lp) (lpCabalFile lp) (lpDir lp)
            TTIndex package _ pir -> do
                mdist <- distRelativeDir
                dir <- unpackPackageIdent eeTempDir mdist pir

                let name = packageIdentifierName taskProvides
                cabalfpRel <- parseRelFile $ packageNameString name ++ ".cabal"
                let cabalfp = dir </> cabalfpRel
                inner package cabalfp dir

    withLogFile pkgDir package inner
        | console = inner Nothing
        | otherwise = do
            logPath <- buildLogPath package msuffix
            ensureDir (parent logPath)
            let fp = toFilePath logPath

            -- We only want to dump logs for local non-dependency packages
            case taskType of
                TTFiles lp _ | lpWanted lp ->
                    liftIO $ atomically $ writeTChan eeLogFiles (pkgDir, logPath)
                _ -> return ()

            withBinaryFile fp WriteMode $ \h -> inner (Just (logPath, h))

    withCabal
        :: Package
        -> Path Abs Dir
        -> Maybe (Path Abs File, Handle)
        -> ((ExcludeTHLoading -> [String] -> RIO env ()) -> RIO env a)
        -> RIO env a
    withCabal package pkgDir mlogFile inner = do
        config <- view configL

        unless (configAllowDifferentUser config) $
            checkOwnership (pkgDir </> configWorkDir config)

        let envSettings = EnvSettings
                { esIncludeLocals = taskLocation task == Local
                , esIncludeGhcPackagePath = False
                , esStackExe = False
                , esLocaleUtf8 = True
                , esKeepGhcRts = False
                }
        menv <- liftIO $ configProcessContextSettings config envSettings
        distRelativeDir' <- distRelativeDir
        esetupexehs <-
            -- Avoid broken Setup.hs files causing problems for simple build
            -- types, see:
            -- https://github.com/commercialhaskell/stack/issues/370
            case (packageBuildType package, eeSetupExe) of
                (C.Simple, Just setupExe) -> return $ Left setupExe
                _ -> liftIO $ Right <$> getSetupHs pkgDir
        inner $ \stripTHLoading args -> do
            let cabalPackageArg
                    -- Omit cabal package dependency when building
                    -- Cabal. See
                    -- https://github.com/commercialhaskell/stack/issues/1356
                    | packageName package == $(mkPackageName "Cabal") = []
                    | otherwise =
                        ["-package=" ++ packageIdentifierString
                                            (PackageIdentifier cabalPackageName
                                                              eeCabalPkgVer)]
                packageDBArgs =
                    ( "-clear-package-db"
                    : "-global-package-db"
                    : map (("-package-db=" ++) . toFilePathNoTrailingSep) (bcoExtraDBs eeBaseConfigOpts)
                    ) ++
                    ( ("-package-db=" ++ toFilePathNoTrailingSep (bcoSnapDB eeBaseConfigOpts))
                    : ("-package-db=" ++ toFilePathNoTrailingSep (bcoLocalDB eeBaseConfigOpts))
                    : ["-hide-all-packages"]
                    )

                warnCustomNoDeps :: RIO env ()
                warnCustomNoDeps =
                    case (taskType, packageBuildType package) of
                        (TTFiles lp Local, C.Custom) | lpWanted lp -> do
                            prettyWarnL
                                [ flow "Package"
                                , display $ packageName package
                                , flow "uses a custom Cabal build, but does not use a custom-setup stanza"
                                ]
                        _ -> return ()

                getPackageArgs :: Path Abs Dir -> RIO env [String]
                getPackageArgs setupDir =
                    case (packageSetupDeps package, mdeps) of
                        -- The package is using the Cabal custom-setup
                        -- configuration introduced in Cabal 1.24. In
                        -- this case, the package is providing an
                        -- explicit list of dependencies, and we
                        -- should simply use all of them.
                        (Just customSetupDeps, _) -> do
                            unless (Map.member $(mkPackageName "Cabal") customSetupDeps) $
                                prettyWarnL
                                    [ display $ packageName package
                                    , "has a setup-depends field, but it does not mention a Cabal dependency. This is likely to cause build errors."
                                    ]
                            allDeps <-
                                case mdeps of
                                    Just x -> return x
                                    Nothing -> do
                                        prettyWarnS "In getPackageArgs: custom-setup in use, but no dependency map present"
                                        return Map.empty
                            matchedDeps <- forM (Map.toList customSetupDeps) $ \(name, range) -> do
                                let matches (PackageIdentifier name' version) =
                                        name == name' &&
                                        version `withinRange` range
                                case filter (matches . fst) (Map.toList allDeps) of
                                    x:xs -> do
                                        unless (null xs)
                                            (logWarn ("Found multiple installed packages for custom-setup dep: " <> RIO.display name))
                                        return ("-package-id=" ++ ghcPkgIdString (snd x), Just (toCabalPackageIdentifier (fst x)))
                                    [] -> do
                                        logWarn ("Could not find custom-setup dep: " <> RIO.display name)
                                        return ("-package=" ++ packageNameString name, Nothing)
                            let depsArgs = map fst matchedDeps
                            -- Generate setup_macros.h and provide it to ghc
                            let macroDeps = mapMaybe snd matchedDeps
                                cppMacrosFile = toFilePath $ setupDir </> $(mkRelFile "setup_macros.h")
                                cppArgs = ["-optP-include", "-optP" ++ cppMacrosFile]
                            liftIO $ S.writeFile cppMacrosFile (encodeUtf8 (T.pack (C.generatePackageVersionMacros macroDeps)))
                            return (packageDBArgs ++ depsArgs ++ cppArgs)

                        -- This branch is taken when
                        -- 'explicit-setup-deps' is requested in your
                        -- stack.yaml file.
                        (Nothing, Just deps) | explicitSetupDeps (packageName package) config -> do
                            warnCustomNoDeps
                            -- Stack always builds with the global Cabal for various
                            -- reproducibility issues.
                            let depsMinusCabal
                                 = map ghcPkgIdString
                                 $ Set.toList
                                 $ addGlobalPackages deps (Map.elems eeGlobalDumpPkgs)
                            return (
                                packageDBArgs ++
                                cabalPackageArg ++
                                map ("-package-id=" ++) depsMinusCabal)
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
                        (Nothing, _) -> do
                            warnCustomNoDeps
                            return $ cabalPackageArg ++
                                    -- NOTE: This is different from
                                    -- packageDBArgs above in that it does not
                                    -- include the local database and does not
                                    -- pass in the -hide-all-packages argument
                                    ("-clear-package-db"
                                    : "-global-package-db"
                                    : map (("-package-db=" ++) . toFilePathNoTrailingSep) (bcoExtraDBs eeBaseConfigOpts)
                                ++ ["-package-db=" ++ toFilePathNoTrailingSep (bcoSnapDB eeBaseConfigOpts)])

                setupArgs = ("--builddir=" ++ toFilePathNoTrailingSep distRelativeDir') : args

                runExe :: Path Abs File -> [String] -> RIO env ()
                runExe exeName fullArgs = do
                    compilerVer <- view actualCompilerVersionL
                    runAndOutput compilerVer `catch` \ece -> do
                        bss <-
                            case mlogFile of
                                Nothing -> return []
                                Just (logFile, h) -> do
                                    liftIO $ hClose h
                                    withSourceFile (toFilePath logFile) $ \src ->
                                           runConduit
                                         $ src
                                        .| CT.decodeUtf8Lenient
                                        .| mungeBuildOutput stripTHLoading makeAbsolute pkgDir compilerVer
                                        .| CL.consume
                        throwM $ SetupHsBuildFailure
                            (eceExitCode ece)
                            (Just taskProvides)
                            exeName
                            fullArgs
                            (fmap fst mlogFile)
                            bss
                  where
                    runAndOutput :: CompilerVersion 'CVActual -> RIO env ()
                    runAndOutput compilerVer = withWorkingDir (toFilePath pkgDir) $ withProcessContext menv $ case mlogFile of
                        Just (_, h) ->
                            proc (toFilePath exeName) fullArgs
                          $ runProcess_
                          . setStdin (byteStringInput "")
                          . setStdout (useHandleOpen h)
                          . setStderr (useHandleOpen h)
                        Nothing ->
                            void $ sinkProcessStderrStdout (toFilePath exeName) fullArgs
                                (outputSink KeepTHLoading LevelWarn compilerVer)
                                (outputSink stripTHLoading LevelInfo compilerVer)
                    outputSink
                        :: HasCallStack
                        => ExcludeTHLoading
                        -> LogLevel
                        -> CompilerVersion 'CVActual
                        -> ConduitM S.ByteString Void (RIO env) ()
                    outputSink excludeTH level compilerVer =
                        CT.decodeUtf8Lenient
                        .| mungeBuildOutput excludeTH makeAbsolute pkgDir compilerVer
                        .| CL.mapM_ (logGeneric "" level . RIO.display)
                    -- If users want control, we should add a config option for this
                    makeAbsolute :: ConvertPathsToAbsolute
                    makeAbsolute = case stripTHLoading of
                        ExcludeTHLoading -> ConvertPathsToAbsolute
                        KeepTHLoading    -> KeepPathsAsIs

            exeName <- case esetupexehs of
                Left setupExe -> return setupExe
                Right setuphs -> do
                    distDir <- distDirFromDir pkgDir
                    let setupDir = distDir </> $(mkRelDir "setup")
                        outputFile = setupDir </> $(mkRelFile "setup")
                    customBuilt <- liftIO $ readIORef eeCustomBuilt
                    if Set.member (packageName package) customBuilt
                        then return outputFile
                        else do
                            ensureDir setupDir
                            compiler <- view $ actualCompilerVersionL.whichCompilerL
                            compilerPath <-
                                case compiler of
                                    Ghc -> eeGetGhcPath
                                    Ghcjs -> eeGetGhcjsPath
                            packageArgs <- getPackageArgs setupDir
                            runExe compilerPath $
                                [ "--make"
                                , "-odir", toFilePathNoTrailingSep setupDir
                                , "-hidir", toFilePathNoTrailingSep setupDir
                                , "-i", "-i."
                                ] ++ packageArgs ++
                                [ toFilePath setuphs
                                , toFilePath eeSetupShimHs
                                , "-main-is"
                                , "StackSetupShim.mainOverride"
                                , "-o", toFilePath outputFile
                                , "-threaded"
                                ] ++
                                (case compiler of
                                    Ghc -> []
                                    Ghcjs -> ["-build-runner"])
                            liftIO $ atomicModifyIORef' eeCustomBuilt $
                                \oldCustomBuilt -> (Set.insert (packageName package) oldCustomBuilt, ())
                            return outputFile
            runExe exeName $ (if boptsCabalVerbose eeBuildOpts then ("--verbose":) else id) setupArgs

-- Implements running a package's build, used to implement 'ATBuild' and
-- 'ATBuildFinal' tasks.  In particular this does the following:
--
-- * Checks if the package exists in the precompiled cache, and if so,
--   add it to the database instead of performing the build.
--
-- * Runs the configure step if needed ('ensureConfig')
--
-- * Runs the build step
--
-- * Generates haddocks
--
-- * Registers the library and copiesthe built executables into the
--   local install directory. Note that this is literally invoking Cabal
--   with @copy@, and not the copying done by @stack install@ - that is
--   handled by 'copyExecutables'.
singleBuild :: forall env. (HasEnvConfig env, HasRunner env)
            => ActionContext
            -> ExecuteEnv
            -> Task
            -> InstalledMap
            -> Bool             -- ^ Is this a final build?
            -> RIO env ()
singleBuild ac@ActionContext {..} ee@ExecuteEnv {..} task@Task {..} installedMap isFinalBuild = do
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

    annSuffix executableBuildStatuses = if result == "" then "" else " (" <> result <> ")"
      where
        result = T.intercalate " + " $ concat
            [ ["lib" | taskAllInOne && hasLib]
            , ["exe" | taskAllInOne && hasExe]
            , ["test" | enableTests]
            , ["bench" | enableBenchmarks]
            ]
        (hasLib, hasExe) = case taskType of
            TTFiles lp Local ->
              let hasLibrary =
                    case packageLibraries (lpPackage lp) of
                      NoLibraries -> False
                      HasLibraries _ -> True
               in (hasLibrary, not (Set.null (exesToBuild executableBuildStatuses lp)))
            -- This isn't true, but we don't want to have this info for
            -- upstream deps.
            _ -> (False, False)

    getPrecompiled cache =
        case taskLocation task of
            Snap | not shouldHaddockPackage' -> do
                mpc <-
                  case taskLocation task of
                    Snap -> readPrecompiledCache
                      (ttPackageLocation taskType)
                      (configCacheOpts cache)
                      (configCacheDeps cache)
                    _ -> return Nothing
                case mpc of
                    Nothing -> return Nothing
                    -- Only pay attention to precompiled caches that refer to packages within
                    -- the snapshot.
                    Just pc | maybe False
                                    (bcoSnapInstallRoot eeBaseConfigOpts `isProperPrefixOf`)
                                    (parseAbsFile =<< pcLibrary pc) ->
                        return Nothing
                    -- If old precompiled cache files are left around but snapshots are deleted,
                    -- it is possible for the precompiled file to refer to the very library
                    -- we're building, and if flags are changed it may try to copy the library
                    -- to itself. This check prevents that from happening.
                    Just pc -> do
                        let allM _ [] = return True
                            allM f (x:xs) = do
                                b <- f x
                                if b then allM f xs else return False
                        b <- liftIO $ allM D.doesFileExist $ maybe id (:) (pcLibrary pc) $ pcExes pc
                        return $ if b then Just pc else Nothing
            _ -> return Nothing

    copyPreCompiled (PrecompiledCache mlib exes) = do
        wc <- view $ actualCompilerVersionL.whichCompilerL
        announceTask task "using precompiled package"
        forM_ mlib $ \libpath -> do
            withMVar eeInstallLock $ \() -> do
                -- We want to ignore the global and user databases.
                -- Unfortunately, ghc-pkg doesn't take such arguments on the
                -- command line. Instead, we'll set GHC_PACKAGE_PATH. See:
                -- https://github.com/commercialhaskell/stack/issues/1146

                let modifyEnv = Map.insert
                      (ghcPkgPathEnvVar wc)
                      (T.pack $ toFilePathNoTrailingSep $ bcoSnapDB eeBaseConfigOpts)

                withModifyEnvVars modifyEnv $ do
                  -- In case a build of the library with different flags already exists, unregister it
                  -- before copying.
                  let ghcPkgExe = ghcPkgExeName wc
                  catchAny
                      (readProcessNull ghcPkgExe
                          [ "unregister"
                          , "--force"
                          , packageIdentifierString taskProvides
                          ])
                      (const (return ()))

                  readProcessNull ghcPkgExe
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
                mpkgid <- loadInstalledPkg wc pkgDbs eeSnapshotDumpPkgs pname

                return $ Just $
                    case mpkgid of
                        Nothing -> assert False $ Executable taskProvides
                        Just pkgid -> Library taskProvides pkgid Nothing
      where
        bindir = toFilePath $ bcoSnapInstallRoot eeBaseConfigOpts </> bindirSuffix

    realConfigAndBuild cache allDepsMap = withSingleContext ac ee task (Just allDepsMap) Nothing
        $ \package cabalfp pkgDir cabal announce _console _mlogFile -> do
            executableBuildStatuses <- getExecutableBuildStatuses package pkgDir
            when (not (cabalIsSatisfied executableBuildStatuses) && taskIsTarget task)
                 (logInfo
                      ("Building all executables for `" <> RIO.display (packageName package) <>
                       "' once. After a successful build of all of them, only specified executables will be rebuilt."))

            _neededConfig <- ensureConfig cache pkgDir ee (announce ("configure" <> annSuffix executableBuildStatuses)) cabal cabalfp task

            let installedMapHasThisPkg :: Bool
                installedMapHasThisPkg =
                    case Map.lookup (packageName package) installedMap of
                        Just (_, Library ident _ _) -> ident == taskProvides
                        Just (_, Executable _) -> True
                        _ -> False

            case ( boptsCLIOnlyConfigure eeBuildOptsCLI
                 , boptsCLIInitialBuildSteps eeBuildOptsCLI && taskIsTarget task) of
                -- A full build is done if there are downstream actions,
                -- because their configure step will require that this
                -- package is built. See
                -- https://github.com/commercialhaskell/stack/issues/2787
                (True, _) | null acDownstream -> return Nothing
                (_, True) | null acDownstream || installedMapHasThisPkg -> do
                    initialBuildSteps executableBuildStatuses cabal announce
                    return Nothing
                _ -> liftM Just $ realBuild cache package pkgDir cabal announce executableBuildStatuses

    initialBuildSteps executableBuildStatuses cabal announce = do
        () <- announce ("initial-build-steps" <> annSuffix executableBuildStatuses)
        cabal KeepTHLoading ["repl", "stack-initial-build-steps"]

    realBuild
        :: ConfigCache
        -> Package
        -> Path Abs Dir
        -> (ExcludeTHLoading -> [String] -> RIO env ())
        -> (Text -> RIO env ())
        -> Map Text ExecutableBuildStatus
        -> RIO env Installed
    realBuild cache package pkgDir cabal announce executableBuildStatuses = do
        wc <- view $ actualCompilerVersionL.whichCompilerL

        markExeNotInstalled (taskLocation task) taskProvides
        case taskType of
            TTFiles lp _ -> do -- FIXME should this only be for local packages?
                when enableTests $ unsetTestSuccess pkgDir
                mapM_ (uncurry (writeBuildCache pkgDir))
                      (Map.toList $ lpNewBuildCaches lp)
            TTIndex{} -> return ()

        -- FIXME: only output these if they're in the build plan.

        preBuildTime <- modTime <$> liftIO getCurrentTime
        let postBuildCheck _succeeded = do
                mlocalWarnings <- case taskType of
                    TTFiles lp Local -> do
                        warnings <- checkForUnlistedFiles taskType preBuildTime pkgDir
                        -- TODO: Perhaps only emit these warnings for non extra-dep?
                        return (Just (lpCabalFile lp, warnings))
                    _ -> return Nothing
                -- NOTE: once
                -- https://github.com/commercialhaskell/stack/issues/2649
                -- is resolved, we will want to partition the warnings
                -- based on variety, and output in different lists.
                let showModuleWarning (UnlistedModulesWarning comp modules) =
                      "- In" <+>
                      fromString (T.unpack (renderComponent comp)) <>
                      ":" <> line <>
                      indent 4 (mconcat $ intersperse line $ map (styleGood . fromString . C.display) modules)
                forM_ mlocalWarnings $ \(cabalfp, warnings) -> do
                    unless (null warnings) $ prettyWarn $
                        "The following modules should be added to exposed-modules or other-modules in" <+>
                        display cabalfp <> ":" <> line <>
                        indent 4 (mconcat $ map showModuleWarning warnings) <>
                        line <> line <>
                        "Missing modules in the cabal file are likely to cause undefined reference errors from the linker, along with other problems."

        () <- announce ("build" <> annSuffix executableBuildStatuses)
        config <- view configL
        extraOpts <- extraBuildOptions wc eeBuildOpts
        let stripTHLoading
                | configHideTHLoading config = ExcludeTHLoading
                | otherwise                  = KeepTHLoading
        cabal stripTHLoading (("build" :) $ (++ extraOpts) $
            case (taskType, taskAllInOne, isFinalBuild) of
                (_, True, True) -> error "Invariant violated: cannot have an all-in-one build that also has a final build step."
                (TTFiles lp _, False, False) -> primaryComponentOptions executableBuildStatuses lp
                (TTFiles lp _, False, True) -> finalComponentOptions lp
                (TTFiles lp _, True, False) -> primaryComponentOptions executableBuildStatuses lp ++ finalComponentOptions lp
                (TTIndex{}, _, _) -> [])
          `catch` \ex -> case ex of
              CabalExitedUnsuccessfully{} -> postBuildCheck False >> throwM ex
              _ -> throwM ex
        postBuildCheck True

        when (doHaddock package) $ do
            announce "haddock"
            sourceFlag <- if not (boptsHaddockHyperlinkSource eeBuildOpts) then return [] else do
                -- See #2429 for why the temp dir is used
                ec
                  <- withWorkingDir (toFilePath eeTempDir)
                   $ proc "haddock" ["--hyperlinked-source"]
                   $ \pc -> withProcess
                     (setStdout createSource $ setStderr createSource pc) $ \p ->
                       runConcurrently
                         $ Concurrently (runConduit $ getStdout p .| CL.sinkNull)
                        *> Concurrently (runConduit $ getStderr p .| CL.sinkNull)
                        *> Concurrently (waitExitCode p)
                case ec of
                    -- Fancy crosslinked source
                    ExitSuccess -> return ["--haddock-option=--hyperlinked-source"]
                    -- Older hscolour colouring
                    ExitFailure _ -> do
                        hscolourExists <- doesExecutableExist "HsColour"
                        unless hscolourExists $ logWarn
                            ("Warning: haddock not generating hyperlinked sources because 'HsColour' not\n" <>
                             "found on PATH (use 'stack install hscolour' to install).")
                        return ["--hyperlink-source" | hscolourExists]
            cabal KeepTHLoading $ concat
                [ ["haddock", "--html", "--hoogle", "--html-location=../$pkg-$version/"]
                , sourceFlag
                , ["--internal" | boptsHaddockInternal eeBuildOpts]
                , [ "--haddock-option=" <> opt
                  | opt <- hoAdditionalArgs (boptsHaddockOpts eeBuildOpts) ]
                ]

        let hasLibrary =
              case packageLibraries package of
                NoLibraries -> False
                HasLibraries _ -> True
            shouldCopy = not isFinalBuild && (hasLibrary || not (Set.null (packageExes package)))
        when shouldCopy $ withMVar eeInstallLock $ \() -> do
            announce "copy/register"
            eres <- try $ cabal KeepTHLoading ["copy"]
            case eres of
                Left err@CabalExitedUnsuccessfully{} ->
                    throwM $ CabalCopyFailed (packageBuildType package == C.Simple) (show err)
                _ -> return ()
            when hasLibrary $ cabal KeepTHLoading ["register"]

        let (installedPkgDb, installedDumpPkgsTVar) =
                case taskLocation task of
                    Snap ->
                         ( bcoSnapDB eeBaseConfigOpts
                         , eeSnapshotDumpPkgs )
                    Local ->
                        ( bcoLocalDB eeBaseConfigOpts
                        , eeLocalDumpPkgs )
        let ident = PackageIdentifier (packageName package) (packageVersion package)
        mpkgid <- case packageLibraries package of
            HasLibraries _ -> do
                mpkgid <- loadInstalledPkg wc [installedPkgDb] installedDumpPkgsTVar (packageName package)
                case mpkgid of
                    Nothing -> throwM $ Couldn'tFindPkgId $ packageName package
                    Just pkgid -> return $ Library ident pkgid Nothing
            NoLibraries -> do
                markExeInstalled (taskLocation task) taskProvides -- TODO unify somehow with writeFlagCache?
                return $ Executable ident

        case taskLocation task of
            Snap ->
              writePrecompiledCache
                eeBaseConfigOpts
                (ttPackageLocation taskType)
                (configCacheOpts cache)
                (configCacheDeps cache)
                mpkgid (packageExes package)
            _ -> return ()

        case taskType of
            -- For packages from a package index, pkgDir is in the tmp
            -- directory. We eagerly delete it if no other tasks
            -- require it, to reduce space usage in tmp (#3018).
            TTIndex{} -> do
                let remaining = filter (\(ActionId x _) -> x == taskProvides) (Set.toList acRemaining)
                when (null remaining) $ removeDirRecur pkgDir
            _ -> return ()

        return mpkgid

    loadInstalledPkg wc pkgDbs tvar name = do
        dps <- ghcPkgDescribe name wc pkgDbs $ conduitDumpPackage .| CL.consume
        case dps of
            [] -> return Nothing
            [dp] -> do
                liftIO $ atomically $ modifyTVar' tvar (Map.insert (dpGhcPkgId dp) dp)
                return $ Just (dpGhcPkgId dp)
            _ -> error "singleBuild: invariant violated: multiple results when describing installed package"

-- | Get the build status of all the package executables. Do so by
-- testing whether their expected output file exists, e.g.
--
-- .stack-work/dist/x86_64-osx/Cabal-1.22.4.0/build/alpha/alpha
-- .stack-work/dist/x86_64-osx/Cabal-1.22.4.0/build/alpha/alpha.exe
-- .stack-work/dist/x86_64-osx/Cabal-1.22.4.0/build/alpha/alpha.jsexe/ (NOTE: a dir)
getExecutableBuildStatuses
    :: HasEnvConfig env
    => Package -> Path Abs Dir -> RIO env (Map Text ExecutableBuildStatus)
getExecutableBuildStatuses package pkgDir = do
    compiler <- view $ actualCompilerVersionL.whichCompilerL
    distDir <- distDirFromDir pkgDir
    platform <- view platformL
    fmap
        M.fromList
        (mapM (checkExeStatus compiler platform distDir) (Set.toList (packageExes package)))

-- | Check whether the given executable is defined in the given dist directory.
checkExeStatus
    :: HasLogFunc env
    => WhichCompiler
    -> Platform
    -> Path b Dir
    -> Text
    -> RIO env (Text, ExecutableBuildStatus)
checkExeStatus compiler platform distDir name = do
    exename <- parseRelDir (T.unpack name)
    exists <- checkPath (distDir </> $(mkRelDir "build") </> exename)
    pure
        ( name
        , if exists
              then ExecutableBuilt
              else ExecutableNotBuilt)
  where
    checkPath base =
        case compiler of
            Ghcjs -> do
                dir <- parseRelDir (file ++ ".jsexe")
                doesDirExist (base </> dir)
            _ ->
                case platform of
                    Platform _ Windows -> do
                        fileandext <- parseRelFile (file ++ ".exe")
                        doesFileExist (base </> fileandext)
                    _ -> do
                        fileandext <- parseRelFile file
                        doesFileExist (base </> fileandext)
      where
        file = T.unpack name

-- | Check if any unlisted files have been found, and add them to the build cache.
checkForUnlistedFiles :: HasEnvConfig env => TaskType -> ModTime -> Path Abs Dir -> RIO env [PackageWarning]
checkForUnlistedFiles (TTFiles lp _) preBuildTime pkgDir = do
    (addBuildCache,warnings) <-
        addUnlistedToBuildCache
            preBuildTime
            (lpPackage lp)
            (lpCabalFile lp)
            (lpComponents lp)
            (lpNewBuildCaches lp)
    forM_ (M.toList addBuildCache) $ \(component, newToCache) -> do
        let cache = Map.findWithDefault Map.empty component (lpNewBuildCaches lp)
        writeBuildCache pkgDir component $
            Map.unions (cache : newToCache)
    return warnings
checkForUnlistedFiles TTIndex{} _ _ = return []

-- | Determine if all of the dependencies given are installed
depsPresent :: InstalledMap -> Map PackageName VersionRange -> Bool
depsPresent installedMap deps = all
    (\(name, range) ->
        case Map.lookup name installedMap of
            Just (_, installed) -> installedVersion installed `withinRange` range
            Nothing -> False)
    (Map.toList deps)

-- | Implements running a package's tests. Also handles producing
-- coverage reports if coverage is enabled.
singleTest :: HasEnvConfig env
           => TestOpts
           -> [Text]
           -> ActionContext
           -> ExecuteEnv
           -> Task
           -> InstalledMap
           -> RIO env ()
singleTest topts testsToRun ac ee task installedMap = do
    -- FIXME: Since this doesn't use cabal, we should be able to avoid using a
    -- fullblown 'withSingleContext'.
    (allDepsMap, _cache) <- getConfigCache ee task installedMap True False
    withSingleContext ac ee task (Just allDepsMap) (Just "test") $ \package _cabalfp pkgDir _cabal announce _console mlogFile -> do
        config <- view configL
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
                menv <- liftIO $ configProcessContextSettings config EnvSettings
                    { esIncludeLocals = taskLocation task == Local
                    , esIncludeGhcPackagePath = True
                    , esStackExe = True
                    , esLocaleUtf8 = False
                    , esKeepGhcRts = False
                    }
                withProcessContext menv $ if exists
                    then do
                        -- We clear out the .tix files before doing a run.
                        when needHpc $ do
                            tixexists <- doesFileExist tixPath
                            when tixexists $
                                logWarn ("Removing HPC file " <> fromString (toFilePath tixPath))
                            liftIO $ ignoringAbsence (removeFile tixPath)

                        let args = toAdditionalArgs topts
                            argsDisplay = case args of
                                            [] -> ""
                                            _ -> ", args: " <> T.intercalate " " (map showProcessArgDebug args)
                        announce $ "test (suite: " <> testName <> argsDisplay <> ")"

                        -- Clear "Progress: ..." message before
                        -- redirecting output.
                        when (isNothing mlogFile) $ do
                            logStickyDone ""
                            liftIO $ hFlush stdout
                            liftIO $ hFlush stderr

                        let output setter =
                                case mlogFile of
                                    Nothing -> id
                                    Just (_, h) -> setter (useHandleOpen h)

                        ec <- withWorkingDir (toFilePath pkgDir) $
                          proc (toFilePath exePath) args $ \pc0 -> do
                            let pc = setStdin createPipe
                                   $ output setStdout
                                   $ output setStderr
                                     pc0
                            withProcess pc $ \p -> do
                              when isTestTypeLib $ do
                                logPath <- buildLogPath package (Just stestName)
                                ensureDir (parent logPath)
                                liftIO $ hPutStr (getStdin p) $ show (logPath, testName)
                              waitExitCode p
                        -- Add a trailing newline, incase the test
                        -- output didn't finish with a newline.
                        when (isNothing mlogFile) (logInfo "")
                        -- Move the .tix file out of the package
                        -- directory into the hpc work dir, for
                        -- tidiness.
                        when needHpc $
                            updateTixFile (packageName package) tixPath testName'
                        let announceResult result = announce $ "Test suite " <> testName <> " " <> result
                        case ec of
                            ExitSuccess -> do
                                announceResult "passed"
                                return Map.empty
                            _ -> do
                                announceResult "failed"
                                return $ Map.singleton testName (Just ec)
                    else do
                        logError $ displayShow $ TestSuiteExeMissing
                            (packageBuildType package == C.Simple)
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

            setTestSuccess pkgDir

-- | Implements running a package's benchmarks.
singleBench :: HasEnvConfig env
            => BenchmarkOpts
            -> [Text]
            -> ActionContext
            -> ExecuteEnv
            -> Task
            -> InstalledMap
            -> RIO env ()
singleBench beopts benchesToRun ac ee task installedMap = do
    (allDepsMap, _cache) <- getConfigCache ee task installedMap False True
    withSingleContext ac ee task (Just allDepsMap) (Just "bench") $ \_package _cabalfp _pkgDir cabal announce _console _mlogFile -> do
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
            cabal KeepTHLoading ("bench" : args)

data ExcludeTHLoading = ExcludeTHLoading | KeepTHLoading
data ConvertPathsToAbsolute = ConvertPathsToAbsolute | KeepPathsAsIs

-- | Strip Template Haskell "Loading package" lines and making paths absolute.
mungeBuildOutput :: forall m. MonadIO m
                 => ExcludeTHLoading       -- ^ exclude TH loading?
                 -> ConvertPathsToAbsolute -- ^ convert paths to absolute?
                 -> Path Abs Dir           -- ^ package's root directory
                 -> CompilerVersion 'CVActual -- ^ compiler we're building with
                 -> ConduitM Text Text m ()
mungeBuildOutput excludeTHLoading makeAbsolute pkgDir compilerVer = void $
    CT.lines
    .| CL.map stripCR
    .| CL.filter (not . isTHLoading)
    .| filterLinkerWarnings
    .| toAbsolute
  where
    -- | Is this line a Template Haskell "Loading package" line
    -- ByteString
    isTHLoading :: Text -> Bool
    isTHLoading = case excludeTHLoading of
        KeepTHLoading    -> const False
        ExcludeTHLoading -> \bs ->
            "Loading package " `T.isPrefixOf` bs &&
            ("done." `T.isSuffixOf` bs || "done.\r" `T.isSuffixOf` bs)

    filterLinkerWarnings :: ConduitM Text Text m ()
    filterLinkerWarnings
        -- Check for ghc 7.8 since it's the only one prone to producing
        -- linker warnings on Windows x64
        | getGhcVersion compilerVer >= $(mkVersion "7.8") = doNothing
        | otherwise = CL.filter (not . isLinkerWarning)

    isLinkerWarning :: Text -> Bool
    isLinkerWarning str =
        ("ghc.exe: warning:" `T.isPrefixOf` str || "ghc.EXE: warning:" `T.isPrefixOf` str) &&
        "is linked instead of __imp_" `T.isInfixOf` str

    -- | Convert GHC error lines with file paths to have absolute file paths
    toAbsolute :: ConduitM Text Text m ()
    toAbsolute = case makeAbsolute of
        KeepPathsAsIs          -> doNothing
        ConvertPathsToAbsolute -> CL.mapM toAbsolutePath

    toAbsolutePath :: Text -> m Text
    toAbsolutePath bs = do
        let (x, y) = T.break (== ':') bs
        mabs <-
            if isValidSuffix y
                then liftIO $ liftM (fmap ((T.takeWhile isSpace x <>) . T.pack . toFilePath)) $
                         forgivingAbsence (resolveFile pkgDir (T.unpack $ T.dropWhile isSpace x)) `catch`
                             \(_ :: PathException) -> return Nothing
                else return Nothing
        case mabs of
            Nothing -> return bs
            Just fp -> return $ fp `T.append` y

    doNothing :: ConduitM Text Text m ()
    doNothing = awaitForever yield

    -- | Match the error location format at the end of lines
    isValidSuffix = isRight . parseOnly lineCol
    lineCol = char ':'
           >> choice
                [ num >> char ':' >> num >> optional (char '-' >> num) >> return ()
                , char '(' >> num >> char ',' >> num >> string ")-(" >> num >> char ',' >> num >> char ')' >> return ()
                ]
           >> char ':'
           >> return ()
        where num = some digit

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
extraBuildOptions :: (HasEnvConfig env, HasRunner env)
                  => WhichCompiler -> BuildOpts -> RIO env [String]
extraBuildOptions wc bopts = do
    colorOpt <- appropriateGhcColorFlag
    let ddumpOpts = " -ddump-hi -ddump-to-file"
        optsFlag = compilerOptionsCabalFlag wc
        baseOpts = ddumpOpts ++ maybe "" (" " ++) colorOpt
    if toCoverage (boptsTestOpts bopts)
      then do
        hpcIndexDir <- toFilePathNoTrailingSep <$> hpcRelativeDir
        return [optsFlag, "-hpcdir " ++ hpcIndexDir ++ baseOpts]
      else
        return [optsFlag, baseOpts]

-- Library and executable build components.
primaryComponentOptions :: Map Text ExecutableBuildStatus -> LocalPackage -> [String]
primaryComponentOptions executableBuildStatuses lp =
      -- TODO: get this information from target parsing instead,
      -- which will allow users to turn off library building if
      -- desired
      (case packageLibraries (lpPackage lp) of
         NoLibraries -> []
         HasLibraries names ->
             map T.unpack
           $ T.append "lib:" (packageNameText (packageName (lpPackage lp)))
           : map (T.append "flib:") (Set.toList names)) ++
      map (T.unpack . T.append "exe:") (Set.toList $ exesToBuild executableBuildStatuses lp)

-- | History of this function:
--
-- * Normally it would do either all executables or if the user
--   specified requested components, just build them. Afterwards, due
--   to this Cabal bug <https://github.com/haskell/cabal/issues/2780>,
--   we had to make Stack build all executables every time.
--
-- * In <https://github.com/commercialhaskell/stack/issues/3229> this
--   was flagged up as very undesirable behavior on a large project,
--   hence the behavior below that we build all executables once
--   (modulo success), and thereafter pay attention to user-wanted
--   components.
--
exesToBuild :: Map Text ExecutableBuildStatus -> LocalPackage -> Set Text
exesToBuild executableBuildStatuses lp =
    if cabalIsSatisfied executableBuildStatuses && lpWanted lp
        then exeComponents (lpComponents lp)
        else packageExes (lpPackage lp)

-- | Do the current executables satisfy Cabal's bugged out requirements?
cabalIsSatisfied :: Map k ExecutableBuildStatus -> Bool
cabalIsSatisfied = all (== ExecutableBuilt) . M.elems

-- Test-suite and benchmark build components.
finalComponentOptions :: LocalPackage -> [String]
finalComponentOptions lp =
    map (T.unpack . renderComponent) $
    Set.toList $
    Set.filter (\c -> isCTest c || isCBench c) (lpComponents lp)

taskComponents :: Task -> Set NamedComponent
taskComponents task =
    case taskType task of
        TTFiles lp _ -> lpComponents lp -- FIXME probably just want Local, maybe even just lpWanted
        TTIndex{} -> Set.empty

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
                  -> [DumpPackage () () ()] -- ^ global packages
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
