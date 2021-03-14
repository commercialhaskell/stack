{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
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
    , KeepOutputOpen(..)
    ) where

import           Control.Concurrent.Execute
import           Control.Concurrent.STM (check)
import           Stack.Prelude hiding (Display (..))
import           Crypto.Hash
import           Data.Attoparsec.Text hiding (try)
import qualified Data.ByteArray as Mem (convert)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64.URL as B64URL
import           Data.Char (isSpace)
import           Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Filesystem as CF
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process.Typed (createSource)
import qualified Data.Conduit.Text as CT
import           Data.List hiding (any)
import           Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import           Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Tuple
import           Data.Time (ZonedTime, getZonedTime, formatTime, defaultTimeLocale)
import qualified Data.ByteString.Char8 as S8
import qualified Distribution.PackageDescription as C
import qualified Distribution.Simple.Build.Macros as C
import           Distribution.System            (OS (Windows),
                                                 Platform (Platform))
import qualified Distribution.Text as C
import           Distribution.Types.PackageName (mkPackageName)
import           Distribution.Types.UnqualComponentName (mkUnqualComponentName)
import           Distribution.Version (mkVersion)
import           Path
import           Path.CheckInstall
import           Path.Extra (toFilePathNoTrailingSep, rejectMissingFile)
import           Path.IO hiding (findExecutable, makeAbsolute, withSystemTempDir)
import qualified RIO
import           Stack.Build.Cache
import           Stack.Build.Haddock
import           Stack.Build.Installed
import           Stack.Build.Precompiled (copyPreCompiled, getPrecompiled, loadInstalledPkg, getExecutableBuildStatuses)
import           Stack.Build.Source
import           Stack.Build.Target
import           Stack.Config
import           Stack.Constants
import           Stack.Constants.Config
import           Stack.Coverage
import           Stack.DefaultColorWhen (defaultColorWhen)
import           Stack.GhcPkg
import           Stack.Package
import           Stack.PackageDump
import           Stack.Types.Build
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.Execute
import           Stack.Types.GhcPkgId
import           Stack.Types.NamedComponent
import           Stack.Types.Package
import           Stack.Types.Version
import qualified System.Directory as D
import           System.Environment (getExecutablePath, lookupEnv)
import           System.FileLock (withTryFileLock, SharedExclusive (Exclusive), withFileLock)
import qualified System.FilePath as FP
import           System.IO.Error (isDoesNotExistError)
import           System.PosixCompat.Files (modificationTime, getFileStatus)
import           RIO.PrettyPrint
import           RIO.Process
import           Pantry.Internal.Companion

-- | Fetch the packages necessary for a build, for example in combination with a dry run.
preFetch :: HasEnvConfig env => Plan -> RIO env ()
preFetch plan
    | Set.null pkgLocs = logDebug "Nothing to fetch"
    | otherwise = do
        logDebug $
            "Prefetching: " <>
            mconcat (intersperse ", " (RIO.display <$> Set.toList pkgLocs))
        fetchPackages pkgLocs
  where
    pkgLocs = Set.unions $ map toPkgLoc $ Map.elems $ planTasks plan

    toPkgLoc task =
        case taskType task of
            TTLocalMutable{} -> Set.empty
            TTRemotePackage _ _ pkgloc -> Set.singleton pkgloc

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

simpleSetupCode :: Builder
simpleSetupCode = "import Distribution.Simple\nmain = defaultMain"

simpleSetupHash :: String
simpleSetupHash =
    T.unpack $ decodeUtf8 $ S.take 8 $ B64URL.encode $ Mem.convert $ hashWith SHA256 $
    toStrictBytes $
    Data.ByteString.Builder.toLazyByteString $
    encodeUtf8Builder (T.pack (unwords buildSetupArgs)) <> setupGhciShimCode <> simpleSetupCode

-- | Get a compiled Setup exe based on the path of a Setup.hs file.
-- Note that this is never invoked directly, it is passed to the
-- relevant functions (realBuild essentially) through the 'withExecuteEnv'
-- bracket-style function.
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
        setupDir =
            view stackRootL config </>
            relDirSetupExeCache </>
            platformDir

    exePath <- (setupDir </>) <$> parseRelFile exeNameS

    exists <- liftIO $ D.doesFileExist $ toFilePath exePath

    if exists
        then return $ Just exePath
        else do
            tmpExePath <- fmap (setupDir </>) $ parseRelFile $ "tmp-" ++ exeNameS
            tmpOutputPath <- fmap (setupDir </>) $ parseRelFile $ "tmp-" ++ outputNameS
            ensureDir setupDir
            let args = buildSetupArgs ++
                    [ "-package"
                    , "Cabal-" ++ cabalVersionString
                    , toFilePath setupHs
                    , toFilePath setupShimHs
                    , "-o"
                    , toFilePath tmpOutputPath
                    ]
            compilerPath <- getCompilerPath
            withWorkingDir (toFilePath tmpdir) (proc (toFilePath compilerPath) args $ \pc0 -> do
              let pc = setStdout (useHandleOpen stderr) pc0
              runProcess_ pc)
                `catch` \ece ->
                    throwM $ SetupHsBuildFailure (eceExitCode ece) Nothing compilerPath args Nothing []
            renameFile tmpExePath exePath
            return $ Just exePath

-- | Execute a function that takes an 'ExecuteEnv'.
withExecuteEnv :: forall env a. HasEnvConfig env
               => BuildOpts
               -> BuildOptsCLI
               -> BaseConfigOpts
               -> [LocalPackage]
               -> [DumpPackage] -- ^ global packages
               -> [DumpPackage] -- ^ snapshot packages
               -> [DumpPackage] -- ^ local packages
               -> Maybe Int -- ^ largest package name, for nicer interleaved output
               -> (ExecuteEnv -> RIO env a)
               -> RIO env a
withExecuteEnv bopts boptsCli baseConfigOpts locals globalPackages snapshotPackages localPackages mlargestPackageName inner =
    createTempDirFunction stackProgName $ \tmpdir -> do
        configLock <- liftIO $ newMVar ()
        installLock <- liftIO $ newMVar ()
        idMap <- liftIO $ newTVarIO Map.empty
        config <- view configL

        customBuiltRef <- newIORef Set.empty

        -- Create files for simple setup and setup shim, if necessary
        let setupSrcDir =
                view stackRootL config </>
                relDirSetupExeSrc
        ensureDir setupSrcDir
        setupFileName <- parseRelFile ("setup-" ++ simpleSetupHash ++ ".hs")
        let setupHs = setupSrcDir </> setupFileName
        setupHsExists <- doesFileExist setupHs
        unless setupHsExists $ writeBinaryFileAtomic setupHs simpleSetupCode
        setupShimFileName <- parseRelFile ("setup-shim-" ++ simpleSetupHash ++ ".hs")
        let setupShimHs = setupSrcDir </> setupShimFileName
        setupShimHsExists <- doesFileExist setupShimHs
        unless setupShimHsExists $ writeBinaryFileAtomic setupShimHs setupGhciShimCode
        setupExe <- getSetupExe setupHs setupShimHs tmpdir

        cabalPkgVer <- view cabalVersionL
        globalDB <- view $ compilerPathsL.to cpGlobalDB
        snapshotPackagesTVar <- liftIO $ newTVarIO (toDumpPackagesByGhcPkgId snapshotPackages)
        localPackagesTVar <- liftIO $ newTVarIO (toDumpPackagesByGhcPkgId localPackages)
        logFilesTChan <- liftIO $ atomically newTChan
        let totalWanted = length $ filter lpShouldBeBuilt locals
        pathEnvVar <- liftIO $ maybe mempty T.pack <$> lookupEnv "PATH"
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
            , eeLocals = locals
            , eeGlobalDB = globalDB
            , eeGlobalDumpPkgs = toDumpPackagesByGhcPkgId globalPackages
            , eeSnapshotDumpPkgs = snapshotPackagesTVar
            , eeLocalDumpPkgs = localPackagesTVar
            , eeLogFiles = logFilesTChan
            , eeCustomBuilt = customBuiltRef
            , eeLargestPackageName = mlargestPackageName
            , eePathEnvVar = pathEnvVar
            } `finally` dumpLogs logFilesTChan totalWanted
  where
    toDumpPackagesByGhcPkgId = Map.fromList . map (\dp -> (dpGhcPkgId dp, dp))

    createTempDirFunction
        | boptsKeepTmpFiles bopts = withKeepSystemTempDir
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
               || "mwarning:" `T.isInfixOf` t -- colorized output

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
            -> [DumpPackage] -- ^ global packages
            -> [DumpPackage] -- ^ snapshot packages
            -> [DumpPackage] -- ^ local packages
            -> InstalledMap
            -> Map PackageName Target
            -> Plan
            -> RIO env ()
executePlan boptsCli baseConfigOpts locals globalPackages snapshotPackages localPackages installedMap targets plan = do
    logDebug "Executing the build plan"
    bopts <- view buildOptsL
    withExecuteEnv bopts boptsCli baseConfigOpts locals globalPackages snapshotPackages localPackages mlargestPackageName
      (executePlan' installedMap targets plan)

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
  where
    mlargestPackageName =
      Set.lookupMax $
      Set.map (length . packageNameString) $
      Map.keysSet (planTasks plan) <> Map.keysSet (planFinals plan)

-- | This is not doing the cabal copy.
-- It is the copying done by @stack install@.
-- The cabal copying is done in 'realBuild'.
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
    case nonEmpty . Map.toList $ planUnregisterLocal plan of
        Nothing -> return ()
        Just ids -> do
            localDB <- packageDatabaseLocal
            unregisterPackages cv localDB ids

    liftIO $ atomically $ modifyTVar' eeLocalDumpPkgs $ \initMap ->
        foldl' (flip Map.delete) initMap $ Map.keys (planUnregisterLocal plan)

    run <- askRunInIO

    -- If running tests concurrently with eachother, then create an MVar
    -- which is empty while each test is being run.
    concurrentTests <- view $ configL.to configConcurrentTests
    mtestLock <- if concurrentTests then return Nothing else Just <$> liftIO (newMVar ())
    let taskAndFinals = mergeTasksAndFinal plan
    let actions = concatMap (toActions installedMap' mtestLock run ee) taskAndFinals
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
                    let packageNames = map (\(ActionId pkgID _) -> pkgName pkgID) (toList inProgress)
                        nowBuilding :: [PackageName] -> Utf8Builder
                        nowBuilding []    = ""
                        nowBuilding names = mconcat $ ": " : intersperse ", " (map (fromString . packageNameString) names)
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
        generateLocalHaddockIndex eeBaseConfigOpts localDumpPkgs eeLocals
        generateDepsHaddockIndex eeBaseConfigOpts eeGlobalDumpPkgs snapshotDumpPkgs localDumpPkgs eeLocals
        generateSnapHaddockIndex eeBaseConfigOpts eeGlobalDumpPkgs snapshotDumpPkgs
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
                  $ map (\(ident, _) -> (pkgName ident, ()))
                  $ Map.elems
                  $ planUnregisterLocal plan

unregisterPackages ::
       (HasProcessContext env, HasLogFunc env, HasPlatform env, HasCompiler env)
    => ActualCompiler
    -> Path Abs Dir
    -> NonEmpty (GhcPkgId, (PackageIdentifier, Text))
    -> RIO env ()
unregisterPackages cv localDB ids = do
    let logReason ident reason =
            logInfo $
            fromString (packageIdentifierString ident) <> ": unregistering" <>
            if T.null reason
                then ""
                else " (" <> RIO.display reason <> ")"
    let unregisterSinglePkg select (gid, (ident, reason)) = do
            logReason ident reason
            pkg <- getGhcPkgExe
            unregisterGhcPkgIds pkg localDB $ select ident gid :| []

    case cv of
        -- GHC versions >= 8.2.1 support batch unregistering of packages. See
        -- https://gitlab.haskell.org/ghc/ghc/issues/12637
        ACGhc v | v >= mkVersion [8, 2, 1] -> do
                platform <- view platformL
                -- According to https://support.microsoft.com/en-us/help/830473/command-prompt-cmd-exe-command-line-string-limitation
                -- the maximum command line length on Windows since XP is 8191 characters.
                -- We use conservative batch size of 100 ids on this OS thus argument name '-ipid', package name,
                -- its version and a hash should fit well into this limit.
                -- On Unix-like systems we're limited by ARG_MAX which is normally hundreds
                -- of kilobytes so batch size of 500 should work fine.
                let batchSize = case platform of
                      Platform _ Windows -> 100
                      _ -> 500
                let chunksOfNE size = mapMaybe nonEmpty . chunksOf size . NonEmpty.toList
                for_ (chunksOfNE batchSize ids) $ \batch -> do
                    for_ batch $ \(_, (ident, reason)) -> logReason ident reason
                    pkg <- getGhcPkgExe
                    unregisterGhcPkgIds pkg localDB $ fmap (Right . fst) batch

        -- GHC versions >= 7.9 support unregistering of packages via their
        -- GhcPkgId.
        ACGhc v | v >= mkVersion [7, 9] -> for_ ids . unregisterSinglePkg $ \_ident gid -> Right gid

        _ -> for_ ids . unregisterSinglePkg $ \ident _gid -> Left ident

-- | Transforms the 'Task' from 'Plan' into a list of 'Action'.
-- The 'Action' datatype is not openely descriptive such as the 'Task' datatype,
-- instead, it contains an arbitrary function (actionDo) which is executed when
-- the action is passed to 'runAction'.
-- The end result may have more than one action, because a 'Task'
-- can appear as normal and final.
-- See "Stack.Build.ConstructPlan", the addFinal function for more details about final/normal.
toActions :: HasEnvConfig env
          => InstalledMap
          -- ^ The map of installed packages in the package database
          -- (see 'InstalledMap' for more info).
          -> Maybe (MVar ())
          -> (RIO env () -> IO ())
          -> ExecuteEnv
          -> (Maybe Task, Maybe Task)
          -- ^ fst is build and snd is final.
          -- They both represent the same package.
          -- It's taken from the 'Plan' in "ConstructPlan"
          -> [Action] 
toActions installedMap mtestLock runInBase ee (mbuild, mfinal) =
    abuild ++ afinal
  where
    abuild = maybeToList $ (\bTask -> setupAction bTask ATBuild mempty) <$> mbuild
    afinal =
        case mfinal of
            Nothing -> []
            Just fTask ->
                appendIfNot (taskAllInOne fTask) (setupAction fTask ATBuildFinal mempty) $
                -- These are the "final" actions - running tests and benchmarks.
                appendIfNot (Set.null tests) (setupAction fTask ATRunTests tests) $
                appendIfNot (Set.null benches) (setupAction fTask ATRunBenchmarks benches)
                []
              where
                -- | Takes a condition and an element, append the element if the condition is false.
                appendIfNot ifCond e = if ifCond then id else (:) e
                comps = taskComponents fTask
                tests = testComponents comps
                benches = benchComponents comps
    setupAction :: Task -> ActionType -> Set Text -> Action
    setupAction task !actType compSet = Action
        { actionId = ActionId taskProvidesVal actType
        , actionDeps = case actType of
            ATBuild -> mainBuildValDeps
            ATBuildFinal -> addBuild mainBuildValDeps
            _ -> finalDeps
        , actionDo = case actType of
            ATRunTests -> \ac -> withLock mtestLock $ runInBase $ do
                            let topts = boptsTestOpts bopts
                            singleTest topts (Set.toList compSet) ac ee task installedMap
            ATRunBenchmarks -> \ac -> runInBase $ do
                            let beopts = boptsBenchmarkOpts bopts
                            singleBench beopts (Set.toList compSet) ac ee task installedMap
            _ -> \ac -> runInBase $ do
                let builder = singleBuild ac ee task installedMap (actType == ATBuildFinal)
                -- The idea here, is that, if we can we use component based builds, we should.
                -- I.e, if no components exist in the Task,
                -- we fallback to the previous (full-package) build system.
                -- For now this only supports component builds for local packages.
                if null taskCompList
                    then builder Nothing
                    else sequence_ $ (builder . Just) <$> taskCompList
        , actionConcurrency = case actType of
            -- Never run benchmarks concurrently with any other task, see #3663
            ATRunBenchmarks -> ConcurrencyDisallowed
            -- Always allow tests tasks to run concurrently with
            -- other tasks, particularly build tasks. Note that
            -- 'mtestLock' can optionally make it so that only
            -- one test is run at a time.
            _ -> ConcurrencyAllowed
        }
        where
            tcoMissingVal = tcoMissing $ taskConfigOpts task
            mainBuildValDeps = Set.map (\ident -> ActionId ident ATBuild) tcoMissingVal
            finalDeps
                | taskAllInOne task = addBuild mempty
                | otherwise = Set.singleton (ActionId taskProvidesVal ATBuildFinal)
            taskProvidesVal = taskProvides task
            taskCompList = sortOn naiveExecutionOrdering ((Set.toList . taskComponentSet $ task) <> [])
            addBuild
                | isNothing mbuild = id -- has no normal build
                | otherwise = Set.insert $ ActionId taskProvidesVal ATBuild
            withLock Nothing f = f
            withLock (Just lock) f = withMVar lock $ \() -> f
            bopts = eeBuildOpts ee

-- | Generate the ConfigCache.
getConfigCache :: HasEnvConfig env
               => ExecuteEnv -> Task -> InstalledMap -> Bool -> Bool -> Bool
               -> RIO env (Map PackageIdentifier GhcPkgId, ConfigCache)
getConfigCache ExecuteEnv {..} task@Task {..} installedMap enableTest enableBench isNotComponentBuild = do
    let extra =
            -- We enable tests if the test suite dependencies are already
            -- installed, so that we avoid unnecessary recompilation based on
            -- cabal_macros.h changes when switching between 'stack build' and
            -- 'stack test'. See:
            -- https://github.com/commercialhaskell/stack/issues/805
            case taskType of
                TTLocalMutable _ ->
                  -- FIXME: make this work with exact-configuration.
                  -- Not sure how to plumb the info atm. See
                  -- https://github.com/commercialhaskell/stack/issues/2049
                  [ "--enable-tests" | isNotComponentBuild && enableTest] ++
                  [ "--enable-benchmarks" | isNotComponentBuild && enableBench]
                TTRemotePackage{} -> []
    idMap <- liftIO $ readTVarIO eeGhcPkgIds
    let getMissing ident =
            case Map.lookup ident idMap of
                Nothing
                    -- Expect to instead find it in installedMap if it's
                    -- an initialBuildSteps target.
                    | boptsCLIInitialBuildSteps eeBuildOptsCLI && taskIsTarget task,
                      Just (_, installed) <- Map.lookup (pkgName ident) installedMap
                        -> installedToGhcPkgId ident installed
                Just installed -> installedToGhcPkgId ident installed
                _ -> error $ "singleBuild: invariant violated, missing package ID missing: " ++ show ident
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
                    TTLocalMutable lp -> Set.map (encodeUtf8 . renderComponent) $ lpComponents lp
                    TTRemotePackage{} -> Set.empty
            , configCacheHaddock = taskBuildHaddock
            , configCachePkgSrc = taskCachePkgSrc
            , configCachePathEnvVar = eePathEnvVar
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
             -> Maybe NamedComponent
             -> RIO env Bool
ensureConfig newConfigCache pkgDir ExecuteEnv {..} announce cabal cabalfp task maybeComp = do
    newCabalMod <- liftIO $ modificationTime <$> getFileStatus (toFilePath cabalfp)
    setupConfigfp <- setupConfigFromDir pkgDir
    newSetupConfigMod <- liftIO $ either (const Nothing) (Just . modificationTime) <$>
      tryJust (guard . isDoesNotExistError) (getFileStatus (toFilePath setupConfigfp))
    -- See https://github.com/commercialhaskell/stack/issues/3554
    taskAnyMissingHack <- view $ actualCompilerVersionL.to getGhcVersion.to (< mkVersion [8, 4])
    needConfig <-
        if boptsReconfigure eeBuildOpts || (taskAnyMissing task && taskAnyMissingHack)
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

                -- Cabal's setup-config is created per OS/Cabal version, multiple
                -- projects using the same package could get a conflict because of this
                mOldSetupConfigMod <- tryGetSetupConfigMod pkgDir

                return $ fmap ignoreComponents mOldConfigCache /= Just (ignoreComponents newConfigCache)
                      || mOldCabalMod /= Just newCabalMod
                      || mOldSetupConfigMod /= newSetupConfigMod
    let ConfigureOpts dirs nodirs = configCacheOpts newConfigCache

    when (taskBuildTypeConfig task) ensureConfigureScript

    when needConfig $ withMVar eeConfigureLock $ \_ -> do
        deleteCaches pkgDir
        announce
        cp <- view compilerPathsL
        let (GhcPkgExe pkgPath) = cpPkg cp
        let programNames =
              case cpWhich cp of
                Ghc ->
                  [ "--with-ghc=" ++ toFilePath (cpCompiler cp)
                  , "--with-ghc-pkg=" ++ toFilePath pkgPath
                  ]
        exes <- forM programNames $ \name -> do
            mpath <- findExecutable name
            return $ case mpath of
                Left _ -> []
                Right x -> return $ concat ["--with-", name, "=", x]
        -- Configure cabal with arguments determined by
        -- Stack.Types.Build.configureOpts
        let componentBasedArg = configureComponentFlag (taskPackageName task) maybeComp
        cabal KeepTHLoading $ "configure" : componentBasedArg <> concat
            [ concat exes
            , dirs
            , nodirs
            ]
        -- Only write the cache for local packages.  Remote packages are built
        -- in a temporary directory so the cache would never be used anyway.
        case taskType task of
            TTLocalMutable{} -> writeConfigCache pkgDir newConfigCache
            TTRemotePackage{} -> return ()
        writeCabalMod pkgDir newCabalMod

    return needConfig
  where
    -- When build-type is Configure, we need to have a configure
    -- script in the local directory. If it doesn't exist, build it
    -- with autoreconf -i. See:
    -- https://github.com/commercialhaskell/stack/issues/3534
    ensureConfigureScript = do
      let fp = pkgDir </> relFileConfigure
      exists <- doesFileExist fp
      unless exists $ do
        logInfo $ "Trying to generate configure with autoreconf in " <> fromString (toFilePath pkgDir)
        let autoreconf = if osIsWindows
                           then readProcessNull "sh" ["autoreconf", "-i"]
                           else readProcessNull "autoreconf" ["-i"]
            -- On Windows 10, an upstream issue with the `sh autoreconf -i`
            -- command means that command clears, but does not then restore, the
            -- ENABLE_VIRTUAL_TERMINAL_PROCESSING flag for native terminals. The
            -- following hack re-enables the lost ANSI-capability.
            fixupOnWindows = when osIsWindows (void $ liftIO defaultColorWhen)
        withWorkingDir (toFilePath pkgDir) $ autoreconf `catchAny` \ex -> do
          fixupOnWindows
          logWarn $ "Unable to run autoreconf: " <> displayShow ex
          when osIsWindows $ do
            logInfo $ "Check that executable perl is on the path in stack's " <>
              "MSYS2 \\usr\\bin folder, and working, and that script file " <>
              "autoreconf is on the path in that location. To check that " <>
              "perl or autoreconf are on the path in the required location, " <>
              "run commands:"
            logInfo ""
            logInfo "  stack exec where -- perl"
            logInfo "  stack exec where -- autoreconf"
            logInfo ""
            logInfo $ "If perl or autoreconf is not on the path in the " <>
              "required location, add them with command (note that the " <>
              "relevant package name is 'autoconf' not 'autoreconf'):"
            logInfo ""
            logInfo "  stack exec pacman -- --sync --refresh autoconf"
            logInfo ""
            logInfo $ "Some versions of perl from MYSY2 are broken. See " <>
              "https://github.com/msys2/MSYS2-packages/issues/1611 and " <>
              "https://github.com/commercialhaskell/stack/pull/4781. To " <>
              "test if perl in the required location is working, try command:"
            logInfo ""
            logInfo "  stack exec perl -- --version"
            logInfo ""
        fixupOnWindows

-- | Ensure we're the only action using the directory.  See
-- <https://github.com/commercialhaskell/stack/issues/2730>
withLockedDistDir
  :: HasEnvConfig env
  => (Utf8Builder -> RIO env ()) -- ^ announce
  -> Path Abs Dir -- ^ root directory for package
  -> RIO env a
  -> RIO env a
withLockedDistDir announce root inner = do
  distDir <- distRelativeDir
  let lockFP = root </> distDir </> relFileBuildLock
  ensureDir $ parent lockFP

  mres <-
    withRunInIO $ \run ->
    withTryFileLock (toFilePath lockFP) Exclusive $ \_lock ->
    run inner

  case mres of
    Just res -> pure res
    Nothing -> do
      let complainer delay = do
            delay 5000000 -- 5 seconds
            announce $ "blocking for directory lock on " <> fromString (toFilePath lockFP)
            forever $ do
              delay 30000000 -- 30 seconds
              announce $ "still blocking for directory lock on " <>
                         fromString (toFilePath lockFP) <>
                         "; maybe another Stack process is running?"
      withCompanion complainer $
        \stopComplaining ->
        withRunInIO $ \run ->
        withFileLock (toFilePath lockFP) Exclusive $ \_ ->
        run $ stopComplaining *> inner

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
                  -> Map PackageIdentifier GhcPkgId
                  -- ^ All dependencies' package ids to provide to Setup.hs.
                  -> Maybe String
                  -> (  Package                                -- Package info
                     -> Path Abs File                          -- Cabal file path
                     -> Path Abs Dir                           -- Package root directory file path
                     -- Note that the `Path Abs Dir` argument is redundant with the `Path Abs File`
                     -- argument, but we provide both to avoid recalculating `parent` of the `File`.
                     -> (KeepOutputOpen -> ExcludeTHLoading -> [String] -> RIO env ())
                                                               -- Function to run Cabal with args
                     -> (Utf8Builder -> RIO env ())      -- An 'announce' function, for different build phases
                     -> OutputType
                     -> RIO env a)
                  -> RIO env a
withSingleContext ActionContext {..} ee@ExecuteEnv {..} task@Task {..} allDeps msuffix inner0 =
    withPackage $ \package cabalfp pkgDir ->
    withOutputType pkgDir package $ \outputType ->
    withCabal package pkgDir outputType $ \cabal ->
    inner0 package cabalfp pkgDir cabal announce outputType
  where
    announce = announceTask ee task

    wanted =
        case taskType of
            TTLocalMutable lp -> lpShouldBeBuilt lp
            TTRemotePackage{} -> False

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
            TTLocalMutable lp -> do
              let root = parent $ lpCabalFile lp
              withLockedDistDir announce root $
                inner (lpPackage lp) (lpCabalFile lp) root
            TTRemotePackage _ package pkgloc -> do
                suffix <- parseRelDir $ packageIdentifierString $ packageIdent package
                let dir = eeTempDir </> suffix
                unpackPackageLocation dir pkgloc

                -- See: https://github.com/fpco/stack/issues/157
                distDir <- distRelativeDir
                let oldDist = dir </> relDirDist
                    newDist = dir </> distDir
                exists <- doesDirExist oldDist
                when exists $ do
                  -- Previously used takeDirectory, but that got confused
                  -- by trailing slashes, see:
                  -- https://github.com/commercialhaskell/stack/issues/216
                  --
                  -- Instead, use Path which is a bit more resilient
                  ensureDir $ parent newDist
                  renameDir oldDist newDist

                let name = pkgName taskProvides
                cabalfpRel <- parseRelFile $ packageNameString name ++ ".cabal"
                let cabalfp = dir </> cabalfpRel
                inner package cabalfp dir

    withOutputType pkgDir package inner
        -- Not in interleaved mode. When building a single wanted package, dump
        -- to the console with no prefix.
        | console = inner $ OTConsole Nothing

        -- If the user requested interleaved output, dump to the console with a
        -- prefix.
        | boptsInterleavedOutput eeBuildOpts =
             inner $ OTConsole $ Just $ packageNamePrefix ee $ packageName package

        -- Neither condition applies, dump to a file.
        | otherwise = do
            logPath <- buildLogPath package msuffix
            ensureDir (parent logPath)
            let fp = toFilePath logPath

            -- We only want to dump logs for local non-dependency packages
            case taskType of
                TTLocalMutable lp | lpShouldBeBuilt lp ->
                    liftIO $ atomically $ writeTChan eeLogFiles (pkgDir, logPath)
                _ -> return ()

            withBinaryFile fp WriteMode $ \h -> inner $ OTLogFile logPath h

    withCabal
        :: Package
        -> Path Abs Dir
        -> OutputType
        -> ((KeepOutputOpen -> ExcludeTHLoading -> [String] -> RIO env ()) -> RIO env a)
        -> RIO env a
    withCabal package pkgDir outputType inner = do
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
        inner $ \keepOutputOpen stripTHLoading args -> do
            let cabalPackageArg
                    -- Omit cabal package dependency when building
                    -- Cabal. See
                    -- https://github.com/commercialhaskell/stack/issues/1356
                    | packageName package == mkPackageName "Cabal" = []
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
                        (TTLocalMutable lp, C.Custom) | lpShouldBeBuilt lp -> do
                            prettyWarnL
                                [ flow "Package"
                                , fromString $ packageNameString $ packageName package
                                , flow "uses a custom Cabal build, but does not use a custom-setup stanza"
                                ]
                        _ -> return ()

                getPackageArgs :: Path Abs Dir -> RIO env [String]
                getPackageArgs setupDir =
                    case packageSetupDeps package of
                        -- The package is using the Cabal custom-setup
                        -- configuration introduced in Cabal 1.24. In
                        -- this case, the package is providing an
                        -- explicit list of dependencies, and we
                        -- should simply use all of them.
                        Just customSetupDeps -> do
                            unless (Map.member (mkPackageName "Cabal") customSetupDeps) $
                                prettyWarnL
                                    [ fromString $ packageNameString $ packageName package
                                    , "has a setup-depends field, but it does not mention a Cabal dependency. This is likely to cause build errors."
                                    ]
                            matchedDeps <- forM (Map.toList customSetupDeps) $ \(name, range) -> do
                                let matches (PackageIdentifier name' version) =
                                        name == name' &&
                                        version `withinRange` range
                                case filter (matches . fst) (Map.toList allDeps) of
                                    x:xs -> do
                                        unless (null xs)
                                            (logWarn ("Found multiple installed packages for custom-setup dep: " <> fromString (packageNameString name)))
                                        return ("-package-id=" ++ ghcPkgIdString (snd x), Just (fst x))
                                    [] -> do
                                        logWarn ("Could not find custom-setup dep: " <> fromString (packageNameString name))
                                        return ("-package=" ++ packageNameString name, Nothing)
                            let depsArgs = map fst matchedDeps
                            -- Generate setup_macros.h and provide it to ghc
                            let macroDeps = mapMaybe snd matchedDeps
                                cppMacrosFile = setupDir </> relFileSetupMacrosH
                                cppArgs = ["-optP-include", "-optP" ++ toFilePath cppMacrosFile]
                            writeBinaryFileAtomic cppMacrosFile (encodeUtf8Builder (T.pack (C.generatePackageVersionMacros macroDeps)))
                            return (packageDBArgs ++ depsArgs ++ cppArgs)

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
                        Nothing -> do
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
                    logInfo $ fromString $ mconcat $ intersperse "; " fullArgs
                    runAndOutput compilerVer `catch` \ece -> do
                        (mlogFile, bss) <-
                            case outputType of
                                OTConsole _ -> return (Nothing, [])
                                OTLogFile logFile h ->
                                    if keepOutputOpen == KeepOpen
                                    then return (Nothing, []) -- expected failure build continues further
                                    else do
                                        liftIO $ hClose h
                                        fmap (Just logFile,) $ withSourceFile (toFilePath logFile) $ \src ->
                                               runConduit
                                             $ src
                                            .| CT.decodeUtf8Lenient
                                            .| mungeBuildOutput stripTHLoading makeAbsolute pkgDir compilerVer
                                            .| CL.consume
                        throwM $ CabalExitedUnsuccessfully
                            (eceExitCode ece)
                            taskProvides
                            exeName
                            fullArgs
                            mlogFile
                            bss
                  where
                    runAndOutput :: ActualCompiler -> RIO env ()
                    runAndOutput compilerVer = withWorkingDir (toFilePath pkgDir) $ withProcessContext menv $ case outputType of
                        OTLogFile _ h -> do
                          let prefixWithTimestamps =
                                if configPrefixTimestamps config
                                   then PrefixWithTimestamps
                                   else WithoutTimestamps
                          void $ sinkProcessStderrStdout (toFilePath exeName) fullArgs
                              (sinkWithTimestamps prefixWithTimestamps h)
                              (sinkWithTimestamps prefixWithTimestamps h)
                        OTConsole mprefix ->
                            let prefix = fold mprefix in
                            void $ sinkProcessStderrStdout (toFilePath exeName) fullArgs
                                (outputSink KeepTHLoading LevelWarn compilerVer prefix)
                                (outputSink stripTHLoading LevelInfo compilerVer prefix)
                    outputSink
                        :: HasCallStack
                        => ExcludeTHLoading
                        -> LogLevel
                        -> ActualCompiler
                        -> Utf8Builder
                        -> ConduitM S.ByteString Void (RIO env) ()
                    outputSink excludeTH level compilerVer prefix =
                        CT.decodeUtf8Lenient
                        .| mungeBuildOutput excludeTH makeAbsolute pkgDir compilerVer
                        .| CL.mapM_ (logGeneric "" level . (prefix <>) . RIO.display)
                    -- If users want control, we should add a config option for this
                    makeAbsolute :: ConvertPathsToAbsolute
                    makeAbsolute = case stripTHLoading of
                        ExcludeTHLoading -> ConvertPathsToAbsolute
                        KeepTHLoading    -> KeepPathsAsIs

            exeName <- case esetupexehs of
                Left setupExe -> return setupExe
                Right setuphs -> do
                    distDir <- distDirFromDir pkgDir
                    let setupDir = distDir </> relDirSetup
                        outputFile = setupDir </> relFileSetupLower
                    customBuilt <- liftIO $ readIORef eeCustomBuilt
                    if Set.member (packageName package) customBuilt
                        then return outputFile
                        else do
                            ensureDir setupDir
                            compilerPath <- view $ compilerPathsL.to cpCompiler
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

                                -- Apply GHC options
                                -- https://github.com/commercialhaskell/stack/issues/4526
                                map T.unpack (
                                  Map.findWithDefault [] AGOEverything (configGhcOptionsByCat config) ++
                                  case configApplyGhcOptions config of
                                    AGOEverything -> boptsCLIGhcOptions eeBuildOptsCLI
                                    AGOTargets -> []
                                    AGOLocals -> [])

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
-- * Registers the library and copies the built executables into the
--   local install directory. Note that this is literally invoking Cabal
--   with @copy@, and not the copying done by @stack install@ - that is
--   handled by 'copyExecutables'.
singleBuild :: forall env. (HasEnvConfig env, HasRunner env)
            => ActionContext
            -> ExecuteEnv
            -> Task
            -> InstalledMap
            -> Bool             -- ^ Is this a final build?
            -> Maybe NamedComponent
            -- ^ If given, this triggers a component based build of the specified component
            -> RIO env ()
singleBuild ac@ActionContext {..} ee@ExecuteEnv {..} task@Task {..} installedMap isFinalBuild maybeComp = do
    (allDepsMap, cache) <- getConfigCache ee task installedMap enableTests enableBenchmarks (isNothing maybeComp)
    minstalled <- installIfNotPrecompiled allDepsMap cache
    case minstalled of
        Nothing -> return ()
        Just installed -> do
            writeFlagCache installed cache
            liftIO $ atomically $ modifyTVar eeGhcPkgIds $ Map.insert taskProvides installed
  where
    installIfNotPrecompiled :: Map PackageIdentifier GhcPkgId -> ConfigCache -> RIO env (Maybe Installed)
    installIfNotPrecompiled allDepsMap confCache = do
        mprecompiled <- getPrecompiled taskType eeBaseConfigOpts confCache
        case mprecompiled of
            Just precompiled -> copyPreCompiled pname ee task precompiled
            Nothing -> do
                mcurator <- view $ buildConfigL.to bcCurator
                realConfigAndBuild confCache mcurator allDepsMap 
    pname = pkgName taskProvides
    doHaddock mcurator package
                      = taskBuildHaddock &&
                        not isFinalBuild &&
                        -- Works around haddock failing on bytestring-builder since it has no modules
                        -- when bytestring is new enough.
                        packageHasExposedModules package &&
                        -- Special help for the curator tool to avoid haddocks that are known to fail
                        maybe True (Set.notMember pname . curatorSkipHaddock) mcurator
    expectHaddockFailure mcurator =
        maybe False (Set.member pname . curatorExpectHaddockFailure) mcurator
    fulfillHaddockExpectations mcurator action | expectHaddockFailure mcurator = do
        eres <- tryAny $ action KeepOpen
        case eres of
          Right () -> logWarn $ fromString (packageNameString pname) <> ": unexpected Haddock success"
          Left _ -> return ()
    fulfillHaddockExpectations _ action = do
        action CloseOnException

    buildingFinals = isFinalBuild || taskAllInOne
    enableTests = buildingFinals && any isCTest (taskComponents task)
    enableBenchmarks = buildingFinals && any isCBench (taskComponents task)

    annSuffix executableBuildStatuses = if result == "" then "" else " (" <> result <> ")"
      where
        result = T.intercalate " + " $ concat
            [ ["lib" | taskAllInOne && hasLib]
            , ["internal-lib" | taskAllInOne && hasSubLib]
            , ["exe" | taskAllInOne && hasExe]
            , ["test" | enableTests]
            , ["bench" | enableBenchmarks]
            ]
        (hasLib, hasSubLib, hasExe) = case taskType of
            TTLocalMutable lp ->
              let package = lpPackage lp
                  hasLibrary =
                    case packageLibraries package of
                      NoLibraries -> False
                      HasLibraries _ -> True
                  hasSubLibrary = not . Set.null $ packageInternalLibraries package
                  hasExecutables = not . Set.null $ exesToBuild executableBuildStatuses lp
               in (hasLibrary, hasSubLibrary, hasExecutables)
            -- This isn't true, but we don't want to have this info for
            -- upstream deps.
            _ -> (False, False, False)

    realConfigAndBuild cache mcurator allDepsMap = withSingleContext ac ee task allDepsMap Nothing
        $ \package cabalfp pkgDir cabal0 announce _outputType -> do
            let cabal = cabal0 CloseOnException
            executableBuildStatuses <- getExecutableBuildStatuses package pkgDir
            when (not (cabalIsSatisfied executableBuildStatuses) && taskIsTarget task)
                 (logInfo
                      ("Building all executables for `" <> fromString (packageNameString (packageName package)) <>
                       "' once. After a successful build of all of them, only specified executables will be rebuilt."))
            -- announce (fromString $ show task)
            _neededConfig <- ensureConfig cache pkgDir ee (announce ("configure" <> RIO.display (annSuffix executableBuildStatuses))) cabal cabalfp task maybeComp
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
                _ -> fulfillCuratorBuildExpectations pname mcurator enableTests enableBenchmarks Nothing $
                     Just <$> realBuild cache package pkgDir cabal0 announce executableBuildStatuses

    initialBuildSteps executableBuildStatuses cabal announce = do
        announce ("initial-build-steps" <> RIO.display (annSuffix executableBuildStatuses))
        cabal KeepTHLoading ["repl", "stack-initial-build-steps"]

    realBuild
        :: ConfigCache
        -> Package
        -> Path Abs Dir
        -> (KeepOutputOpen -> ExcludeTHLoading -> [String] -> RIO env ())
        -> (Utf8Builder -> RIO env ())
        -> Map Text ExecutableBuildStatus
        -> RIO env Installed
    realBuild cache package pkgDir cabal0 announce executableBuildStatuses = do
        let cabal = cabal0 CloseOnException
        wc <- view $ actualCompilerVersionL.whichCompilerL

        markExeNotInstalled (taskLocation task) taskProvides
        case taskType of
            TTLocalMutable lp -> do
                when enableTests $ setTestStatus pkgDir TSUnknown
                caches <- runMemoizedWith $ lpNewBuildCaches lp
                mapM_ (uncurry (writeBuildCache pkgDir))
                      (Map.toList caches)
            TTRemotePackage{} -> return ()

        -- FIXME: only output these if they're in the build plan.

        let postBuildCheck _succeeded = do
                mlocalWarnings <- case taskType of
                    TTLocalMutable lp -> do
                        warnings <- checkForUnlistedFiles taskType pkgDir
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
                      indent 4 (mconcat $ intersperse line $ map (style Good . fromString . C.display) modules)
                forM_ mlocalWarnings $ \(cabalfp, warnings) -> do
                    unless (null warnings) $ prettyWarn $
                        "The following modules should be added to exposed-modules or other-modules in" <+>
                        pretty cabalfp <> ":" <> line <>
                        indent 4 (mconcat $ intersperse line $ map showModuleWarning warnings) <>
                        line <> line <>
                        "Missing modules in the cabal file are likely to cause undefined reference errors from the linker, along with other problems."

        () <- announce ("build" <> RIO.display (annSuffix executableBuildStatuses))
        config <- view configL
        extraOpts <- extraBuildOptions wc eeBuildOpts
        let stripTHLoading
                | configHideTHLoading config = ExcludeTHLoading
                | otherwise                  = KeepTHLoading
        let moreOpt
                | isNothing maybeComp = (++ extraOpts) $
                    case (taskType, taskAllInOne, isFinalBuild) of
                        (_, True, True) -> error "Invariant violated: cannot have an all-in-one build that also has a final build step."
                        (TTLocalMutable lp, False, False) -> primaryComponentOptions executableBuildStatuses lp
                        (TTLocalMutable lp, False, True) -> finalComponentOptions lp
                        (TTLocalMutable _, True, False) -> [] -- primaryComponentOptions executableBuildStatuses lp ++ finalComponentOptions lp
                        (TTRemotePackage{}, _, _) -> []
                | otherwise = []
        cabal stripTHLoading (("build" :) moreOpt)
          `catch` \ex -> case ex of
              CabalExitedUnsuccessfully{} -> postBuildCheck False >> throwM ex
              _ -> throwM ex
        postBuildCheck True

        mcurator <- view $ buildConfigL.to bcCurator
        when (doHaddock mcurator package) $ do
            announce "haddock"
            sourceFlag <- if not (boptsHaddockHyperlinkSource eeBuildOpts) then return [] else do
                -- See #2429 for why the temp dir is used
                ec
                  <- withWorkingDir (toFilePath eeTempDir)
                   $ proc "haddock" ["--hyperlinked-source"]
                   $ \pc -> withProcessWait
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

            -- For GHC 8.4 and later, provide the --quickjump option.
            actualCompiler <- view actualCompilerVersionL
            let quickjump =
                  case actualCompiler of
                    ACGhc ghcVer
                      | ghcVer >= mkVersion [8, 4] -> ["--haddock-option=--quickjump"]
                    _ -> []

            fulfillHaddockExpectations mcurator $ \keep -> cabal0 keep KeepTHLoading $ concat
                [ ["haddock", "--html", "--hoogle", "--html-location=../$pkg-$version/"]
                , sourceFlag
                , ["--internal" | boptsHaddockInternal eeBuildOpts]
                , [ "--haddock-option=" <> opt
                  | opt <- hoAdditionalArgs (boptsHaddockOpts eeBuildOpts) ]
                , quickjump
                ]

        let hasLibrary =
              case packageLibraries package of
                NoLibraries -> False
                HasLibraries _ -> True
            packageHasComponentSet f = not $ Set.null $ f package
            hasInternalLibrary = packageHasComponentSet packageInternalLibraries
            hasExecutables = packageHasComponentSet packageExes
            shouldCopy = not isFinalBuild && (hasLibrary || hasInternalLibrary || hasExecutables)
        when shouldCopy $ withMVar eeInstallLock $ \() -> do
            announce "copy/register"
            eres <- try $ cabal KeepTHLoading ["copy"]
            case eres of
                Left err@CabalExitedUnsuccessfully{} ->
                    throwM $ CabalCopyFailed (packageBuildType package == C.Simple) (show err)
                _ -> return ()
            when hasLibrary $ cabal KeepTHLoading ["register"]

        -- copy ddump-* files
        case T.unpack <$> boptsDdumpDir eeBuildOpts of
          Just ddumpPath | buildingFinals && not (null ddumpPath) -> do
            distDir <- distRelativeDir
            ddumpDir <- parseRelDir ddumpPath

            logDebug $ fromString ("ddump-dir: " <> toFilePath ddumpDir)
            logDebug $ fromString ("dist-dir: " <> toFilePath distDir)

            runConduitRes
              $ CF.sourceDirectoryDeep False (toFilePath distDir)
             .| CL.filter (isInfixOf ".dump-")
             .| CL.mapM_ (\src -> liftIO $ do
                  parentDir <- parent <$> parseRelDir src
                  destBaseDir <- (ddumpDir </>) <$> stripProperPrefix distDir parentDir
                  -- exclude .stack-work dir
                  unless (".stack-work" `isInfixOf` toFilePath destBaseDir) $ do
                    ensureDir destBaseDir
                    src' <- parseRelFile src
                    copyFile src' (destBaseDir </> filename src'))
          _ -> pure ()

        let (installedPkgDb, installedDumpPkgsTVar) =
                case taskLocation task of
                    Snap ->
                         ( bcoSnapDB eeBaseConfigOpts
                         , eeSnapshotDumpPkgs )
                    Local ->
                        ( bcoLocalDB eeBaseConfigOpts
                        , eeLocalDumpPkgs )
        let ident = PackageIdentifier (packageName package) (packageVersion package)
        -- only return the sublibs to cache them if we also cache the main lib (that is, if it exists)
        (mpkgid, sublibsPkgIds) <- case packageLibraries package of
            HasLibraries _ -> do
                sublibsPkgIds <- fmap catMaybes $
                  forM (Set.toList $ packageInternalLibraries package) $ \sublib -> do
                    -- z-haddock-library-z-attoparsec for internal lib attoparsec of haddock-library
                    let sublibName = T.concat ["z-", T.pack $ packageNameString $ packageName package, "-z-", sublib]
                    case parsePackageName $ T.unpack sublibName of
                      Nothing -> return Nothing -- invalid lib, ignored
                      Just subLibName -> loadInstalledPkg [installedPkgDb] installedDumpPkgsTVar subLibName

                mpkgid <- loadInstalledPkg [installedPkgDb] installedDumpPkgsTVar (packageName package)
                case mpkgid of
                    Nothing -> throwM $ Couldn'tFindPkgId $ packageName package
                    Just pkgid -> return (Library ident pkgid Nothing, sublibsPkgIds)
            NoLibraries -> do
                markExeInstalled (taskLocation task) taskProvides -- TODO unify somehow with writeFlagCache?
                return (Executable ident, []) -- don't return sublibs in this case

        case taskType of
            TTRemotePackage Immutable _ loc ->
              writePrecompiledCache
                eeBaseConfigOpts
                loc
                (configCacheOpts cache)
                (configCacheHaddock cache)
                (configCacheDeps cache)
                mpkgid sublibsPkgIds (packageExes package)
            _ -> return ()

        case taskType of
            -- For packages from a package index, pkgDir is in the tmp
            -- directory. We eagerly delete it if no other tasks
            -- require it, to reduce space usage in tmp (#3018).
            TTRemotePackage{} -> do
                let remaining = filter (\(ActionId x _) -> x == taskProvides) (Set.toList acRemaining)
                when (null remaining) $ removeDirRecur pkgDir
            TTLocalMutable{} -> return ()

        return mpkgid

-- | Check if any unlisted files have been found, and add them to the build cache.
checkForUnlistedFiles :: HasEnvConfig env => TaskType -> Path Abs Dir -> RIO env [PackageWarning]
checkForUnlistedFiles (TTLocalMutable lp) pkgDir = do
    caches <- runMemoizedWith $ lpNewBuildCaches lp
    (addBuildCache,warnings) <-
        addUnlistedToBuildCache
            (lpPackage lp)
            (lpCabalFile lp)
            (lpComponents lp)
            caches
    forM_ (M.toList addBuildCache) $ \(component, newToCache) -> do
        let cache = Map.findWithDefault Map.empty component caches
        writeBuildCache pkgDir component $
            Map.unions (cache : newToCache)
    return warnings
checkForUnlistedFiles TTRemotePackage{} _ = return []

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
    (allDepsMap, _cache) <- getConfigCache ee task installedMap True False True
    mcurator <- view $ buildConfigL.to bcCurator
    let pname = pkgName $ taskProvides task
        expectFailure = expectTestFailure pname mcurator
    withSingleContext ac ee task allDepsMap (Just "test") $ \package _cabalfp pkgDir _cabal announce outputType -> do
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
                    status <- getTestStatus pkgDir
                    case status of
                      TSSuccess -> do
                        unless (null testsToRun) $ announce "skipping already passed test"
                        return False
                      TSFailure
                        | expectFailure -> do
                            announce "skipping already failed test that's expected to fail"
                            return False
                        | otherwise -> do
                            announce "rerunning previously failed test"
                            return True
                      TSUnknown -> return True

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
                -- in Stack.Package.packageFromPackageDescription we filter out
                -- package itself of any dependencies so any tests requiring loading
                -- of their own package library will fail
                -- so to prevent this we return it back here but unfortunately unconditionally
                installed <- case Map.lookup pname installedMap of
                  Just (_, installed) -> pure $ Just installed
                  Nothing -> do
                    idMap <- liftIO $ readTVarIO (eeGhcPkgIds ee)
                    pure $ Map.lookup (taskProvides task) idMap
                let pkgGhcIdList = case installed of
                                       Just (Library _ ghcPkgId _) -> [ghcPkgId]
                                       _ -> []
                -- doctest relies on template-haskell in QuickCheck-based tests
                thGhcId <- case find ((== "template-haskell") . pkgName . dpPackageIdent. snd)
                                (Map.toList $ eeGlobalDumpPkgs ee) of
                  Just (ghcId, _) -> return ghcId
                  Nothing -> error "template-haskell is a wired-in GHC boot library but it wasn't found"
                -- env variable GHC_ENVIRONMENT is set for doctest so module names for
                -- packages with proper dependencies should no longer get ambiguous
                -- see e.g. https://github.com/doctest/issues/119
                -- also we set HASKELL_DIST_DIR to a package dist directory so
                -- doctest will be able to load modules autogenerated by Cabal
                let setEnv f pc = modifyEnvVars pc $ \envVars ->
                      Map.insert "HASKELL_DIST_DIR" (T.pack $ toFilePath buildDir) $
                      Map.insert "GHC_ENVIRONMENT" (T.pack f) envVars
                    fp = toFilePath $ eeTempDir ee </> testGhcEnvRelFile
                    snapDBPath = toFilePathNoTrailingSep (bcoSnapDB $ eeBaseConfigOpts ee)
                    localDBPath = toFilePathNoTrailingSep (bcoLocalDB $ eeBaseConfigOpts ee)
                    ghcEnv =
                        "clear-package-db\n" <>
                        "global-package-db\n" <>
                        "package-db " <> fromString snapDBPath <> "\n" <>
                        "package-db " <> fromString localDBPath <> "\n" <>
                        foldMap (\ghcId -> "package-id " <> RIO.display (unGhcPkgId ghcId) <> "\n")
                            (pkgGhcIdList ++ thGhcId:M.elems allDepsMap)
                writeFileUtf8Builder fp ghcEnv
                menv <- liftIO $ setEnv fp =<< configProcessContextSettings config EnvSettings
                    { esIncludeLocals = taskLocation task == Local
                    , esIncludeGhcPackagePath = True
                    , esStackExe = True
                    , esLocaleUtf8 = False
                    , esKeepGhcRts = False
                    }
                let emptyResult = Map.singleton testName Nothing
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
                        announce $ "test (suite: " <> RIO.display testName <> RIO.display argsDisplay <> ")"

                        -- Clear "Progress: ..." message before
                        -- redirecting output.
                        case outputType of
                          OTConsole _ -> do
                            logStickyDone ""
                            liftIO $ hFlush stdout
                            liftIO $ hFlush stderr
                          OTLogFile _ _ -> pure ()

                        let output =
                                case outputType of
                                    OTConsole Nothing -> Nothing <$ inherit
                                    OTConsole (Just prefix) -> fmap
                                      (\src -> Just $ runConduit $ src .|
                                               CT.decodeUtf8Lenient .|
                                               CT.lines .|
                                               CL.map stripCR .|
                                               CL.mapM_ (\t -> logInfo $ prefix <> RIO.display t))
                                      createSource
                                    OTLogFile _ h -> Nothing <$ useHandleOpen h
                            optionalTimeout action
                                | Just maxSecs <- toMaximumTimeSeconds topts, maxSecs > 0 = do
                                    timeout (maxSecs * 1000000) action
                                | otherwise = Just <$> action

                        mec <- withWorkingDir (toFilePath pkgDir) $
                          optionalTimeout $ proc (toFilePath exePath) args $ \pc0 -> do
                            stdinBS <-
                              if isTestTypeLib
                                then do
                                  logPath <- buildLogPath package (Just stestName)
                                  ensureDir (parent logPath)
                                  pure $ BL.fromStrict
                                       $ encodeUtf8 $ fromString $
                                       show (logPath, mkUnqualComponentName (T.unpack testName))
                                else pure mempty
                            let pc = setStdin (byteStringInput stdinBS)
                                   $ setStdout output
                                   $ setStderr output
                                     pc0
                            withProcessWait pc $ \p -> do
                              case (getStdout p, getStderr p) of
                                (Nothing, Nothing) -> pure ()
                                (Just x, Just y) -> concurrently_ x y
                                (x, y) -> assert False $ concurrently_ (fromMaybe (pure ()) x) (fromMaybe (pure ()) y)
                              waitExitCode p
                        -- Add a trailing newline, incase the test
                        -- output didn't finish with a newline.
                        case outputType of
                          OTConsole Nothing -> logInfo ""
                          _ -> pure ()
                        -- Move the .tix file out of the package
                        -- directory into the hpc work dir, for
                        -- tidiness.
                        when needHpc $
                            updateTixFile (packageName package) tixPath testName'
                        let announceResult result = announce $ "Test suite " <> RIO.display testName <> " " <> result
                        case mec of
                            Just ExitSuccess -> do
                                announceResult "passed"
                                return Map.empty
                            Nothing -> do
                                announceResult "timed out"
                                if expectFailure
                                then return Map.empty
                                else return $ Map.singleton testName Nothing
                            Just ec -> do
                                announceResult "failed"
                                if expectFailure
                                then return Map.empty
                                else return $ Map.singleton testName (Just ec)
                    else do
                        unless expectFailure $ logError $ displayShow $ TestSuiteExeMissing
                            (packageBuildType package == C.Simple)
                            exeName
                            (packageNameString (packageName package))
                            (T.unpack testName)
                        return emptyResult

            when needHpc $ do
                let testsToRun' = map f testsToRun
                    f tName =
                        case Map.lookup tName (packageTests package) of
                            Just C.TestSuiteLibV09{} -> tName <> "Stub"
                            _ -> tName
                generateHpcReport pkgDir package testsToRun'

            bs <- liftIO $
                case outputType of
                    OTConsole _ -> return ""
                    OTLogFile logFile h -> do
                        hClose h
                        S.readFile $ toFilePath logFile

            let succeeded = Map.null errs
            unless (succeeded || expectFailure) $ throwM $ TestSuiteFailure
                (taskProvides task)
                errs
                (case outputType of
                   OTLogFile fp _ -> Just fp
                   OTConsole _ -> Nothing)
                bs

            setTestStatus pkgDir $ if succeeded then TSSuccess else TSFailure

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
    (allDepsMap, _cache) <- getConfigCache ee task installedMap False True True
    withSingleContext ac ee task allDepsMap (Just "bench") $ \_package _cabalfp _pkgDir cabal announce _outputType -> do
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
            cabal CloseOnException KeepTHLoading ("bench" : args)

data ExcludeTHLoading = ExcludeTHLoading | KeepTHLoading
data ConvertPathsToAbsolute = ConvertPathsToAbsolute | KeepPathsAsIs
-- | special marker for expected failures in curator builds, using those
-- we need to keep log handle open as build continues further even after a failure
data KeepOutputOpen = KeepOpen | CloseOnException deriving Eq

-- | Strip Template Haskell "Loading package" lines and making paths absolute.
mungeBuildOutput :: forall m. MonadIO m
                 => ExcludeTHLoading       -- ^ exclude TH loading?
                 -> ConvertPathsToAbsolute -- ^ convert paths to absolute?
                 -> Path Abs Dir           -- ^ package's root directory
                 -> ActualCompiler         -- ^ compiler we're building with
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
        | getGhcVersion compilerVer >= mkVersion [7, 8] = doNothing
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

-- | Whether to prefix log lines with timestamps.
data PrefixWithTimestamps = PrefixWithTimestamps | WithoutTimestamps

-- | Write stream of lines to handle, but adding timestamps.
sinkWithTimestamps :: MonadIO m => PrefixWithTimestamps -> Handle -> ConduitT ByteString Void m ()
sinkWithTimestamps prefixWithTimestamps h =
    case prefixWithTimestamps of
        PrefixWithTimestamps ->
            CB.lines .| CL.mapM addTimestamp .| CL.map (<> "\n") .| sinkHandle h
        WithoutTimestamps -> sinkHandle h
  where
    addTimestamp theLine = do
        now <- liftIO getZonedTime
        pure (formatZonedTimeForLog now <> " " <> theLine)

-- | Format a time in ISO8601 format. We choose ZonedTime over UTCTime
-- because a user expects to see logs in their local time, and would
-- be confused to see UTC time. Stack's debug logs also use the local
-- time zone.
formatZonedTimeForLog :: ZonedTime -> ByteString
formatZonedTimeForLog = S8.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6Q"

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
    fp1 = dir </> relFileSetupHs
    fp2 = dir </> relFileSetupLhs

-- Do not pass `-hpcdir` as GHC option if the coverage is not enabled.
-- This helps running stack-compiled programs with dynamic interpreters like `hint`.
-- Cfr: https://github.com/commercialhaskell/stack/issues/997
extraBuildOptions :: (HasEnvConfig env, HasRunner env)
                  => WhichCompiler -> BuildOpts -> RIO env [String]
extraBuildOptions wc bopts = do
    colorOpt <- appropriateGhcColorFlag
    let optsFlag = compilerOptionsCabalFlag wc
        baseOpts = maybe "" (" " ++) colorOpt
    if toCoverage (boptsTestOpts bopts)
      then do
        hpcIndexDir <- toFilePathNoTrailingSep <$> hpcRelativeDir
        return [optsFlag, "-hpcdir " ++ hpcIndexDir ++ baseOpts]
      else
        return [optsFlag, baseOpts]

-- Library, internal and foreign libraries and executable build components.
primaryComponentOptions :: Map Text ExecutableBuildStatus -> LocalPackage -> [String]
primaryComponentOptions executableBuildStatuses lp =
      -- TODO: get this information from target parsing instead,
      -- which will allow users to turn off library building if
      -- desired
      (case packageLibraries package of
         NoLibraries -> []
         HasLibraries names ->
             map T.unpack
           $ T.append "lib:" (T.pack (packageNameString (packageName package)))
           : map (T.append "flib:") (Set.toList names)) ++
      map (T.unpack . T.append "lib:") (Set.toList $ packageInternalLibraries package) ++
      map (T.unpack . T.append "exe:") (Set.toList $ exesToBuild executableBuildStatuses lp)
  where
    package = lpPackage lp

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
    if cabalIsSatisfied executableBuildStatuses && lpShouldBeBuilt lp
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

expectTestFailure :: PackageName -> Maybe Curator -> Bool
expectTestFailure pname mcurator =
    maybe False (Set.member pname . curatorExpectTestFailure) mcurator

expectBenchmarkFailure :: PackageName -> Maybe Curator -> Bool
expectBenchmarkFailure pname mcurator =
    maybe False (Set.member pname . curatorExpectBenchmarkFailure) mcurator

fulfillCuratorBuildExpectations ::
       (HasLogFunc env, HasCallStack)
    => PackageName
    -> Maybe Curator
    -> Bool
    -> Bool
    -> b
    -> RIO env b
    -> RIO env b
fulfillCuratorBuildExpectations pname mcurator enableTests _ defValue action | enableTests &&
                                                                          expectTestFailure pname mcurator = do
    eres <- tryAny action
    case eres of
      Right res -> do
          logWarn $ fromString (packageNameString pname) <> ": unexpected test build success"
          return res
      Left _ -> return defValue
fulfillCuratorBuildExpectations pname mcurator _ enableBench defValue action | enableBench &&
                                                                          expectBenchmarkFailure pname mcurator = do
    eres <- tryAny action
    case eres of
      Right res -> do
          logWarn $ fromString (packageNameString pname) <> ": unexpected benchmark build success"
          return res
      Left _ -> return defValue
fulfillCuratorBuildExpectations _ _ _ _ _ action = do
    action
