{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}

{-|
Module      : Stack.Build.Execute
Description : Perform a build.
License     : BSD-3-Clause

Perform a build.
-}

module Stack.Build.Execute
  ( printPlan
  , preFetch
  , executePlan
  -- * Running Setup.hs
  , ExcludeTHLoading (..)
  , KeepOutputOpen (..)
  ) where

import           Control.Concurrent.Execute
                   ( Action (..), ActionId (..), ActionType (..)
                   , Concurrency (..), runActions
                   )
import           Control.Concurrent.STM ( check )
import           Control.Monad.Extra ( whenJust )
import qualified Data.List as L
import           Data.List.Split ( chunksOf )
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Tuple ( swap )
import           Distribution.System ( OS (..), Platform (..) )
import           Path ( (</>),  parent )
import           Path.CheckInstall ( warnInstallSearchPathIssues )
import           Path.Extra ( forgivingResolveFile, rejectMissingFile )
import           Path.IO ( ensureDir )
import           RIO.NonEmpty ( nonEmpty )
import qualified RIO.NonEmpty as NE
import           RIO.Process ( HasProcessContext (..), proc, runProcess_ )
import           Stack.Build.ExecuteEnv ( ExecuteEnv (..), withExecuteEnv )
import           Stack.Build.ExecutePackage
                   ( singleBench, singleBuild, singleTest )
import           Stack.Build.Haddock
                   ( generateDepsHaddockIndex
                   , generateLocalHaddockForHackageArchives
                   , generateLocalHaddockIndex, generateSnapHaddockIndex
                   , openHaddocksInBrowser
                   )
import           Stack.Constants ( bindirSuffix )
import           Stack.Coverage
                   ( deleteHpcReports, generateHpcMarkupIndex
                   , generateHpcUnifiedReport
                   )
import           Stack.GhcPkg ( unregisterGhcPkgIds )
import           Stack.Prelude
import           Stack.Types.Build
                   ( ExcludeTHLoading (..), KeepOutputOpen (..), Plan (..)
                   , Task (..), TaskConfigOpts (..), TaskType (..), taskLocation
                   , taskProvides
                   )
import           Stack.Types.Build.Exception ( BuildPrettyException (..) )
import           Stack.Types.BuildOpts
                   ( BenchmarkOpts (..), BuildOpts (..), TestOpts (..) )
import           Stack.Types.BuildOptsCLI ( BuildOptsCLI (..) )
import           Stack.Types.BuildOptsMonoid ( ProgressBarFormat (..) )
import           Stack.Types.CompilerPaths ( HasCompiler (..), getGhcPkgExe )
import           Stack.Types.ComponentUtils
                   ( StackUnqualCompName, unqualCompToString )
import           Stack.Types.Config ( Config (..), HasConfig (..), buildOptsL )
import           Stack.Types.ConfigureOpts ( BaseConfigOpts (..) )
import           Stack.Types.DumpPackage ( DumpPackage (..) )
import           Stack.Types.EnvConfig
                   ( HasEnvConfig (..), bindirCompilerTools
                   , installationRootDeps, installationRootLocal
                   , packageDatabaseLocal
                   )
import           Stack.Types.EnvSettings ( EnvSettings (..) )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.Installed
                   ( InstallLocation (..), InstalledMap
                   , installedPackageIdentifier
                   )
import           Stack.Types.NamedComponent
                   ( NamedComponent, benchComponents, testComponents )
import           Stack.Types.Package
                   ( LocalPackage (..), Package (..), packageIdentifier )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Runner ( terminalL, viewExecutablePath )
import           Stack.Types.SourceMap ( Target )
import qualified System.Directory as D
import qualified System.FilePath as FP

-- | Fetch the packages necessary for a build, for example in combination with
-- a dry run.
preFetch :: HasEnvConfig env => Plan -> RIO env ()
preFetch plan
  | Set.null pkgLocs = logDebug "Nothing to fetch"
  | otherwise = do
      logDebug $
           "Prefetching: "
        <> mconcat (L.intersperse ", " (display <$> Set.toList pkgLocs))
      fetchPackages pkgLocs
 where
  pkgLocs = Set.unions $ map toPkgLoc $ Map.elems plan.tasks

  toPkgLoc task =
    case task.taskType of
      TTLocalMutable{} -> Set.empty
      TTRemotePackage _ _ pkgloc -> Set.singleton pkgloc

-- | Print a description of build plan for human consumption.
printPlan :: HasEnvConfig env => Plan -> RIO env ()
printPlan plan = do
  case Map.elems plan.unregisterLocal of
    [] -> prettyInfo $
               flow "No packages would be unregistered."
            <> line
    xs -> do
      let unregisterMsg (ident, reason) = fillSep $
              fromString (packageIdentifierString ident)
            : [ parens $ flow (T.unpack reason) | not $ T.null reason ]
      prettyInfo $
           flow "Would unregister locally:"
        <> line
        <> bulletedList (map unregisterMsg xs)
        <> line

  case Map.elems plan.tasks of
    [] -> prettyInfo $
               flow "Nothing to build."
            <> line
    xs -> do
      prettyInfo $
           flow "Would build:"
        <> line
        <> bulletedList (map displayTask xs)
        <> line

  buildOpts <- view buildOptsL
  let hasTests = not . Set.null . testComponents . taskComponents
      hasBenches = not . Set.null . benchComponents . taskComponents
      tests = Map.elems $ Map.filter hasTests plan.finals
      benches = Map.elems $ Map.filter hasBenches plan.finals
      runTests = buildOpts.testOpts.runTests
      runBenchmarks = buildOpts.benchmarkOpts.runBenchmarks

  unless (null tests) $
    if runTests
      then
        prettyInfo $
             flow "Would test:"
          <> line
          <> bulletedList (map displayTask tests)
          <> line
      else
        prettyInfo $
             fillSep
               [ flow "Would not test, as running disabled by"
               , style Shell "--no-run-tests"
               , "flag."
               ]
          <> line

  unless (null benches) $
    if runBenchmarks
      then
        prettyInfo $
             flow "Would benchmark:"
          <> line
          <> bulletedList (map displayTask benches)
          <> line
      else
        prettyInfo $
             fillSep
               [ flow "Would not benchmark, as running disabled by"
               , style Shell "--no-run-benchmarks"
               , "flag."
               ]
          <> line

  case Map.toList plan.installExes of
    [] -> prettyInfo $
               flow "No executables to be installed."
            <> line
    xs -> do
      let executableMsg (name, loc) = fillSep $
              fromString (unqualCompToString name)
            : "from"
            : ( case loc of
                  Snap -> "snapshot" :: StyleDoc
                  Local -> "local" :: StyleDoc
              )
            : ["database."]
      prettyInfo $
           flow "Would install executables:"
        <> line
        <> bulletedList (map executableMsg xs)
        <> line

-- | For a dry run
displayTask :: Task -> StyleDoc
displayTask task = fillSep $
     [ fromString (packageIdentifierString (taskProvides task)) <> ":"
     ,    "database="
       <> ( case taskLocation task of
              Snap -> "snapshot" :: StyleDoc
              Local -> "local" :: StyleDoc
          )
       <> ","
     ,    "source="
       <> ( case task.taskType of
              TTLocalMutable lp -> pretty $ parent lp.cabalFP
              TTRemotePackage _ _ pl -> fromString $ T.unpack $ textDisplay pl
          )
       <> if Set.null missing
            then mempty
            else ","
     ]
  <> [ fillSep $
           "after:"
         : mkNarrativeList Nothing False
             (map fromPackageId (Set.toList missing) :: [StyleDoc])
     | not $ Set.null missing
     ]
 where
  missing = task.configOpts.missing

-- | Perform the actual plan
executePlan ::
     HasEnvConfig env
  => BuildOptsCLI
  -> BaseConfigOpts
  -> [LocalPackage]
  -> [DumpPackage]
     -- ^ global packages
  -> [DumpPackage]
     -- ^ snapshot packages
  -> [DumpPackage]
     -- ^ project packages and local extra-deps
  -> InstalledMap
  -> Map PackageName Target
  -> Plan
  -> RIO env ()
executePlan
    boptsCli
    baseConfigOpts
    locals
    globalPackages
    snapshotPackages
    localPackages
    installedMap
    targets
    plan
  = do
    logDebug "Executing the build plan"
    bopts <- view buildOptsL
    withExecuteEnv
      bopts
      boptsCli
      baseConfigOpts
      locals
      globalPackages
      snapshotPackages
      localPackages
      mlargestPackageName
      (executePlan' installedMap targets plan)

    copyExecutables plan.installExes

    config <- view configL
    menv' <- liftIO $ config.processContextSettings EnvSettings
               { includeLocals = True
               , includeGhcPackagePath = True
               , stackExe = True
               , localeUtf8 = False
               , keepGhcRts = False
               }
    withProcessContext menv' $
      forM_ boptsCli.exec $ \(cmd, args) ->
      proc cmd args runProcess_
 where
  mlargestPackageName =
    Set.lookupMax $
    Set.map (length . packageNameString) $
    Map.keysSet plan.tasks <> Map.keysSet plan.finals

copyExecutables ::
     HasEnvConfig env
  => Map StackUnqualCompName InstallLocation
  -> RIO env ()
copyExecutables exes | Map.null exes = pure ()
copyExecutables exes = do
  snapBin <- (</> bindirSuffix) <$> installationRootDeps
  localBin <- (</> bindirSuffix) <$> installationRootLocal
  compilerSpecific <- (.installCompilerTool) <$> view buildOptsL
  destDir <- if compilerSpecific
               then bindirCompilerTools
               else view $ configL . to (.localBin)
  ensureDir destDir

  destDir' <- liftIO . D.canonicalizePath . toFilePath $ destDir

  platform <- view platformL
  let ext =
        case platform of
          Platform _ Windows -> ".exe"
          _ -> ""

  -- needed for windows, see below
  currExe <- toFilePath <$> viewExecutablePath

  installed <- forMaybeM (Map.toList exes) $ \(name, loc) -> do
    let strName = unqualCompToString name
        bindir =
            case loc of
                Snap -> snapBin
                Local -> localBin
    mfp <- forgivingResolveFile bindir (strName ++ ext)
      >>= rejectMissingFile
    case mfp of
      Nothing -> do
        prettyWarnL
          [ flow "Couldn't find executable"
          , style Current (fromString strName)
          , flow "in directory"
          , pretty bindir <> "."
          ]
        pure Nothing
      Just file -> do
        let destFile = destDir' FP.</> strName ++ ext
        prettyInfoL
          [ flow "Copying from"
          , pretty file
          , "to"
          , style File (fromString destFile) <> "."
          ]

        liftIO $ case platform of
          Platform _ Windows | FP.equalFilePath destFile currExe ->
              windowsRenameCopy (toFilePath file) destFile
          _ -> D.copyFile (toFilePath file) destFile
        pure $ Just (strName ++ ext)

  unless (null installed) $ do
    prettyInfo $
         fillSep
           [ flow "Copied executables to"
           , pretty destDir <> ":"
           ]
      <> line
      <> bulletedList
           (map fromString installed :: [StyleDoc])
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
executePlan' ::
     HasEnvConfig env
  => InstalledMap
  -> Map PackageName Target
  -> Plan
  -> ExecuteEnv
  -> RIO env ()
executePlan' installedMap0 targets plan ee = do
  config <- view configL
  let !buildOpts = ee.buildOpts
      !testOpts = buildOpts.testOpts
      !benchmarkOpts = buildOpts.benchmarkOpts
      runTests = testOpts.runTests
      runBenchmarks = benchmarkOpts.runBenchmarks
      noNotifyIfNoRunTests = not config.notifyIfNoRunTests
      noNotifyIfNoRunBenchmarks = not config.notifyIfNoRunBenchmarks
      hasTests = not . Set.null . testComponents . taskComponents
      hasBenches = not . Set.null . benchComponents . taskComponents
      tests = Map.elems $ Map.filter hasTests plan.finals
      benches = Map.elems $ Map.filter hasBenches plan.finals
  when testOpts.coverage deleteHpcReports
  whenJust (nonEmpty $ Map.toList plan.unregisterLocal) $ \ids -> do
    localDB <- packageDatabaseLocal
    unregisterPackages localDB ids

  liftIO $ atomically $ modifyTVar' ee.localDumpPkgs $ \initMap ->
    foldl' (flip Map.delete) initMap $ Map.keys plan.unregisterLocal

  run <- askRunInIO

  -- If running tests concurrently with each other, then create an MVar
  -- which is empty while each test is being run.
  concurrentTests <- view $ configL . to (.concurrentTests)
  mtestLock <- if concurrentTests
                 then pure Nothing
                 else Just <$> liftIO (newMVar ())

  let actions = concatMap (toActions installedMap' mtestLock run ee) $
        Map.elems $ Map.merge
          (Map.mapMissing (\_ b -> (Just b, Nothing)))
          (Map.mapMissing (\_ f -> (Nothing, Just f)))
          (Map.zipWithMatched (\_ b f -> (Just b, Just f)))
          plan.tasks
          plan.finals
  threads <- view $ configL . to (.jobs)
  let keepGoing = fromMaybe
        (not (Map.null plan.finals))
        buildOpts.keepGoing
  terminal <- view terminalL
  terminalWidth <- view termWidthL
  unless (noNotifyIfNoRunTests || runTests || null tests) $
    prettyInfo $
      fillSep
        [ flow "All test running disabled by"
        , style Shell "--no-run-tests"
        , flow "flag. To mute this message in future, set"
        , style Shell (flow "notify-if-no-run-tests: false")
        , flow "in Stack's configuration."
        ]
  unless (noNotifyIfNoRunBenchmarks || runBenchmarks || null benches) $
    prettyInfo $
      fillSep
        [ flow "All benchmark running disabled by"
        , style Shell "--no-run-benchmarks"
        , flow "flag. To mute this message in future, set"
        , style Shell (flow "notify-if-no-run-benchmarks: false")
        , flow "in Stack's configuration."
        ]
  errs <- liftIO $ runActions threads keepGoing actions $
    \doneVar actionsVar -> do
      let total = length actions
          loop prev
            | prev == total =
                run $ logStickyDone
                  ( "Completed " <> display total <> " action(s).")
            | otherwise = do
                inProgress <- readTVarIO actionsVar
                let packageNames = map
                      (\(ActionId pkgID _) -> pkgName pkgID)
                      (toList inProgress)
                    nowBuilding :: [PackageName] -> Utf8Builder
                    nowBuilding []    = ""
                    nowBuilding names = mconcat $
                        ": "
                      : L.intersperse ", " (map fromPackageName names)
                    progressFormat = buildOpts.progressBar
                    progressLine prev' total' =
                         "Progress "
                      <> display prev' <> "/" <> display total'
                      <> if progressFormat == CountOnlyBar
                           then mempty
                           else nowBuilding packageNames
                    ellipsize n text =
                      if T.length text <= n || progressFormat /= CappedBar
                        then text
                        else T.take (n - 1) text <> "…"
                when (terminal && progressFormat /= NoBar) $
                  run $ logSticky $ display $ ellipsize terminalWidth $
                    utf8BuilderToText $ progressLine prev total
                done <- atomically $ do
                  done <- readTVar doneVar
                  check $ done /= prev
                  pure done
                loop done
      when (total > 1) $ loop 0
  when testOpts.coverage $ do
    generateHpcUnifiedReport
    generateHpcMarkupIndex
  unless (null errs) $
    prettyThrowM $ ExecutionFailure errs
  when buildOpts.buildHaddocks $ do
    if buildOpts.haddockForHackage
      then
        generateLocalHaddockForHackageArchives ee.locals
      else do
        snapshotDumpPkgs <- liftIO (readTVarIO ee.snapshotDumpPkgs)
        localDumpPkgs <- liftIO (readTVarIO ee.localDumpPkgs)
        generateLocalHaddockIndex ee.baseConfigOpts localDumpPkgs ee.locals
        generateDepsHaddockIndex
          ee.baseConfigOpts
          ee.globalDumpPkgs
          snapshotDumpPkgs
          localDumpPkgs
          ee.locals
        generateSnapHaddockIndex
          ee.baseConfigOpts
          ee.globalDumpPkgs
          snapshotDumpPkgs
        when buildOpts.openHaddocks $ do
          let planPkgs, localPkgs, installedPkgs, availablePkgs
                :: Map PackageName (PackageIdentifier, InstallLocation)
              planPkgs =
                Map.map (taskProvides &&& taskLocation) plan.tasks
              localPkgs =
                Map.fromList
                  [ (p.name, (packageIdentifier p, Local))
                  | p <- map (.package) ee.locals
                  ]
              installedPkgs =
                Map.map (swap . second installedPackageIdentifier) installedMap'
              availablePkgs = Map.unions [planPkgs, localPkgs, installedPkgs]
          openHaddocksInBrowser
            ee.baseConfigOpts
            availablePkgs
            (Map.keysSet targets)
 where
  installedMap' = Map.difference installedMap0
                $ Map.fromList
                $ map (\(ident, _) -> (pkgName ident, ()))
                $ Map.elems plan.unregisterLocal

unregisterPackages ::
     (HasCompiler env, HasPlatform env, HasProcessContext env, HasTerm env)
  => Path Abs Dir
  -> NonEmpty (GhcPkgId, (PackageIdentifier, Text))
  -> RIO env ()
unregisterPackages localDB ids = do
  let logReason ident reason =
        prettyInfoL
          (  [ fromString (packageIdentifierString ident) <> ":"
             , "unregistering"
             ]
          <> [ parens (flow $ T.unpack reason) | not $ T.null reason ]
          )
  -- GHC versions >= 8.2.1 support batch unregistering of packages. See
  -- https://gitlab.haskell.org/ghc/ghc/issues/12637
  platform <- view platformL
  -- According to
  -- https://support.microsoft.com/en-us/help/830473/command-prompt-cmd-exe-command-line-string-limitation
  -- the maximum command line length on Windows since XP is 8191 characters. We
  -- use conservative batch size of 100 ids on this OS thus argument name
  -- '-ipid', package name, its version and a hash should fit well into this
  -- limit. On Unix-like systems we're limited by ARG_MAX which is normally
  -- hundreds of kilobytes so batch size of 500 should work fine.
  let batchSize = case platform of
        Platform _ Windows -> 100
        _ -> 500
  let chunksOfNE size = mapMaybe nonEmpty . chunksOf size . NE.toList
  for_ (chunksOfNE batchSize ids) $ \batch -> do
    for_ batch $ \(_, (ident, reason)) -> logReason ident reason
    pkg <- getGhcPkgExe
    unregisterGhcPkgIds True pkg localDB $ fmap (Right . fst) batch

toActions ::
     HasEnvConfig env
  => InstalledMap
  -> Maybe (MVar ())
  -> (RIO env () -> IO ())
  -> ExecuteEnv
  -> (Maybe Task, Maybe Task) -- build and final
  -> [Action]
toActions installedMap mtestLock runInBase ee (mbuild, mfinal) =
  abuild ++ afinal
 where
  abuild = case mbuild of
    Nothing -> []
    Just task ->
      [ Action
          { actionId = ActionId (taskProvides task) ATBuild
          , actionDeps =
              Set.map (`ActionId` ATBuild) task.configOpts.missing
          , action =
              \ac -> runInBase $ singleBuild ac ee task installedMap False
          , concurrency = ConcurrencyAllowed
          }
      ]
  afinal = case mfinal of
    Nothing -> []
    Just task ->
      ( if task.allInOne
          then id
          else (:) Action
            { actionId = ActionId pkgId ATBuildFinal
            , actionDeps = addBuild
                (Set.map (`ActionId` ATBuild) task.configOpts.missing)
            , action =
                \ac -> runInBase $ singleBuild ac ee task installedMap True
            , concurrency = ConcurrencyAllowed
            }
      ) $
      -- These are the "final" actions - running test suites and benchmarks,
      -- unless --no-run-tests or --no-run-benchmarks is enabled.
      ( if Set.null tests || not runTests
          then id
          else (:) Action
            { actionId = ActionId pkgId ATRunTests
            , actionDeps = finalDeps
            , action = \ac -> withLock mtestLock $ runInBase $
                singleTest topts (Set.toList tests) ac ee task installedMap
              -- Always allow tests tasks to run concurrently with other tasks,
              -- particularly build tasks. Note that 'mtestLock' can optionally
              -- make it so that only one test is run at a time.
            , concurrency = ConcurrencyAllowed
            }
      ) $
      ( if Set.null benches || not runBenchmarks
          then id
          else (:) Action
            { actionId = ActionId pkgId ATRunBenchmarks
            , actionDeps = finalDeps
            , action = \ac -> runInBase $
                singleBench
                  beopts
                  (Set.toList benches)
                  ac
                  ee
                  task
                  installedMap
              -- Never run benchmarks concurrently with any other task, see
              -- #3663
            , concurrency = ConcurrencyDisallowed
            }
      )
      []
     where
      pkgId = taskProvides task
      comps = taskComponents task
      tests = testComponents comps
      benches = benchComponents comps
      finalDeps =
        if task.allInOne
          then addBuild mempty
          else Set.singleton (ActionId pkgId ATBuildFinal)
      addBuild =
        case mbuild of
          Nothing -> id
          Just _ -> Set.insert $ ActionId pkgId ATBuild
  withLock Nothing f = f
  withLock (Just lock) f = withMVar lock $ \() -> f
  bopts = ee.buildOpts
  topts = bopts.testOpts
  beopts = bopts.benchmarkOpts
  runTests = topts.runTests
  runBenchmarks = beopts.runBenchmarks

taskComponents :: Task -> Set NamedComponent
taskComponents task =
  case task.taskType of
    TTLocalMutable lp -> lp.components -- FIXME probably just want lpWanted
    TTRemotePackage{} -> Set.empty
