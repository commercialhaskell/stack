{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Perform a build
module Stack.Build.Execute
  ( printPlan
  , preFetch
  , executePlan
  -- * Running Setup.hs
  , ExecuteEnv
  , withExecuteEnv
  , withSingleContext
  , ExcludeTHLoading (..)
  , KeepOutputOpen (..)
  ) where

import           Control.Concurrent.Companion ( Companion, withCompanion )
import           Control.Concurrent.Execute
                   ( Action (..), ActionContext (..), ActionId (..)
                   , ActionType (..)
                   , Concurrency (..), runActions
                   )
import           Control.Concurrent.STM ( check )
import           Data.Attoparsec.Text ( char, choice, digit, parseOnly )
import qualified Data.Attoparsec.Text as P ( string )
import qualified Data.ByteString as S
import           Data.Char ( isSpace )
import           Conduit
                   ( ConduitT, awaitForever, sinkHandle
                   , withSourceFile, yield
                   )
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.List as L
import           Data.List.Split ( chunksOf )
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Tuple ( swap )
import           Data.Time
                   ( ZonedTime, getZonedTime, formatTime, defaultTimeLocale )
import qualified Data.ByteString.Char8 as S8
import qualified Distribution.PackageDescription as C
import qualified Distribution.Simple.Build.Macros as C
import           Distribution.System ( OS (Windows), Platform (Platform) )
import           Distribution.Types.PackageName ( mkPackageName )
import           Distribution.Verbosity ( showForCabal )
import           Distribution.Version ( mkVersion )
import           Path
                   ( PathException, (</>)
                   , parent, parseRelDir, parseRelFile

                   )
import           Path.CheckInstall ( warnInstallSearchPathIssues )
import           Path.Extra
                   ( forgivingResolveFile, rejectMissingFile
                   , toFilePathNoTrailingSep
                   )
import           Path.IO
                   ( doesDirExist, doesFileExist, ensureDir
                   , renameDir

                   )
import           RIO.NonEmpty ( nonEmpty )
import qualified RIO.NonEmpty as NE
import           RIO.Process
                   ( HasProcessContext, eceExitCode

                   , proc, runProcess_

                   , withWorkingDir
                   )
import           Stack.Build.Haddock
                   ( generateDepsHaddockIndex
                   , generateLocalHaddockForHackageArchives
                   , generateLocalHaddockIndex, generateSnapHaddockIndex
                   , openHaddocksInBrowser
                   )
import           Stack.Build.Installed (  )
import           Stack.Build.Target (  )
import           Stack.Config ( checkOwnership )
import           Stack.Constants
                   ( bindirSuffix, cabalPackageName
                   , relDirDist, relDirSetup
                   , relFileBuildLock, relFileSetupHs
                   , relFileSetupLhs, relFileSetupLower, relFileSetupMacrosH

                   )
import           Stack.Constants.Config
                   ( distDirFromDir, distRelativeDir

                   )
import           Stack.Coverage
                   ( deleteHpcReports, generateHpcMarkupIndex
                   , generateHpcUnifiedReport
                   )
import           Stack.GhcPkg ( unregisterGhcPkgIds )
import           Stack.Package
                   ( buildLogPath
                   )
import           Stack.Prelude
import           Stack.Types.ApplyGhcOptions ( ApplyGhcOptions (..) )
import           Stack.Types.Build
                   ( Plan (..)
                   , Task (..), TaskConfigOpts (..), TaskType (..)
                   , ExcludeTHLoading (..), ConvertPathsToAbsolute (..), KeepOutputOpen (..)
                   , taskLocation, taskProvides, taskTypeLocation
                   , taskTypePackageIdentifier
                   )
import           Stack.Types.Build.Exception
                   ( BuildException (..), BuildPrettyException (..) )
import           Stack.Types.BuildOpts
                   ( BuildOpts (..), BuildOptsCLI (..)
                   , CabalVerbosity (..)
                   , ProgressBarFormat (..), TestOpts (..)
                   )
import           Stack.Types.Compiler
                   ( ActualCompiler (..)
                   , getGhcVersion
                   )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), HasCompiler (..)
                   , getGhcPkgExe
                   )
import           Stack.Types.Config
                   ( Config (..), HasConfig (..), buildOptsL )
import           Stack.Types.ConfigureOpts
                   ( BaseConfigOpts (..) )
import           Stack.Types.Dependency (DepValue(dvVersionRange))
import           Stack.Types.DumpPackage ( DumpPackage (..) )
import           Stack.Types.EnvConfig
                   ( HasEnvConfig (..), actualCompilerVersionL
                   , bindirCompilerTools
                   , installationRootDeps, installationRootLocal
                   , packageDatabaseLocal

                   )
import           Stack.Types.EnvSettings ( EnvSettings (..) )
import           Stack.Types.GhcPkgId ( GhcPkgId, ghcPkgIdString )
import           Stack.Types.Installed
                   ( InstallLocation (..), InstalledMap
                   , installedPackageIdentifier
                   )
import           Stack.Types.NamedComponent
                   ( NamedComponent, benchComponents
                   , testComponents
                   )
import           Stack.Types.Package
                   ( LocalPackage (..), Package (..)
                   , packageIdentifier

                   )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Runner ( HasRunner, terminalL )
import           Stack.Types.SourceMap ( Target )
import           Stack.Types.Version ( withinRange )
import qualified System.Directory as D
import           System.Environment ( getExecutablePath )
import           System.FileLock
                   ( SharedExclusive (Exclusive), withFileLock, withTryFileLock
                   )
import qualified System.FilePath as FP
import           Stack.Build.ExecutePackage
                 ( singleBuild, singleTest, singleBench )
import Stack.Build.ExecuteEnv
                 (ExecuteEnv (..), withExecuteEnv
                 )

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
  pkgLocs = Set.unions $ map toPkgLoc $ Map.elems plan.planTasks

  toPkgLoc task =
    case task.taskType of
      TTLocalMutable{} -> Set.empty
      TTRemotePackage _ _ pkgloc -> Set.singleton pkgloc

-- | Print a description of build plan for human consumption.
printPlan :: (HasRunner env, HasTerm env) => Plan -> RIO env ()
printPlan plan = do
  case Map.elems plan.planUnregisterLocal of
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

  case Map.elems plan.planTasks of
    [] -> prettyInfo $
               flow "Nothing to build."
            <> line
    xs -> do
      prettyInfo $
           flow "Would build:"
        <> line
        <> bulletedList (map displayTask xs)
        <> line

  let hasTests = not . Set.null . testComponents . taskComponents
      hasBenches = not . Set.null . benchComponents . taskComponents
      tests = Map.elems $ Map.filter hasTests plan.planFinals
      benches = Map.elems $ Map.filter hasBenches plan.planFinals

  unless (null tests) $ do
    prettyInfo $
         flow "Would test:"
      <> line
      <> bulletedList (map displayTask tests)
      <> line

  unless (null benches) $ do
    prettyInfo $
         flow "Would benchmark:"
      <> line
      <> bulletedList (map displayTask benches)
      <> line

  case Map.toList plan.planInstallExes of
    [] -> prettyInfo $
               flow "No executables to be installed."
            <> line
    xs -> do
      let executableMsg (name, loc) = fillSep $
              fromString (T.unpack name)
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
              TTLocalMutable lp -> pretty $ parent lp.cabalFile
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
  missing = task.taskConfigOpts.tcoMissing

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

    copyExecutables plan.planInstallExes

    config <- view configL
    menv' <- liftIO $ config.processContextSettings EnvSettings
               { esIncludeLocals = True
               , esIncludeGhcPackagePath = True
               , esStackExe = True
               , esLocaleUtf8 = False
               , esKeepGhcRts = False
               }
    withProcessContext menv' $
      forM_ boptsCli.boptsCLIExec $ \(cmd, args) ->
      proc cmd args runProcess_
 where
  mlargestPackageName =
    Set.lookupMax $
    Set.map (length . packageNameString) $
    Map.keysSet plan.planTasks <> Map.keysSet plan.planFinals

copyExecutables ::
       HasEnvConfig env
    => Map Text InstallLocation
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

  currExe <- liftIO getExecutablePath -- needed for windows, see below

  installed <- forMaybeM (Map.toList exes) $ \(name, loc) -> do
    let bindir =
            case loc of
                Snap -> snapBin
                Local -> localBin
    mfp <- forgivingResolveFile bindir (T.unpack name ++ ext)
      >>= rejectMissingFile
    case mfp of
      Nothing -> do
        prettyWarnL
          [ flow "Couldn't find executable"
          , style Current (fromString $ T.unpack name)
          , flow "in directory"
          , pretty bindir <> "."
          ]
        pure Nothing
      Just file -> do
        let destFile = destDir' FP.</> T.unpack name ++ ext
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
        pure $ Just (name <> T.pack ext)

  unless (null installed) $ do
    prettyInfo $
         fillSep
           [ flow "Copied executables to"
           , pretty destDir <> ":"
           ]
      <> line
      <> bulletedList
           (map (fromString . T.unpack . textDisplay) installed :: [StyleDoc])
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
executePlan' installedMap0 targets plan ee = do
  when ee.buildOpts.testOpts.toCoverage deleteHpcReports
  cv <- view actualCompilerVersionL
  case nonEmpty $ Map.toList plan.planUnregisterLocal of
    Nothing -> pure ()
    Just ids -> do
      localDB <- packageDatabaseLocal
      unregisterPackages cv localDB ids

  liftIO $ atomically $ modifyTVar' ee.localDumpPkgs $ \initMap ->
    foldl' (flip Map.delete) initMap $ Map.keys plan.planUnregisterLocal

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
          plan.planTasks
          plan.planFinals
  threads <- view $ configL . to (.jobs)
  let keepGoing = fromMaybe
        (not (Map.null plan.planFinals))
        ee.buildOpts.keepGoing
  terminal <- view terminalL
  terminalWidth <- view termWidthL
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
                    progressFormat = ee.buildOpts.progressBar
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
  when ee.buildOpts.testOpts.toCoverage $ do
    generateHpcUnifiedReport
    generateHpcMarkupIndex
  unless (null errs) $
    prettyThrowM $ ExecutionFailure errs
  when ee.buildOpts.haddock $ do
    if ee.buildOpts.haddockForHackage
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
        when ee.buildOpts.openHaddocks $ do
          let planPkgs, localPkgs, installedPkgs, availablePkgs
                :: Map PackageName (PackageIdentifier, InstallLocation)
              planPkgs =
                Map.map (taskProvides &&& taskLocation) plan.planTasks
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
                $ Map.elems plan.planUnregisterLocal

unregisterPackages ::
     (HasCompiler env, HasPlatform env, HasProcessContext env, HasTerm env)
  => ActualCompiler
  -> Path Abs Dir
  -> NonEmpty (GhcPkgId, (PackageIdentifier, Text))
  -> RIO env ()
unregisterPackages cv localDB ids = do
  let logReason ident reason =
        prettyInfoL
          (  [ fromString (packageIdentifierString ident) <> ":"
             , "unregistering"
             ]
          <> [ parens (flow $ T.unpack reason) | not $ T.null reason ]
          )
  let unregisterSinglePkg select (gid, (ident, reason)) = do
        logReason ident reason
        pkg <- getGhcPkgExe
        unregisterGhcPkgIds True pkg localDB $ select ident gid :| []
  case cv of
    -- GHC versions >= 8.2.1 support batch unregistering of packages. See
    -- https://gitlab.haskell.org/ghc/ghc/issues/12637
    ACGhc v | v >= mkVersion [8, 2, 1] -> do
      platform <- view platformL
      -- According to
      -- https://support.microsoft.com/en-us/help/830473/command-prompt-cmd-exe-command-line-string-limitation
      -- the maximum command line length on Windows since XP is 8191 characters.
      -- We use conservative batch size of 100 ids on this OS thus argument name
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

    -- GHC versions >= 7.9 support unregistering of packages via their GhcPkgId.
    ACGhc v | v >= mkVersion [7, 9] ->
      for_ ids . unregisterSinglePkg $ \_ident gid -> Right gid

    _ -> for_ ids . unregisterSinglePkg $ \ident _gid -> Left ident

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
  abuild = case mbuild of
    Nothing -> []
    Just task ->
      [ Action
          { actionId = ActionId (taskProvides task) ATBuild
          , actionDeps =
              Set.map (`ActionId` ATBuild) task.taskConfigOpts.tcoMissing
          , actionDo =
              \ac -> runInBase $ singleBuild ac ee task installedMap False
          , actionConcurrency = ConcurrencyAllowed
          }
      ]
  afinal = case mfinal of
    Nothing -> []
    Just task ->
      ( if task.taskAllInOne
          then id
          else (:) Action
            { actionId = ActionId pkgId ATBuildFinal
            , actionDeps = addBuild
                (Set.map (`ActionId` ATBuild) task.taskConfigOpts.tcoMissing)
            , actionDo =
                \ac -> runInBase $ singleBuild ac ee task installedMap True
            , actionConcurrency = ConcurrencyAllowed
            }
      ) $
      -- These are the "final" actions - running tests and benchmarks.
      ( if Set.null tests
          then id
          else (:) Action
            { actionId = ActionId pkgId ATRunTests
            , actionDeps = finalDeps
            , actionDo = \ac -> withLock mtestLock $ runInBase $
                singleTest topts (Set.toList tests) ac ee task installedMap
              -- Always allow tests tasks to run concurrently with other tasks,
              -- particularly build tasks. Note that 'mtestLock' can optionally
              -- make it so that only one test is run at a time.
            , actionConcurrency = ConcurrencyAllowed
            }
      ) $
      ( if Set.null benches
          then id
          else (:) Action
            { actionId = ActionId pkgId ATRunBenchmarks
            , actionDeps = finalDeps
            , actionDo = \ac -> runInBase $
                singleBench
                  beopts
                  (Set.toList benches)
                  ac
                  ee
                  task
                  installedMap
              -- Never run benchmarks concurrently with any other task, see
              -- #3663
            , actionConcurrency = ConcurrencyDisallowed
            }
      )
      []
     where
      pkgId = taskProvides task
      comps = taskComponents task
      tests = testComponents comps
      benches = benchComponents comps
      finalDeps =
        if task.taskAllInOne
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

-- | Make a padded prefix for log messages
packageNamePrefix :: ExecuteEnv -> PackageName -> String
packageNamePrefix ee name' =
  let name = packageNameString name'
      paddedName =
        case ee.largestPackageName of
          Nothing -> name
          Just len ->
            assert (len >= length name) $ take len $ name ++ L.repeat ' '
  in  paddedName <> "> "

announceTask ::
     HasLogFunc env
  => ExecuteEnv
  -> TaskType
  -> Utf8Builder
  -> RIO env ()
announceTask ee taskType action = logInfo $
     fromString
       (packageNamePrefix ee (pkgName (taskTypePackageIdentifier taskType)))
  <> action

prettyAnnounceTask ::
     HasTerm env
  => ExecuteEnv
  -> TaskType
  -> StyleDoc
  -> RIO env ()
prettyAnnounceTask ee taskType action = prettyInfo $
     fromString
       (packageNamePrefix ee (pkgName (taskTypePackageIdentifier taskType)))
  <> action

-- | Ensure we're the only action using the directory.  See
-- <https://github.com/commercialhaskell/stack/issues/2730>
withLockedDistDir ::
     forall env a. HasEnvConfig env
  => (StyleDoc -> RIO env ()) -- ^ A pretty announce function
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
      let complainer :: Companion (RIO env)
          complainer delay = do
            delay 5000000 -- 5 seconds
            announce $ fillSep
              [ flow "blocking for directory lock on"
              , pretty lockFP
              ]
            forever $ do
              delay 30000000 -- 30 seconds
              announce $ fillSep
                [ flow "still blocking for directory lock on"
                , pretty lockFP <> ";"
                , flow "maybe another Stack process is running?"
                ]
      withCompanion complainer $
        \stopComplaining ->
          withRunInIO $ \run ->
            withFileLock (toFilePath lockFP) Exclusive $ \_ ->
              run $ stopComplaining *> inner

-- | How we deal with output from GHC, either dumping to a log file or the
-- console (with some prefix).
data OutputType
  = OTLogFile !(Path Abs File) !Handle
  | OTConsole !(Maybe Utf8Builder)

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
withSingleContext ::
     forall env a. HasEnvConfig env
  => ActionContext
  -> ExecuteEnv
  -> TaskType
  -> Map PackageIdentifier GhcPkgId
     -- ^ All dependencies' package ids to provide to Setup.hs.
  -> Maybe String
  -> (  Package        -- Package info
     -> Path Abs File  -- Cabal file path
     -> Path Abs Dir   -- Package root directory file path
        -- Note that the `Path Abs Dir` argument is redundant with the
        -- `Path Abs File` argument, but we provide both to avoid recalculating
        -- `parent` of the `File`.
     -> (KeepOutputOpen -> ExcludeTHLoading -> [String] -> RIO env ())
        -- Function to run Cabal with args
     -> (Utf8Builder -> RIO env ())
        -- An plain 'announce' function, for different build phases
     -> OutputType
     -> RIO env a)
  -> RIO env a
withSingleContext
    ac
    ee
    taskType
    allDeps
    msuffix
    inner0
  = withPackage $ \package cabalfp pkgDir ->
      withOutputType pkgDir package $ \outputType ->
        withCabal package pkgDir outputType $ \cabal ->
          inner0 package cabalfp pkgDir cabal announce outputType
 where
  pkgId = taskTypePackageIdentifier taskType
  announce = announceTask ee taskType
  prettyAnnounce = prettyAnnounceTask ee taskType

  wanted =
    case taskType of
      TTLocalMutable lp -> lp.wanted
      TTRemotePackage{} -> False

  -- Output to the console if this is the last task, and the user asked to build
  -- it specifically. When the action is a 'ConcurrencyDisallowed' action
  -- (benchmarks), then we can also be sure to have exclusive access to the
  -- console, so output is also sent to the console in this case.
  --
  -- See the discussion on #426 for thoughts on sending output to the console
  --from concurrent tasks.
  console =
       (  wanted
       && all
            (\(ActionId ident _) -> ident == pkgId)
            (Set.toList ac.acRemaining)
       && ee.totalWanted == 1
       )
    || ac.acConcurrency == ConcurrencyDisallowed

  withPackage inner =
    case taskType of
      TTLocalMutable lp -> do
        let root = parent lp.cabalFile
        withLockedDistDir prettyAnnounce root $
          inner lp.package lp.cabalFile root
      TTRemotePackage _ package pkgloc -> do
        suffix <-
          parseRelDir $ packageIdentifierString $ packageIdentifier package
        let dir = ee.tempDir </> suffix
        unpackPackageLocation dir pkgloc

        -- See: https://github.com/commercialhaskell/stack/issues/157
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

        let name = pkgName pkgId
        cabalfpRel <- parseRelFile $ packageNameString name ++ ".cabal"
        let cabalfp = dir </> cabalfpRel
        inner package cabalfp dir

  withOutputType pkgDir package inner
    -- Not in interleaved mode. When building a single wanted package, dump
    -- to the console with no prefix.
    | console = inner $ OTConsole Nothing

    -- If the user requested interleaved output, dump to the console with a
    -- prefix.
    | ee.buildOpts.interleavedOutput = inner $
        OTConsole $ Just $ fromString (packageNamePrefix ee package.name)

    -- Neither condition applies, dump to a file.
    | otherwise = do
        logPath <- buildLogPath package msuffix
        ensureDir (parent logPath)
        let fp = toFilePath logPath

        -- We only want to dump logs for local non-dependency packages
        case taskType of
          TTLocalMutable lp | lp.wanted ->
              liftIO $ atomically $ writeTChan ee.logFiles (pkgDir, logPath)
          _ -> pure ()

        withBinaryFile fp WriteMode $ \h -> inner $ OTLogFile logPath h

  withCabal ::
       Package
    -> Path Abs Dir
    -> OutputType
    -> (  (KeepOutputOpen -> ExcludeTHLoading -> [String] -> RIO env ())
       -> RIO env a
       )
    -> RIO env a
  withCabal package pkgDir outputType inner = do
    config <- view configL
    unless config.allowDifferentUser $
      checkOwnership (pkgDir </> config.workDir)
    let envSettings = EnvSettings
          { esIncludeLocals = taskTypeLocation taskType == Local
          , esIncludeGhcPackagePath = False
          , esStackExe = False
          , esLocaleUtf8 = True
          , esKeepGhcRts = False
          }
    menv <- liftIO $ config.processContextSettings envSettings
    distRelativeDir' <- distRelativeDir
    esetupexehs <-
      -- Avoid broken Setup.hs files causing problems for simple build
      -- types, see:
      -- https://github.com/commercialhaskell/stack/issues/370
      case (package.buildType, ee.setupExe) of
        (C.Simple, Just setupExe) -> pure $ Left setupExe
        _ -> liftIO $ Right <$> getSetupHs pkgDir
    inner $ \keepOutputOpen stripTHLoading args -> do
      let cabalPackageArg
            -- Omit cabal package dependency when building
            -- Cabal. See
            -- https://github.com/commercialhaskell/stack/issues/1356
            | package.name == mkPackageName "Cabal" = []
            | otherwise =
                ["-package=" ++ packageIdentifierString
                                    (PackageIdentifier cabalPackageName
                                                      ee.cabalPkgVer)]
          packageDBArgs =
            ( "-clear-package-db"
            : "-global-package-db"
            : map
                (("-package-db=" ++) . toFilePathNoTrailingSep)
                ee.baseConfigOpts.bcoExtraDBs
            ) ++
            ( (  "-package-db="
              ++ toFilePathNoTrailingSep ee.baseConfigOpts.bcoSnapDB
              )
            : (  "-package-db="
              ++ toFilePathNoTrailingSep ee.baseConfigOpts.bcoLocalDB
              )
            : ["-hide-all-packages"]
            )

          warnCustomNoDeps :: RIO env ()
          warnCustomNoDeps =
            case (taskType, package.buildType) of
              (TTLocalMutable lp, C.Custom) | lp.wanted ->
                prettyWarnL
                  [ flow "Package"
                  , fromPackageName package.name
                  , flow "uses a custom Cabal build, but does not use a \
                         \custom-setup stanza"
                  ]
              _ -> pure ()

          getPackageArgs :: Path Abs Dir -> RIO env [String]
          getPackageArgs setupDir =
            case package.setupDeps of
              -- The package is using the Cabal custom-setup
              -- configuration introduced in Cabal 1.24. In
              -- this case, the package is providing an
              -- explicit list of dependencies, and we
              -- should simply use all of them.
              Just customSetupDeps -> do
                unless (Map.member (mkPackageName "Cabal") customSetupDeps) $
                  prettyWarnL
                    [ fromPackageName package.name
                    , flow "has a setup-depends field, but it does not mention \
                           \a Cabal dependency. This is likely to cause build \
                           \errors."
                    ]
                matchedDeps <-
                  forM (Map.toList customSetupDeps) $ \(name, depValue) -> do
                    let matches (PackageIdentifier name' version) =
                             name == name'
                          && version `withinRange` depValue.dvVersionRange
                    case filter (matches . fst) (Map.toList allDeps) of
                      x:xs -> do
                        unless (null xs) $
                          prettyWarnL
                            [ flow "Found multiple installed packages for \
                                   \custom-setup dep:"
                            , style Current (fromPackageName name) <> "."
                            ]
                        pure ("-package-id=" ++ ghcPkgIdString (snd x), Just (fst x))
                      [] -> do
                        prettyWarnL
                          [ flow "Could not find custom-setup dep:"
                          , style Current (fromPackageName name) <> "."
                          ]
                        pure ("-package=" ++ packageNameString name, Nothing)
                let depsArgs = map fst matchedDeps
                -- Generate setup_macros.h and provide it to ghc
                let macroDeps = mapMaybe snd matchedDeps
                    cppMacrosFile = setupDir </> relFileSetupMacrosH
                    cppArgs =
                      ["-optP-include", "-optP" ++ toFilePath cppMacrosFile]
                writeBinaryFileAtomic
                  cppMacrosFile
                  ( encodeUtf8Builder
                      ( T.pack
                          ( C.generatePackageVersionMacros
                              package.version
                              macroDeps
                          )
                      )
                  )
                pure (packageDBArgs ++ depsArgs ++ cppArgs)

              -- This branch is usually taken for builds, and is always taken
              -- for `stack sdist`.
              --
              -- This approach is debatable. It adds access to the snapshot
              -- package database for Cabal. There are two possible objections:
              --
              -- 1. This doesn't isolate the build enough; arbitrary other
              -- packages available could cause the build to succeed or fail.
              --
              -- 2. This doesn't provide enough packages: we should also
              -- include the local database when building local packages.
              --
              -- Currently, this branch is only taken via `stack sdist` or when
              -- explicitly requested in the stack.yaml file.
              Nothing -> do
                warnCustomNoDeps
                pure $
                     cabalPackageArg
                      -- NOTE: This is different from packageDBArgs above in
                      -- that it does not include the local database and does
                      -- not pass in the -hide-all-packages argument
                  ++ ( "-clear-package-db"
                     : "-global-package-db"
                     : map
                         (("-package-db=" ++) . toFilePathNoTrailingSep)
                         ee.baseConfigOpts.bcoExtraDBs
                     ++ [    "-package-db="
                          ++ toFilePathNoTrailingSep ee.baseConfigOpts.bcoSnapDB
                        ]
                     )

          setupArgs =
            ("--builddir=" ++ toFilePathNoTrailingSep distRelativeDir') : args

          runExe :: Path Abs File -> [String] -> RIO env ()
          runExe exeName fullArgs = do
            compilerVer <- view actualCompilerVersionL
            runAndOutput compilerVer `catch` \ece -> do
              (mlogFile, bss) <-
                case outputType of
                  OTConsole _ -> pure (Nothing, [])
                  OTLogFile logFile h ->
                    if keepOutputOpen == KeepOpen
                    then
                      pure (Nothing, []) -- expected failure build continues further
                    else do
                      liftIO $ hClose h
                      fmap (Just logFile,) $ withSourceFile (toFilePath logFile) $
                        \src ->
                             runConduit
                           $ src
                          .| CT.decodeUtf8Lenient
                          .| mungeBuildOutput
                               stripTHLoading makeAbsolute pkgDir compilerVer
                          .| CL.consume
              prettyThrowM $ CabalExitedUnsuccessfully
                (eceExitCode ece) pkgId exeName fullArgs mlogFile bss
           where
            runAndOutput :: ActualCompiler -> RIO env ()
            runAndOutput compilerVer = withWorkingDir (toFilePath pkgDir) $
              withProcessContext menv $ case outputType of
                OTLogFile _ h -> do
                  let prefixWithTimestamps =
                        if config.prefixTimestamps
                          then PrefixWithTimestamps
                          else WithoutTimestamps
                  void $ sinkProcessStderrStdout (toFilePath exeName) fullArgs
                    (sinkWithTimestamps prefixWithTimestamps h)
                    (sinkWithTimestamps prefixWithTimestamps h)
                OTConsole mprefix ->
                  let prefix = fromMaybe mempty mprefix
                  in  void $ sinkProcessStderrStdout
                        (toFilePath exeName)
                        fullArgs
                        (outputSink KeepTHLoading LevelWarn compilerVer prefix)
                        (outputSink stripTHLoading LevelInfo compilerVer prefix)
            outputSink ::
                 HasCallStack
              => ExcludeTHLoading
              -> LogLevel
              -> ActualCompiler
              -> Utf8Builder
              -> ConduitM S.ByteString Void (RIO env) ()
            outputSink excludeTH level compilerVer prefix =
              CT.decodeUtf8Lenient
              .| mungeBuildOutput excludeTH makeAbsolute pkgDir compilerVer
              .| CL.mapM_ (logGeneric "" level . (prefix <>) . display)
            -- If users want control, we should add a config option for this
            makeAbsolute :: ConvertPathsToAbsolute
            makeAbsolute = case stripTHLoading of
              ExcludeTHLoading -> ConvertPathsToAbsolute
              KeepTHLoading    -> KeepPathsAsIs

      exeName <- case esetupexehs of
        Left setupExe -> pure setupExe
        Right setuphs -> do
          distDir <- distDirFromDir pkgDir
          let setupDir = distDir </> relDirSetup
              outputFile = setupDir </> relFileSetupLower
          customBuilt <- liftIO $ readIORef ee.customBuilt
          if Set.member package.name customBuilt
            then pure outputFile
            else do
              ensureDir setupDir
              compilerPath <- view $ compilerPathsL . to (.cpCompiler)
              packageArgs <- getPackageArgs setupDir
              runExe compilerPath $
                [ "--make"
                , "-odir", toFilePathNoTrailingSep setupDir
                , "-hidir", toFilePathNoTrailingSep setupDir
                , "-i", "-i."
                ] ++ packageArgs ++
                [ toFilePath setuphs
                , toFilePath ee.setupShimHs
                , "-main-is"
                , "StackSetupShim.mainOverride"
                , "-o", toFilePath outputFile
                , "-threaded"
                ] ++

                -- Apply GHC options
                -- https://github.com/commercialhaskell/stack/issues/4526
                map
                  T.unpack
                  ( Map.findWithDefault
                      []
                      AGOEverything
                      config.ghcOptionsByCat
                  ++ case config.applyGhcOptions of
                       AGOEverything -> ee.buildOptsCLI.boptsCLIGhcOptions
                       AGOTargets -> []
                       AGOLocals -> []
                  )

              liftIO $ atomicModifyIORef' ee.customBuilt $
                \oldCustomBuilt ->
                  (Set.insert package.name oldCustomBuilt, ())
              pure outputFile
      let cabalVerboseArg =
            let CabalVerbosity cv = ee.buildOpts.cabalVerbose
            in  "--verbose=" <> showForCabal cv
      runExe exeName $ cabalVerboseArg:setupArgs

-- | Strip Template Haskell "Loading package" lines and making paths absolute.
mungeBuildOutput ::
     forall m. (MonadIO m, MonadUnliftIO m)
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
       (  "ghc.exe: warning:" `T.isPrefixOf` str
       || "ghc.EXE: warning:" `T.isPrefixOf` str
       )
    && "is linked instead of __imp_" `T.isInfixOf` str

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
        then
          fmap (fmap ((T.takeWhile isSpace x <>) . T.pack . toFilePath)) $
            forgivingResolveFile pkgDir (T.unpack $ T.dropWhile isSpace x) `catch`
              \(_ :: PathException) -> pure Nothing
        else pure Nothing
    case mabs of
      Nothing -> pure bs
      Just fp -> pure $ fp `T.append` y

  doNothing :: ConduitM Text Text m ()
  doNothing = awaitForever yield

  -- | Match the error location format at the end of lines
  isValidSuffix = isRight . parseOnly lineCol
  lineCol = char ':'
    >> choice
         [ num >> char ':' >> num >> optional (char '-' >> num) >> pure ()
         , char '(' >> num >> char ',' >> num >> P.string ")-(" >> num >>
           char ',' >> num >> char ')' >> pure ()
         ]
    >> char ':'
    >> pure ()
   where
    num = some digit

-- | Whether to prefix log lines with timestamps.
data PrefixWithTimestamps
  = PrefixWithTimestamps
  | WithoutTimestamps

-- | Write stream of lines to handle, but adding timestamps.
sinkWithTimestamps ::
     MonadIO m
  => PrefixWithTimestamps
  -> Handle
  -> ConduitT ByteString Void m ()
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
formatZonedTimeForLog =
  S8.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6Q"

-- | Find the Setup.hs or Setup.lhs in the given directory. If none exists,
-- throw an exception.
getSetupHs :: Path Abs Dir -- ^ project directory
           -> IO (Path Abs File)
getSetupHs dir = do
  exists1 <- doesFileExist fp1
  if exists1
    then pure fp1
    else do
      exists2 <- doesFileExist fp2
      if exists2
        then pure fp2
        else throwM $ NoSetupHsFound dir
 where
  fp1 = dir </> relFileSetupHs
  fp2 = dir </> relFileSetupLhs

taskComponents :: Task -> Set NamedComponent
taskComponents task =
  case task.taskType of
    TTLocalMutable lp -> lp.components -- FIXME probably just want lpWanted
    TTRemotePackage{} -> Set.empty
