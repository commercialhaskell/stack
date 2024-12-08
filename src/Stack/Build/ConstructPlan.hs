{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Construct a @Plan@ for how to build
module Stack.Build.ConstructPlan
  ( constructPlan
  ) where

import           Control.Monad.Trans.Maybe ( MaybeT (..) )
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import           Data.Monoid.Map ( MonoidMap(..) )
import qualified Data.Set as Set
import qualified Data.Text as T
import           Distribution.Types.BuildType ( BuildType (Configure) )
import           Distribution.Types.PackageName ( mkPackageName )
import           Path ( parent )
import qualified RIO.NonEmpty as NE
import           RIO.Process ( findExecutable )
import           RIO.State
                   ( State, StateT (..), execState, get, modify, modify', put )
import           RIO.Writer ( WriterT (..), pass, tell )
import           Stack.Build.Cache ( tryGetFlagCache )
import           Stack.Build.Haddock ( shouldHaddockDeps )
import           Stack.Build.Source ( loadLocalPackage )
import           Stack.Constants ( compilerOptionsCabalFlag )
import           Stack.Package
                   ( applyForceCustomBuild, buildableExes, packageUnknownTools
                   , processPackageDepsEither
                   )
import           Stack.Prelude hiding ( loadPackage )
import           Stack.SourceMap ( getPLIVersion, mkProjectPackage )
import           Stack.Types.Build
                   ( CachePkgSrc (..), ConfigCache (..), Plan (..), Task (..)
                   , TaskConfigOpts (..), TaskType (..)
                   , installLocationIsMutable, taskIsTarget, taskLocation
                   , taskProvides, taskTargetIsMutable, toCachePkgSrc
                   )
import           Stack.Types.Build.ConstructPlan
                   ( AddDepRes (..), CombinedMap, Ctx (..), M
                   , MissingPresentDeps (..), PackageInfo (..), ToolWarning(..)
                   , UnregisterState (..), W (..), adrHasLibrary, adrVersion
                   , isAdrToInstall, toTask
                   )
import           Stack.Types.Build.Exception
                   ( BadDependency (..), BuildException (..)
                   , BuildPrettyException (..), ConstructPlanException (..)
                   )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..), configFileL )
import           Stack.Types.BuildOpts ( BuildOpts (..) )
import           Stack.Types.BuildOptsCLI
                   ( BuildOptsCLI (..), BuildSubset (..) )
import           Stack.Types.CompCollection ( collectionMember )
import           Stack.Types.Compiler ( WhichCompiler (..) )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), HasCompiler (..) )
import           Stack.Types.ComponentUtils ( unqualCompFromText )
import           Stack.Types.Config ( Config (..), HasConfig (..), stackRootL )
import           Stack.Types.ConfigureOpts ( BaseConfigOpts (..) )
import qualified Stack.Types.ConfigureOpts as ConfigureOpts
import           Stack.Types.Curator ( Curator (..) )
import           Stack.Types.Dependency ( DepValue (..), isDepTypeLibrary )
import           Stack.Types.DumpPackage ( DumpPackage (..), sublibParentPkgId )
import           Stack.Types.EnvConfig ( EnvConfig (..), HasEnvConfig (..) )
import           Stack.Types.EnvSettings
                   ( EnvSettings (..), minimalEnvSettings )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.Installed
                   ( InstallLocation (..), Installed (..), InstalledMap
                   , installedVersion
                   )
import           Stack.Types.IsMutable ( IsMutable (..) )
import           Stack.Types.NamedComponent ( exeComponents, renderComponent )
import           Stack.Types.Package
                   ( ExeName (..), LocalPackage (..), Package (..)
                   , PackageSource (..), installedMapGhcPkgId
                   , packageIdentifier, psVersion, runMemoizedWith
                   )
import           Stack.Types.ProjectConfig ( isPCGlobalProject )
import           Stack.Types.Runner ( HasRunner (..), globalOptsL )
import           Stack.Types.SourceMap
                   ( CommonPackage (..), DepPackage (..), FromSnapshot (..)
                   , GlobalPackage (..), SMTargets (..), SourceMap (..)
                   )
import           Stack.Types.Version
                   ( VersionRange, latestApplicableVersion, versionRangeText
                   , withinRange
                   )
import           System.Environment ( lookupEnv )

-- | Computes a build plan. This means figuring out which build 'Task's to take,
-- and the interdependencies among the build 'Task's. In particular:
--
-- 1) It determines which packages need to be built, based on the transitive
-- deps of the current targets. For project packages, this is indicated by the
-- 'lpWanted' boolean. For extra packages to build, this comes from the
-- @extraToBuild0@ argument of type @Set PackageName@. These are usually
-- packages that have been specified on the command line.
--
-- 2) It will only rebuild an upstream package if it isn't present in the
-- 'InstalledMap', or if some of its dependencies have changed.
--
-- 3) It will only rebuild a local package if its files are dirty or some of its
-- dependencies have changed.
constructPlan ::
     forall env. HasEnvConfig env
  => BaseConfigOpts
  -> [DumpPackage] -- ^ locally registered
  -> (  PackageLocationImmutable
     -> Map FlagName Bool
     -> [Text]
        -- ^ GHC options
     -> [Text]
        -- ^ Cabal configure options
     -> RIO EnvConfig Package
     )
     -- ^ load upstream package
  -> SourceMap
  -> InstalledMap
  -> Bool
     -- ^ Only include initial build steps required for GHCi?
  -> RIO env Plan
constructPlan
    baseConfigOpts0
    localDumpPkgs
    loadPackage0
    sourceMap
    installedMap
    initialBuildSteps
  = do
    logDebug "Constructing the build plan"

    when hasBaseInDeps $
      prettyWarn $
           fillSep
             [ flow "You are trying to upgrade or downgrade the"
             , style Current "base"
             , flow "package, which is almost certainly not what you really \
                    \want. Please, consider using another GHC version if you \
                    \need a certain version of"
             , style Current "base" <> ","
             , flow "or removing"
             , style Current "base"
             , flow "as an"
             , style Shell "extra-deps" <> "."
             , flow "For further information, see"
             , style Url "https://github.com/commercialhaskell/stack/issues/3940" <> "."
             ]
        <> line

    econfig <- view envConfigL
    globalCabalVersion <- view $ compilerPathsL . to (.cabalVersion)
    sources <- getSources globalCabalVersion
    curator <- view $ buildConfigL . to (.curator)
    pathEnvVar <- liftIO $ maybe mempty T.pack <$> lookupEnv "PATH"
    let ctx = mkCtx econfig globalCabalVersion sources curator pathEnvVar
        targetPackageNames = Map.keys sourceMap.targets.targets
        -- Ignore the result of 'getCachedDepOrAddDep'.
        onTarget = void . getCachedDepOrAddDep
        inner = mapM_ onTarget targetPackageNames
    (((), W efinals installExes dirtyReason warnings parents), m) <-
      liftIO $ runRIO ctx (runStateT (runWriterT inner) Map.empty)
    -- Report any warnings
    mapM_ prettyWarn (warnings [])
    -- Separate out errors
    let (errlibs, adrs) = partitionEithers $ map toEither $ Map.toList m
        (errfinals, finals) =
          partitionEithers $ map toEither $ Map.toList efinals
        errs = errlibs ++ errfinals
    if null errs
      then do
        let tasks = Map.fromList $ mapMaybe (toMaybe . second toTask) adrs
        takeSubset Plan
          { tasks = tasks
          , finals = Map.fromList finals
          , unregisterLocal =
              mkUnregisterLocal tasks dirtyReason localDumpPkgs initialBuildSteps
          , installExes =
              if    baseConfigOpts0.buildOpts.installExes
                 || baseConfigOpts0.buildOpts.installCompilerTool
                then installExes
                else Map.empty
          }
      else do
        configFile <- view configFileL
        stackRoot <- view stackRootL
        isImplicitGlobal <-
          view $ configL . to (isPCGlobalProject . (.project))
        prettyThrowM $ ConstructPlanFailed
          errs
          configFile
          stackRoot
          isImplicitGlobal
          parents
          ctx.wanted
          prunedGlobalDeps
 where
  sourceProject = sourceMap.project
  sourceDeps = sourceMap.deps

  hasBaseInDeps = Map.member (mkPackageName "base") sourceDeps

  mkCtx ctxEnvConfig globalCabalVersion sources curator pathEnvVar = Ctx
    { baseConfigOpts = baseConfigOpts0
    , loadPackage = \w x y z -> runRIO ctxEnvConfig $
        applyForceCustomBuild globalCabalVersion <$> loadPackage0 w x y z
    , combinedMap = combineMap sources installedMap
    , ctxEnvConfig
    , callStack = []
    , wanted = Map.keysSet sourceMap.targets.targets
    , localNames = Map.keysSet sourceProject
    , curator
    , pathEnvVar
    }

  toEither :: (k, Either e v) -> Either e (k, v)
  toEither (_, Left e)  = Left e
  toEither (k, Right v) = Right (k, v)

  toMaybe :: (k, Maybe v) -> Maybe (k, v)
  toMaybe (_, Nothing) = Nothing
  toMaybe (k, Just v) = Just (k, v)

  takeSubset :: Plan -> RIO env Plan
  takeSubset = case baseConfigOpts0.buildOptsCLI.buildSubset of
    BSAll -> pure
    BSOnlySnapshot -> stripLocals
    BSOnlyDependencies -> stripNonDeps
    BSOnlyLocals -> errorOnSnapshot

  -- | Strip out anything from the 'Plan' intended for the local database.
  stripLocals :: Plan -> RIO env Plan
  stripLocals plan = pure plan
    { tasks = Map.filter checkTask plan.tasks
    , finals = Map.empty
    , unregisterLocal = Map.empty
    , installExes = Map.filter (/= Local) plan.installExes
    }
   where
    checkTask task = taskLocation task == Snap

  stripNonDeps :: Plan -> RIO env Plan
  stripNonDeps plan = pure plan
    { tasks = Map.filter checkTask plan.tasks
    , finals = Map.empty
    , installExes = Map.empty -- TODO maybe don't disable this?
    }
   where
    deps = Map.keysSet sourceDeps
    checkTask task = taskProvides task `Set.member` missingForDeps
    providesDep task = pkgName (taskProvides task) `Set.member` deps
    tasks = Map.elems plan.tasks
    missing =
      Map.fromList $ map (taskProvides &&&  (.configOpts.missing)) tasks
    missingForDeps = flip execState mempty $
      for_ tasks $ \task ->
        when (providesDep task) $
          collectMissing mempty (taskProvides task)
    collectMissing dependents pid = do
      when (pid `elem` dependents) $
        impureThrow $ TaskCycleBug pid
      modify' (<> Set.singleton pid)
      mapM_
        (collectMissing (pid:dependents))
        (fromMaybe mempty $ Map.lookup pid missing)

  -- | Throw an exception if there are any snapshot packages in the plan.
  errorOnSnapshot :: Plan -> RIO env Plan
  errorOnSnapshot plan@(Plan tasks _finals _unregister installExes) = do
    let snapTasks = Map.keys $ Map.filter (\t -> taskLocation t == Snap) tasks
        snapExes = Map.keys $ Map.filter (== Snap) installExes
    unless (null snapTasks && null snapExes) $
      prettyThrowIO $ NotOnlyLocal snapTasks snapExes
    pure plan

  prunedGlobalDeps :: Map PackageName [PackageName]
  prunedGlobalDeps = flip Map.mapMaybe sourceMap.globalPkgs $
    \case
      ReplacedGlobalPackage deps ->
        let pruned = filter (not . inSourceMap) deps
        in  if null pruned then Nothing else Just pruned
      GlobalPackage _ -> Nothing
   where
    inSourceMap pname =
      pname `Map.member` sourceDeps || pname `Map.member` sourceProject

  getSources :: Version -> RIO env (Map PackageName PackageSource)
  getSources globalCabalVersion = do
    let loadLocalPackage' pp = do
          lp <- loadLocalPackage pp
          let lpPackage' =
                applyForceCustomBuild globalCabalVersion lp.package
          pure lp { package = lpPackage' }
    pPackages <- for sourceProject $ \pp -> do
      lp <- loadLocalPackage' pp
      pure $ PSFilePath lp
    bopts <- view $ configL . to (.build)
    deps <- for sourceDeps $ \dp ->
      case dp.location of
        PLImmutable loc ->
          pure $
            PSRemote loc (getPLIVersion loc) dp.fromSnapshot dp.depCommon
        PLMutable dir -> do
          pp <- mkProjectPackage YesPrintWarnings dir (shouldHaddockDeps bopts)
          lp <- loadLocalPackage' pp
          pure $ PSFilePath lp
    pure $ pPackages <> deps

-- | Determine which packages to unregister based on the given tasks and
-- already registered project packages and local extra-deps.
mkUnregisterLocal ::
     Map PackageName Task
     -- ^ Tasks
  -> Map PackageName Text
     -- ^ Reasons why packages are dirty and must be rebuilt
  -> [DumpPackage]
     -- ^ Local package database dump
  -> Bool
     -- ^ If true, we're doing a special initialBuildSteps build - don't
     -- unregister target packages.
  -> Map GhcPkgId (PackageIdentifier, Text)
mkUnregisterLocal tasks dirtyReason localDumpPkgs initialBuildSteps =
  -- We'll take multiple passes through the local packages. This will allow us
  -- to detect that a package should be unregistered, as well as all packages
  -- directly or transitively depending on it.
  loop Map.empty localDumpPkgs
 where
  loop ::
       Map GhcPkgId (PackageIdentifier, Text)
       -- ^ Current local packages to unregister.
    -> [DumpPackage]
       -- ^ Current local packages to keep.
    -> Map GhcPkgId (PackageIdentifier, Text)
       -- ^ Revised local packages to unregister.
  loop toUnregister keep
    -- If any new packages were added to the unregister Map, we need to loop
    -- through the remaining packages again to detect if a transitive dependency
    -- is being unregistered.
    | us.anyAdded = loop us.toUnregister us.toKeep
    -- Nothing added, so we've already caught them all. Return the Map we've
    -- already calculated.
    | otherwise = us.toUnregister
   where
    -- Run the unregister checking function on all packages we currently think
    -- we'll be keeping.
    us = execState (mapM_ go keep) initialUnregisterState
    initialUnregisterState = UnregisterState
      { toUnregister
      , toKeep = []
      , anyAdded = False
      }

  go :: DumpPackage -> State UnregisterState ()
  go dp = do
    us <- get
    case maybeUnregisterReason us.toUnregister ident mParentLibId deps of
      -- Not unregistering, add it to the keep list.
      Nothing -> put us { toKeep = dp : us.toKeep }
      -- Unregistering, add it to the unregister Map; and indicate that a
      -- package was in fact added to the unregister Map, so we loop again.
      Just reason -> put us
        { toUnregister = Map.insert gid (ident, reason) us.toUnregister
        , anyAdded = True
        }
   where
    gid = dp.ghcPkgId
    ident = dp.packageIdent
    mParentLibId = sublibParentPkgId dp
    deps = dp.depends

  maybeUnregisterReason ::
       Map GhcPkgId (PackageIdentifier, Text)
       -- ^ Current local packages to unregister.
    -> PackageIdentifier
       -- ^ Package identifier.
    -> Maybe PackageIdentifier
       -- ^ If package for sub library, package identifier of the parent.
    -> [GhcPkgId]
       -- ^ Dependencies of the package.
    -> Maybe Text
       -- ^ If to be unregistered, the reason for doing so.
  maybeUnregisterReason toUnregister ident mParentLibId deps
    -- If the package is not for a sub library, then it is directly relevant. If
    -- it is, then the relevant package is the parent. If we are planning on
    -- running a task on the relevant package, then the package must be
    -- unregistered, unless it is a target and an initial-build-steps build is
    -- being done.
    | Just task <- Map.lookup relevantPkgName tasks =
        if    initialBuildSteps
           && taskIsTarget task
           && taskProvides task == relevantPkgId
          then Nothing
          else Just $ fromMaybe "" $ Map.lookup relevantPkgName dirtyReason
    -- Check if a dependency is going to be unregistered
    | (dep, _):_ <- mapMaybe (`Map.lookup` toUnregister) deps =
        Just $ "Dependency being unregistered: "
          <> T.pack (packageIdentifierString dep)
    -- None of the above, keep it!
    | otherwise = Nothing
    where
      -- If the package is not for a sub library, then the relevant package
      -- identifier is that of the package. If it is, then the relevant package
      -- identifier is that of the parent.
      relevantPkgId :: PackageIdentifier
      relevantPkgId = fromMaybe ident mParentLibId
      -- If the package is not for a sub library, then the relevant package name
      -- is that of the package. If it is, then the relevant package name is
      -- that of the parent.
      relevantPkgName :: PackageName
      relevantPkgName = maybe (pkgName ident) pkgName mParentLibId

-- | Given a 'LocalPackage' and its 'lpTestBench', adds a 'Task' for running its
-- tests and benchmarks.
--
-- If @isAllInOne@ is 'True', then this means that the build step will also
-- build the tests. Otherwise, this indicates that there's a cyclic dependency
-- and an additional build step needs to be done.
--
-- This will also add all the deps needed to build the tests / benchmarks. If
-- @isAllInOne@ is 'True' (the common case), then all of these should have
-- already been taken care of as part of the build step.
addFinal ::
     LocalPackage
  -> Package
  -> Bool
     -- ^ Will the build step also build the tests?
  -> Bool
     -- ^ Should Haddock documentation be built?
  -> M ()
addFinal lp package allInOne buildHaddocks = do
  depsRes <- addPackageDeps package
  res <- case depsRes of
    Left e -> pure $ Left e
    Right (MissingPresentDeps missing present _minLoc) -> do
      let pkgConfigOpts = ConfigureOpts.packageConfigureOptsFromPackage package
      ctx <- ask
      let configOpts = TaskConfigOpts
            { missing
            , envConfig = ctx.ctxEnvConfig
            , baseConfigOpts = ctx.baseConfigOpts
            , isLocalNonExtraDep = True
            , isMutable = Mutable
            , pkgConfigOpts
            }
      pure $ Right Task
        { configOpts
        , buildHaddocks
        , present
        , taskType = TTLocalMutable lp
        , allInOne
        , cachePkgSrc = CacheSrcLocal (toFilePath (parent lp.cabalFP))
        , buildTypeConfig = packageBuildTypeConfig package
        }
  tell mempty { wFinals = Map.singleton package.name res }

-- | Given a 'PackageName', adds all of the build tasks to build the package, if
-- needed. First checks if the package name is in the library map.
--
-- 'constructPlan' invokes this on all the target packages, setting
-- @treatAsDep'@ to False, because those packages are direct build targets.
-- 'addPackageDeps' invokes this while recursing into the dependencies of a
-- package. As such, it sets @treatAsDep'@ to True, forcing this package to be
-- marked as a dependency, even if it is directly wanted. This makes sense - if
-- we left out packages that are deps, it would break the --only-dependencies
-- build plan.
getCachedDepOrAddDep ::
     PackageName
  -> M (Either ConstructPlanException AddDepRes)
getCachedDepOrAddDep name = do
  libMap <- get
  case Map.lookup name libMap of
    Just res -> do
      logDebugPlanS "getCachedDepOrAddDep" $
           "Using cached result for "
        <> fromPackageName name
        <> ": "
        <> fromString (show res)
      pure res
    Nothing -> checkCallStackAndAddDep name

-- | Given a 'PackageName', known not to be in the library map, adds all of the
-- build tasks to build the package. First checks that the package name is not
-- already in the call stack.
checkCallStackAndAddDep ::
     PackageName
  -> M (Either ConstructPlanException AddDepRes)
checkCallStackAndAddDep name = do
  ctx <- ask
  res <- if name `elem` ctx.callStack
    then do
      logDebugPlanS "checkCallStackAndAddDep" $
           "Detected cycle "
        <> fromPackageName name
        <> ": "
        <> fromString (show $ map packageNameString ctx.callStack)
      pure $ Left $ DependencyCycleDetected $ name : ctx.callStack
    else case Map.lookup name ctx.combinedMap of
      -- TODO look up in the package index and see if there's a
      -- recommendation available
      Nothing -> do
        logDebugPlanS "checkCallStackAndAddDep" $
             "No package info for "
          <> fromPackageName name
          <> "."
        pure $ Left $ UnknownPackage name
      Just packageInfo ->
        -- Add the current package name to the head of the call stack.
        local (\ctx' -> ctx' { callStack = name : ctx'.callStack }) $
          addDep name packageInfo
  updateLibMap name res
  pure res

-- | Given a 'PackageName' and its 'PackageInfo' from the combined map, adds all
-- of the build tasks to build the package. Assumes that the head of the call
-- stack is the current package name.
addDep ::
     PackageName
  -> PackageInfo
  -> M (Either ConstructPlanException AddDepRes)
addDep name packageInfo = do
  logDebugPlanS "addDep" $
       "Package info for "
    <> fromPackageName name
    <> ": "
    <> fromString (show packageInfo)
  case packageInfo of
    PIOnlyInstalled loc installed -> do
      -- FIXME Slightly hacky, no flags since they likely won't affect
      -- executable names. This code does not feel right.
      let version = installedVersion installed
          askPkgLoc = liftRIO $ do
            mrev <- getLatestHackageRevision YesRequireHackageIndex name version
            case mrev of
              Nothing -> do
                -- This could happen for GHC boot libraries missing from
                -- Hackage.
                cs <- asks (NE.nonEmpty . (.callStack))
                cs' <- maybe
                  (throwIO CallStackEmptyBug)
                  (pure . NE.tail)
                  cs
                prettyWarnL
                  $ flow "No latest package revision found for"
                  : style Current (fromPackageName name) <> ","
                  : flow "dependency callstack:"
                  : mkNarrativeList Nothing False
                      (map fromPackageName cs' :: [StyleDoc])
                pure Nothing
              Just (_rev, cfKey, treeKey) ->
                pure $ Just $
                  PLIHackage (PackageIdentifier name version) cfKey treeKey
      tellExecutablesUpstream name askPkgLoc loc Map.empty
      pure $ Right $ ADRFound loc installed
    PIOnlySource ps -> do
      tellExecutables name ps
      installPackage name ps Nothing
    PIBoth ps installed -> do
      tellExecutables name ps
      installPackage name ps (Just installed)

-- | For given 'PackageName' and 'PackageSource' values, adds relevant
-- executables to the collected output.
tellExecutables :: PackageName -> PackageSource -> M ()
tellExecutables _name (PSFilePath lp)
  | lp.wanted = tellExecutablesPackage Local lp.package
  | otherwise = pure ()
-- Ignores ghcOptions because they don't matter for enumerating executables.
tellExecutables name (PSRemote pkgloc _version _fromSnapshot cp) =
  tellExecutablesUpstream name (pure $ Just pkgloc) Snap cp.flags

-- | For a given 'PackageName' value, known to be immutable, adds relevant
-- executables to the collected output.
tellExecutablesUpstream ::
     PackageName
  -> M (Maybe PackageLocationImmutable)
  -> InstallLocation
  -> Map FlagName Bool
  -> M ()
tellExecutablesUpstream name retrievePkgLoc loc flags = do
  ctx <- ask
  when (name `Set.member` ctx.wanted) $ do
    mPkgLoc <- retrievePkgLoc
    forM_ mPkgLoc $ \pkgLoc -> do
      p <- ctx.loadPackage pkgLoc flags [] []
      tellExecutablesPackage loc p

-- | For given 'InstallLocation' and 'Package' values, adds relevant executables
-- to the collected output. In most cases, the relevant executables are all the
-- executables of the package. If the package is a wanted local one, the
-- executables are those executables that are wanted executables.
tellExecutablesPackage :: InstallLocation -> Package -> M ()
tellExecutablesPackage loc p = do
  cm <- asks (.combinedMap)
  -- Determine which components are enabled so we know which ones to copy
  let myComps =
        case Map.lookup p.name cm of
          Nothing -> assert False Set.empty
          Just (PIOnlyInstalled _ _) -> Set.empty
          Just (PIOnlySource ps) -> goSource ps
          Just (PIBoth ps _) -> goSource ps

      goSource (PSFilePath lp)
        | lp.wanted = exeComponents lp.components
        | otherwise = Set.empty
      goSource PSRemote{} = Set.empty

  tell mempty
    { wInstall = Map.fromList $
        map (, loc) $ Set.toList $ filterComps myComps $ buildableExes p
    }
 where
  filterComps myComps x
    | Set.null myComps = x
    | otherwise = Set.intersection x myComps

-- | Given a 'PackageSource' and perhaps an 'Installed' value, adds build
-- 'Task's for the package and its dependencies.
installPackage :: PackageName
               -> PackageSource
               -> Maybe Installed
               -> M (Either ConstructPlanException AddDepRes)
installPackage name ps minstalled = do
  ctx <- ask
  case ps of
    PSRemote pkgLoc _version _fromSnapshot cp -> do
      logDebugPlanS "installPackage" $
           "Doing all-in-one build for upstream package "
        <> fromPackageName name
        <> "."
      package <- ctx.loadPackage
        pkgLoc cp.flags cp.ghcOptions cp.cabalConfigOpts
      resolveDepsAndInstall True cp.buildHaddocks ps package minstalled
    PSFilePath lp -> do
      case lp.testBench of
        Nothing -> do
          logDebugPlanS "installPackage" $
               "No test or bench component for "
            <> fromPackageName name
            <> " so doing an all-in-one build."
          resolveDepsAndInstall
            True lp.buildHaddocks ps lp.package minstalled
        Just tb -> do
          -- Attempt to find a plan which performs an all-in-one build. Ignore
          -- the writer action + reset the state if it fails.
          libMap <- get
          res <- pass $ do
            res <- addPackageDeps tb
            let writerFunc w = case res of
                  Left _ -> mempty
                  _ -> w
            pure (res, writerFunc)
          case res of
            Right deps -> do
              logDebugPlanS "installPackage" $
                   "For "
                <> fromPackageName name
                <> ", successfully added package deps."
              -- in curator builds we can't do all-in-one build as
              -- test/benchmark failure could prevent library from being
              -- available to its dependencies but when it's already available
              -- it's OK to do that
              splitRequired <- expectedTestOrBenchFailures <$> asks (.curator)
              let isAllInOne = not splitRequired
              adr <- installPackageGivenDeps
                isAllInOne lp.buildHaddocks ps tb minstalled deps
              let finalAllInOne = not (isAdrToInstall adr && splitRequired)
              -- FIXME: this redundantly adds the deps (but they'll all just
              -- get looked up in the map)
              addFinal lp tb finalAllInOne False
              pure $ Right adr
            Left _ -> do
              -- Reset the state to how it was before attempting to find an
              -- all-in-one build plan.
              logDebugPlanS "installPackage" $
                   "Before trying cyclic plan, resetting lib result map to: "
                <> fromString (show libMap)
              put libMap
              -- Otherwise, fall back on building the tests / benchmarks in a
              -- separate step.
              res' <- resolveDepsAndInstall
                False lp.buildHaddocks ps lp.package minstalled
              when (isRight res') $ do
                -- Insert it into the map so that it's available for addFinal.
                updateLibMap name res'
                addFinal lp tb False False
              pure res'
 where
  expectedTestOrBenchFailures maybeCurator = fromMaybe False $ do
    curator <- maybeCurator
    pure $  Set.member name curator.expectTestFailure
         || Set.member name curator.expectBenchmarkFailure

resolveDepsAndInstall ::
     Bool
     -- ^ will the build step also build any tests?
  -> Bool
     -- ^ Should Haddock documentation be built?
  -> PackageSource
  -> Package
  -> Maybe Installed
  -> M (Either ConstructPlanException AddDepRes)
resolveDepsAndInstall isAllInOne buildHaddocks ps package minstalled = do
  res <- addPackageDeps package
  case res of
    Left err -> pure $ Left err
    Right deps ->
      Right <$>
        installPackageGivenDeps
          isAllInOne buildHaddocks ps package minstalled deps

-- | Checks if we need to install the given 'Package', given the results of
-- 'addPackageDeps'. If dependencies are missing, the package is dirty, or it is
-- not installed, then it needs to be installed.
installPackageGivenDeps ::
     Bool
     -- ^ will the build step also build any tests?
  -> Bool
     -- ^ Should Haddock documentation be built?
  -> PackageSource
  -> Package
  -> Maybe Installed
  -> MissingPresentDeps
  -> M AddDepRes
installPackageGivenDeps allInOne buildHaddocks ps package minstalled
  (MissingPresentDeps missing present minMutable) = do
    let name = package.name
    mRightVersionInstalled <- case minstalled of
      Just installed -> if Set.null missing
        then do
          shouldInstall <-
            checkDirtiness ps installed package present buildHaddocks
          pure $ if shouldInstall then Nothing else Just installed
        else do
          let packageNameText = T.pack . packageNameString . pkgName
              t = T.intercalate ", " $ map packageNameText (Set.toList missing)
          tell mempty
            { wDirty =
                Map.singleton name $ "missing dependencies: " <> addEllipsis t
            }
          pure Nothing
      Nothing -> pure Nothing
    ctx <- ask
    let loc = psLocation ps
        isMutable = installLocationIsMutable loc <> minMutable
        pkgConfigOpts = ConfigureOpts.packageConfigureOptsFromPackage package
        configOpts = TaskConfigOpts
            { missing
            , envConfig = ctx.ctxEnvConfig
            , baseConfigOpts = ctx.baseConfigOpts
            , isLocalNonExtraDep = psLocal ps
            , isMutable
            , pkgConfigOpts
            }
    pure $ case mRightVersionInstalled of
      Just installed -> ADRFound loc installed
      Nothing -> ADRToInstall Task
        { configOpts
        , buildHaddocks
        , present
        , taskType =
            case ps of
              PSFilePath lp ->
                TTLocalMutable lp
              PSRemote pkgLoc _version _fromSnapshot _cp ->
                TTRemotePackage isMutable package pkgLoc
        , allInOne
        , cachePkgSrc = toCachePkgSrc ps
        , buildTypeConfig = packageBuildTypeConfig package
        }

-- | Is the build type of the package Configure
packageBuildTypeConfig :: Package -> Bool
packageBuildTypeConfig pkg = pkg.buildType == Configure

-- Update response in the library map. If it is an error, and there's already an
-- error about cyclic dependencies, prefer the cyclic error.
updateLibMap :: PackageName -> Either ConstructPlanException AddDepRes -> M ()
updateLibMap name val = modify $ \mp ->
  case (Map.lookup name mp, val) of
    (Just (Left DependencyCycleDetected{}), Left _) -> mp
    _ -> Map.insert name val mp

addEllipsis :: Text -> Text
addEllipsis t
  | T.length t < 100 = t
  | otherwise = T.take 97 t <> "..."

-- | Given a package, recurses into all of its dependencies. The resulting
-- triple indicates: (1) which packages are missing. This means that their
-- 'GhcPkgId's will be figured out during the build, after they've been built;
-- (2) the packages that are already installed and which will be used; and
-- (3) whether the package itself is mutable or immutable.
addPackageDeps ::
     Package
  -> M (Either ConstructPlanException MissingPresentDeps)
addPackageDeps package = do
  checkAndWarnForUnknownTools package
  let pkgId = packageIdentifier package
  result <- processPackageDepsEither package (processDep pkgId)
  pure $ case result of
    -- Note that the Monoid for 'IsMutable' means that if any is 'Mutable',
    -- the result is 'Mutable'. Otherwise the result is 'Immutable'.
    Right v -> Right v
    Left errs ->
      Left $ DependencyPlanFailures package errs

-- | Given a dependency, yields either information for an error message or a
-- triple indicating: (1) if the dependency is to be installed, its package
-- identifier; (2) if the dependency is installed and a library, its package
-- identifier and 'GhcPkgId'; and (3) if the dependency is, or will be when
-- installed, mutable or immutable.
processDep ::
     PackageIdentifier
     -- ^ The package which has the dependency being processed.
  -> PackageName
     -- ^ The name of the dependency.
  -> DepValue
     -- ^ The version range and dependency type of the dependency.
  -> M ( Either
           ( Map
               PackageName
               (VersionRange, Maybe (Version, BlobKey), BadDependency)
           )
           MissingPresentDeps
       )
processDep pkgId name value = do
  eRes <- getCachedDepOrAddDep name
  let failure mLatestApp err =
        Left $ Map.singleton name (range, mLatestApp, err)
  case eRes of
    Left e -> do
      addParent
      let bd = case e of
            UnknownPackage name' -> assert (name' == name) NotInBuildPlan
            DependencyCycleDetected names -> BDDependencyCycleDetected names
            -- Ultimately we won't show any information on this to the user;
            -- we'll allow the dependency failures alone to display to avoid
            -- spamming the user too much.
            DependencyPlanFailures _ _  ->
              Couldn'tResolveItsDependencies version
      mLatestApplicable <- getLatestApplicableVersionAndRev name range
      pure $ failure mLatestApplicable bd
    Right adr
      | isDepTypeLibrary value.depType && not (adrHasLibrary adr) ->
          pure $ failure Nothing HasNoLibrary
    Right adr -> do
      addParent
      inRange <- adrInRange pkgId name range adr
      if inRange
        then pure $ Right $ processAdr adr
        else do
          mLatestApplicable <- getLatestApplicableVersionAndRev name range
          pure $ failure mLatestApplicable (DependencyMismatch $ adrVersion adr)
 where
  range = value.versionRange
  version = pkgVersion pkgId
  -- Update the parents map, for later use in plan construction errors
  -- - see 'getShortestDepsPath'.
  addParent =
    let parentMap = Map.singleton name [(pkgId, range)]
    in  tell mempty { wParents = MonoidMap parentMap }

getLatestApplicableVersionAndRev ::
     PackageName
  -> VersionRange
  -> M (Maybe (Version, BlobKey))
getLatestApplicableVersionAndRev name range = do
  ctx <- ask
  vsAndRevs <- runRIO ctx $
    getHackagePackageVersions YesRequireHackageIndex UsePreferredVersions name
  pure $ do
    lappVer <- latestApplicableVersion range $ Map.keysSet vsAndRevs
    revs <- Map.lookup lappVer vsAndRevs
    (cabalHash, _) <- Map.maxView revs
    Just (lappVer, cabalHash)

-- | Function to determine whether the result of 'addDep' is within range, given
-- the version range of the dependency and taking into account Stack's
-- @allow-newer@ configuration.
adrInRange ::
     PackageIdentifier
     -- ^ The package which has the dependency.
  -> PackageName
     -- ^ The name of the dependency.
  -> VersionRange
     -- ^ The version range of the dependency.
  -> AddDepRes
     -- ^ The result of 'addDep'.
  -> M Bool
adrInRange pkgId name range adr = if adrVersion adr `withinRange` range
  then pure True
  else do
    config <- view configL
    allowNewerCLI <- view $ envConfigL . to (.buildOptsCLI) . to (.allowNewer)
    let allowNewerConfig = config.allowNewer
        allowNewer = fromFirst False $ allowNewerCLI <> allowNewerConfig
        allowNewerDeps = config.allowNewerDeps
    if allowNewer
      then case allowNewerDeps of
        Nothing -> do
          warn_ True $
            fillSep
              [ style Shell "allow-newer"
              , "enabled"
              ]
          pure True
        Just boundsIgnoredDeps -> do
          let pkgName' = fromPackageName pkgName
              isBoundsIgnoreDep = pkgName `elem` boundsIgnoredDeps
              reason = if isBoundsIgnoreDep
                then fillSep
                  [ style Current pkgName'
                  , flow "is an"
                  , style Shell "allow-newer-dep"
                  , flow "and"
                  , style Shell "allow-newer"
                  , "enabled"
                  ]
                else fillSep
                  [ style Current pkgName'
                  , flow "is not an"
                  , style Shell "allow-newer-dep"
                  , flow "although"
                  , style Shell "allow-newer"
                  , "enabled"
                  ]
          warn_ isBoundsIgnoreDep reason
          pure isBoundsIgnoreDep
      else do
        when (isJust allowNewerDeps) $
          warn_ False $
            fillSep
              [ "although"
              , style Shell "allow-newer-deps"
              , flow "are specified,"
              , style Shell "allow-newer"
              , "is"
              , style Shell "false"
              ]
        -- We ignore dependency information for packages in a snapshot
        pkgInSnapshot <- inSnapshot pkgName version
        adrInSnapshot <- inSnapshot name (adrVersion adr)
        if pkgInSnapshot && adrInSnapshot
          then do
            warn_ True
              ( flow "trusting snapshot over Cabal file dependency \
                     \information"
              )
            pure True
          else pure False
 where
  PackageIdentifier pkgName version = pkgId
  warn_ isIgnoring reason = tell mempty { wWarnings = (msg:) }
   where
    msg = fillSep
            [ if isIgnoring
                then "Ignoring"
                else flow "Not ignoring"
            , style Current (fromPackageName pkgName) <> "'s"
            , flow "bounds on"
            , style Current (fromPackageName name)
            , parens (fromString . T.unpack $ versionRangeText range)
            , flow "and using"
            , style
                Current
                (fromPackageId $ PackageIdentifier name (adrVersion adr)) <> "."
            ]
       <> line
       <> fillSep
            [ "Reason:"
            , reason <> "."
            ]

-- | Given a result of 'addDep', yields a triple indicating: (1) if the
-- dependency is to be installed, its package identifier; (2) if the dependency
-- is installed and a library, its package identifier and 'GhcPkgId'; and (3) if
-- the dependency is, or will be when installed, mutable or immutable.
processAdr ::
     AddDepRes
  -> MissingPresentDeps
processAdr adr = case adr of
  ADRToInstall task ->
    MissingPresentDeps
      { missingPackages = Set.singleton $ taskProvides task
      , presentPackages = mempty
      , isMutable = taskTargetIsMutable task
      }
  ADRFound loc installed ->
    MissingPresentDeps
      { missingPackages = mempty
      , presentPackages = presentPackagesV
      , isMutable = installLocationIsMutable loc
      }
   where
    presentPackagesV = case installed of
      Library ident installedInfo -> installedMapGhcPkgId ident installedInfo
      _ -> Map.empty

checkDirtiness ::
     PackageSource
  -> Installed
  -> Package
  -> Map PackageIdentifier GhcPkgId
  -> Bool
     -- ^ Is Haddock documentation being built?
  -> M Bool
checkDirtiness ps installed package present buildHaddocks = do
  ctx <- ask
  moldOpts <- runRIO ctx $ tryGetFlagCache installed
  let packageConfigureOpt =
        ConfigureOpts.packageConfigureOptsFromPackage package
      configureOpts = ConfigureOpts.configureOpts
        (view envConfigL ctx)
        ctx.baseConfigOpts
        present
        (psLocal ps)
        (installLocationIsMutable $ psLocation ps) -- should be Local i.e. mutable always
        packageConfigureOpt
      components = case ps of
        PSFilePath lp ->
          Set.map (encodeUtf8 . renderComponent) lp.components
        PSRemote{} -> Set.empty
      wantConfigCache = ConfigCache
        { configureOpts
        , deps = Set.fromList $ Map.elems present
        , components
        , buildHaddocks
        , pkgSrc = toCachePkgSrc ps
        , pathEnvVar = ctx.pathEnvVar
        }
      config = view configL ctx
  mreason <-
    case moldOpts of
      Nothing -> pure $ Just "old configure information not found"
      Just oldOpts
        | Just reason <- describeConfigDiff config oldOpts wantConfigCache ->
            pure $ Just reason
        | True <- psForceDirty ps -> pure $ Just "--force-dirty specified"
        | otherwise -> do
            dirty <- psDirty ps
            pure $
              case dirty of
                Just files -> Just $
                     "local file changes: "
                  <> addEllipsis (T.pack $ unwords $ Set.toList files)
                Nothing -> Nothing
  case mreason of
    Nothing -> pure False
    Just reason -> do
      tell mempty { wDirty = Map.singleton package.name reason }
      pure True

describeConfigDiff :: Config -> ConfigCache -> ConfigCache -> Maybe Text
describeConfigDiff config old new
  | old.pkgSrc /= new.pkgSrc = Just $
      "switching from " <>
      pkgSrcName old.pkgSrc <> " to " <>
      pkgSrcName new.pkgSrc
  | not (new.deps `Set.isSubsetOf` old.deps) =
      Just "dependencies changed"
  | not $ Set.null newComponents =
      Just $ "components added: " `T.append` T.intercalate ", "
          (map (decodeUtf8With lenientDecode) (Set.toList newComponents))
  | not old.buildHaddocks && new.buildHaddocks =
      Just "rebuilding with haddocks"
  | oldOpts /= newOpts = Just $ T.pack $ concat
      [ "flags changed from "
      , show oldOpts
      , " to "
      , show newOpts
      ]
  | otherwise = Nothing
 where
  stripGhcOptions = go
   where
    go [] = []
    go ("--ghc-option":x:xs) = go' Ghc x xs
    go ("--ghc-options":x:xs) = go' Ghc x xs
    go ((T.stripPrefix "--ghc-option=" -> Just x):xs) = go' Ghc x xs
    go ((T.stripPrefix "--ghc-options=" -> Just x):xs) = go' Ghc x xs
    go (x:xs) = x : go xs

    go' wc x xs = checkKeepers wc x $ go xs

    checkKeepers wc x xs =
      case filter isKeeper $ T.words x of
        [] -> xs
        keepers -> T.pack (compilerOptionsCabalFlag wc) : T.unwords keepers : xs

    -- GHC options which affect build results and therefore should always force
    -- a rebuild
    --
    -- For the most part, we only care about options generated by Stack itself
    isKeeper = (== "-fhpc") -- more to be added later

  userOpts = filter (not . isStackOpt)
           . (if config.rebuildGhcOptions
                then id
                else stripGhcOptions)
           . map T.pack
           . ConfigureOpts.renderConfigureOpts
           . (.configureOpts)
   where
    -- options set by Stack
    isStackOpt :: Text -> Bool
    isStackOpt t = any (`T.isPrefixOf` t)
      [ "--dependency="
      , "--constraint="
      , "--package-db="
      , "--libdir="
      , "--bindir="
      , "--datadir="
      , "--libexecdir="
      , "--sysconfdir"
      , "--docdir="
      , "--htmldir="
      , "--haddockdir="
      , "--enable-tests"
      , "--enable-benchmarks"
      , "--exact-configuration"
      -- Treat these as causing dirtiness, to resolve
      -- https://github.com/commercialhaskell/stack/issues/2984
      --
      -- , "--enable-library-profiling"
      -- , "--enable-executable-profiling"
      -- , "--enable-profiling"
      ] || t == "--user"

  (oldOpts, newOpts) = removeMatching (userOpts old) (userOpts new)

  removeMatching (x:xs) (y:ys)
    | x == y = removeMatching xs ys
  removeMatching xs ys = (xs, ys)

  newComponents =
    new.components `Set.difference` old.components

  pkgSrcName (CacheSrcLocal fp) = T.pack fp
  pkgSrcName CacheSrcUpstream = "upstream source"

psForceDirty :: PackageSource -> Bool
psForceDirty (PSFilePath lp) = lp.forceDirty
psForceDirty PSRemote{} = False

psDirty ::
     (MonadIO m, HasEnvConfig env, MonadReader env m)
  => PackageSource
  -> m (Maybe (Set FilePath))
psDirty (PSFilePath lp) = runMemoizedWith lp.dirtyFiles
psDirty PSRemote {} = pure Nothing -- files never change in a remote package

psLocal :: PackageSource -> Bool
psLocal (PSFilePath _ ) = True
psLocal PSRemote{} = False

psLocation :: PackageSource -> InstallLocation
psLocation (PSFilePath _) = Local
psLocation PSRemote{} = Snap

-- | For the given package, warn about any unknown tools that are not on the
-- PATH and not one of the executables of the package.
checkAndWarnForUnknownTools :: Package -> M ()
checkAndWarnForUnknownTools p = do
  let unknownTools = Set.toList $ packageUnknownTools p
  -- Check whether the tool is on the PATH or a package executable before
  -- warning about it.
  warnings <-
    fmap catMaybes $ forM unknownTools $ \toolName ->
      runMaybeT $ notOnPath toolName *> notPackageExe toolName *> warn toolName
  tell mempty { wWarnings = (map toolWarningText warnings ++) }
 where
  -- From Cabal 2.0, build-tools can specify a pre-built executable that should
  -- already be on the PATH.
  notOnPath toolName = MaybeT $ do
    let settings = minimalEnvSettings { includeLocals = True }
    config <- view configL
    menv <- liftIO $ config.processContextSettings settings
    eFound <- runRIO menv $ findExecutable $ T.unpack toolName
    skipIf $ isRight eFound
  -- From Cabal 1.12, build-tools can specify another executable in the same
  -- package.
  notPackageExe toolName =
    MaybeT $ skipIf $
      collectionMember (unqualCompFromText toolName) p.executables
  warn name = MaybeT . pure . Just $ ToolWarning (ExeName name) p.name
  skipIf p' = pure $ if p' then Nothing else Just ()

toolWarningText :: ToolWarning -> StyleDoc
toolWarningText (ToolWarning (ExeName toolName) pkgName') = fillSep
  [ flow "No packages found in snapshot which provide a"
  , style PkgComponent (fromString $ show toolName)
  , flow "executable, which is a build-tool dependency of"
  , style Current (fromPackageName pkgName')
  ]

-- | Is the given package/version combo defined in the snapshot or in the global
-- database?
inSnapshot :: PackageName -> Version -> M Bool
inSnapshot name version = do
  ctx <- ask
  pure $ fromMaybe False $ do
    ps <- Map.lookup name ctx.combinedMap
    case ps of
      PIOnlySource (PSRemote _ srcVersion FromSnapshot _) ->
        pure $ srcVersion == version
      PIBoth (PSRemote _ srcVersion FromSnapshot _) _ ->
        pure $ srcVersion == version
      -- OnlyInstalled occurs for global database
      PIOnlyInstalled loc (Library pid _) ->
        assert (loc == Snap) $
        assert (pkgVersion pid == version) $
        Just True
      _ -> pure False

-- TODO: Consider intersecting version ranges for multiple deps on a
-- package.  This is why VersionRange is in the parent map.

logDebugPlanS ::
     (HasCallStack, HasRunner env, MonadIO m, MonadReader env m)
  => LogSource
  -> Utf8Builder
  -> m ()
logDebugPlanS s msg = do
  debugPlan <- view $ globalOptsL . to (.planInLog)
  when debugPlan $ logDebugS s msg

-- | A function to yield a 'PackageInfo' value from: (1) a 'PackageSource'
-- value; and (2) a pair of an 'InstallLocation' value and an 'Installed' value.
-- Checks that the version of the 'PackageSource' value and the version of the
-- `Installed` value are the same.
combineSourceInstalled :: PackageSource
                       -> (InstallLocation, Installed)
                       -> PackageInfo
combineSourceInstalled ps (location, installed) =
  assert (psVersion ps == installedVersion installed) $
    case location of
      -- Always trust something in the snapshot
      Snap -> PIOnlyInstalled location installed
      Local -> PIBoth ps installed

-- | A function to yield a 'CombinedMap' value from: (1) a dictionary of package
-- names, and where the source code of the named package is located; and (2) an
-- 'InstalledMap' value.
combineMap :: Map PackageName PackageSource -> InstalledMap -> CombinedMap
combineMap = Map.merge
  (Map.mapMissing (\_ s -> PIOnlySource s))
  (Map.mapMissing (\_ i -> uncurry PIOnlyInstalled i))
  (Map.zipWithMatched (\_ s i -> combineSourceInstalled s i))
