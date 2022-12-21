{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Construct a @Plan@ for how to build
module Stack.Build.ConstructPlan
  ( constructPlan
  ) where

import           Control.Monad.RWS.Strict hiding ( (<>) )
import           Control.Monad.State.Strict ( execState )
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import           Data.Monoid.Map ( MonoidMap(..) )
import qualified Data.Set as Set
import qualified Data.Text as T
import           Distribution.Types.BuildType ( BuildType (Configure) )
import           Distribution.Types.PackageName ( mkPackageName )
import           Generics.Deriving.Monoid ( memptydefault, mappenddefault )
import           Path ( parent )
import           RIO.Process ( HasProcessContext (..), findExecutable )
import           Stack.Build.Cache ( tryGetFlagCache )
import           Stack.Build.Haddock ( shouldHaddockDeps )
import           Stack.Build.Source ( loadLocalPackage )
import           Stack.Constants ( compilerOptionsCabalFlag )
import           Stack.Package ( applyForceCustomBuild )
import           Stack.Prelude hiding ( loadPackage )
import           Stack.SourceMap ( getPLIVersion, mkProjectPackage )
import           Stack.Types.Build
                   ( BaseConfigOpts (..), BadDependency (..)
                   , BuildException (..), BuildPrettyException (..)
                   , CachePkgSrc (..), ConfigCache (..), ConfigureOpts (..)
                   , ConstructPlanException (..), IsMutable (..), ParentMap
                   , Plan (..), Task (..), TaskConfigOpts (..), TaskType (..)
                   , configureOpts, installLocationIsMutable, isStackOpt
                   , taskIsTarget, taskLocation, taskTargetIsMutable
                   , toCachePkgSrc
                   )
import           Stack.Types.Compiler ( WhichCompiler (..) )
import           Stack.Types.Config
                   ( BuildConfig (..), BuildOpts (..), BuildOptsCLI (..)
                   , BuildSubset (..), CompilerPaths (..), Config (..)
                   , Curator (..), DumpPackage (..), EnvConfig, EnvSettings (..)
                   , HasBuildConfig (..), HasCompiler (..), HasConfig (..)
                   , HasEnvConfig (..), HasGHCVariant, HasPlatform
                   , HasRunner (..), HasSourceMap (..), minimalEnvSettings
                   , stackRootL, stackYamlL
                   )
import           Stack.Types.Dependency
                   ( DepValue (DepValue), DepType (AsLibrary) )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.NamedComponent ( exeComponents, renderComponent )
import           Stack.Types.Package
                   ( ExeName (..), InstallLocation (..), Installed (..)
                   , InstalledMap, LocalPackage (..), Package (..)
                   , PackageLibraries (..), PackageSource (..), installedVersion
                   , packageIdentifier, psVersion, runMemoizedWith
                   )
import           Stack.Types.SourceMap
                   ( CommonPackage (..), DepPackage (..), FromSnapshot (..)
                   , GlobalPackage (..), SMTargets (..), SourceMap (..)
                   )
import           Stack.Types.Version
                   ( latestApplicableVersion, versionRangeText, withinRange )
import           System.Environment ( lookupEnv )
import           System.IO ( putStrLn )

data PackageInfo
  = PIOnlyInstalled InstallLocation Installed
    -- ^ This indicates that the package is already installed, and that we
    -- shouldn't build it from source. This is only the case for global
    -- packages.
  | PIOnlySource PackageSource
    -- ^ This indicates that the package isn't installed, and we know where to
    -- find its source.
  | PIBoth PackageSource Installed
    -- ^ This indicates that the package is installed and we know where to find
    -- its source. We may want to reinstall from source.
  deriving Show

combineSourceInstalled :: PackageSource
                       -> (InstallLocation, Installed)
                       -> PackageInfo
combineSourceInstalled ps (location, installed) =
  assert (psVersion ps == installedVersion installed) $
    case location of
      -- Always trust something in the snapshot
      Snap -> PIOnlyInstalled location installed
      Local -> PIBoth ps installed

type CombinedMap = Map PackageName PackageInfo

combineMap :: Map PackageName PackageSource -> InstalledMap -> CombinedMap
combineMap = Map.mergeWithKey
  (\_ s i -> Just $ combineSourceInstalled s i)
  (fmap PIOnlySource)
  (fmap (uncurry PIOnlyInstalled))

data AddDepRes
  = ADRToInstall Task
  | ADRFound InstallLocation Installed
  deriving Show

data W = W
  { wFinals :: !(Map PackageName (Either ConstructPlanException Task))
  , wInstall :: !(Map Text InstallLocation)
  -- ^ executable to be installed, and location where the binary is placed
  , wDirty :: !(Map PackageName Text)
  -- ^ why a local package is considered dirty
  , wWarnings :: !([Text] -> [Text])
  -- ^ Warnings
  , wParents :: !ParentMap
  -- ^ Which packages a given package depends on, along with the package's version
  } deriving Generic

instance Semigroup W where
  (<>) = mappenddefault

instance Monoid W where
  mempty = memptydefault
  mappend = (<>)

type M = RWST -- TODO replace with more efficient WS stack on top of StackT
  Ctx
  W
  (Map PackageName (Either ConstructPlanException AddDepRes))
  IO

data Ctx = Ctx
  { baseConfigOpts :: !BaseConfigOpts
  , loadPackage    :: !(  PackageLocationImmutable
                       -> Map FlagName Bool
                       -> [Text]
                       -> [Text]
                       -> M Package
                       )
  , combinedMap    :: !CombinedMap
  , ctxEnvConfig   :: !EnvConfig
  , callStack      :: ![PackageName]
  , wanted         :: !(Set PackageName)
  , localNames     :: !(Set PackageName)
  , mcurator       :: !(Maybe Curator)
  , pathEnvVar     :: !Text
  }

instance HasPlatform Ctx

instance HasGHCVariant Ctx

instance HasLogFunc Ctx where
  logFuncL = configL.logFuncL

instance HasRunner Ctx where
  runnerL = configL.runnerL

instance HasStylesUpdate Ctx where
  stylesUpdateL = runnerL.stylesUpdateL

instance HasTerm Ctx where
  useColorL = runnerL.useColorL
  termWidthL = runnerL.termWidthL

instance HasConfig Ctx

instance HasPantryConfig Ctx where
  pantryConfigL = configL.pantryConfigL

instance HasProcessContext Ctx where
  processContextL = configL.processContextL

instance HasBuildConfig Ctx

instance HasSourceMap Ctx where
  sourceMapL = envConfigL.sourceMapL

instance HasCompiler Ctx where
  compilerPathsL = envConfigL.compilerPathsL

instance HasEnvConfig Ctx where
  envConfigL = lens ctxEnvConfig (\x y -> x { ctxEnvConfig = y })

-- | Computes a build plan. This means figuring out which build 'Task's
-- to take, and the interdependencies among the build 'Task's. In
-- particular:
--
-- 1) It determines which packages need to be built, based on the
-- transitive deps of the current targets. For local packages, this is
-- indicated by the 'lpWanted' boolean. For extra packages to build,
-- this comes from the @extraToBuild0@ argument of type @Set
-- PackageName@. These are usually packages that have been specified on
-- the commandline.
--
-- 2) It will only rebuild an upstream package if it isn't present in
-- the 'InstalledMap', or if some of its dependencies have changed.
--
-- 3) It will only rebuild a local package if its files are dirty or
-- some of its dependencies have changed.
constructPlan ::
     forall env. HasEnvConfig env
  => BaseConfigOpts
  -> [DumpPackage] -- ^ locally registered
  -> (  PackageLocationImmutable
     -> Map FlagName Bool
     -> [Text]
     -> [Text]
     -> RIO EnvConfig Package
     )
     -- ^ load upstream package
  -> SourceMap
  -> InstalledMap
  -> Bool
  -> RIO env Plan
constructPlan baseConfigOpts0 localDumpPkgs loadPackage0 sourceMap installedMap initialBuildSteps = do
  logDebug "Constructing the build plan"

  when hasBaseInDeps $
    prettyWarn $
         flow "You are trying to upgrade/downgrade base, which is almost \
              \certainly not what you really want. Please, consider using \
              \another GHC version if you need a certain version of base, or \
              \removing base from extra-deps. See more at \
              \https://github.com/commercialhaskell/stack/issues/3940."
      <> line

  econfig <- view envConfigL
  globalCabalVersion <- view $ compilerPathsL.to cpCabalVersion
  sources <- getSources globalCabalVersion
  mcur <- view $ buildConfigL.to bcCurator

  let onTarget = void . addDep
  let inner = mapM_ onTarget $ Map.keys (smtTargets $ smTargets sourceMap)
  pathEnvVar' <- liftIO $ maybe mempty T.pack <$> lookupEnv "PATH"
  let ctx = mkCtx econfig globalCabalVersion sources mcur pathEnvVar'
  ((), m, W efinals installExes dirtyReason warnings parents) <-
    liftIO $ runRWST inner ctx M.empty
  mapM_ (logWarn . display) (warnings [])
  let toEither (_, Left e)  = Left e
      toEither (k, Right v) = Right (k, v)
      (errlibs, adrs) = partitionEithers $ map toEither $ M.toList m
      (errfinals, finals) = partitionEithers $ map toEither $ M.toList efinals
      errs = errlibs ++ errfinals
  if null errs
    then do
      let toTask (_, ADRFound _ _) = Nothing
          toTask (name, ADRToInstall task) = Just (name, task)
          tasks = M.fromList $ mapMaybe toTask adrs
          takeSubset =
            case boptsCLIBuildSubset $ bcoBuildOptsCLI baseConfigOpts0 of
              BSAll -> pure
              BSOnlySnapshot -> pure . stripLocals
              BSOnlyDependencies ->
                pure . stripNonDeps (M.keysSet $ smDeps sourceMap)
              BSOnlyLocals -> errorOnSnapshot
      takeSubset Plan
        { planTasks = tasks
        , planFinals = M.fromList finals
        , planUnregisterLocal =
            mkUnregisterLocal tasks dirtyReason localDumpPkgs initialBuildSteps
        , planInstallExes =
            if boptsInstallExes (bcoBuildOpts baseConfigOpts0) ||
               boptsInstallCompilerTool (bcoBuildOpts baseConfigOpts0)
                then installExes
                else Map.empty
        }
    else do
      planDebug $ show errs
      stackYaml <- view stackYamlL
      stackRoot <- view stackRootL
      throwM $ PrettyException $
        ConstructPlanFailed
          errs stackYaml stackRoot parents (wanted ctx) prunedGlobalDeps
 where
  hasBaseInDeps = Map.member (mkPackageName "base") (smDeps sourceMap)

  mkCtx econfig globalCabalVersion sources mcur pathEnvVar' = Ctx
    { baseConfigOpts = baseConfigOpts0
    , loadPackage = \w x y z -> runRIO econfig $
        applyForceCustomBuild globalCabalVersion <$> loadPackage0 w x y z
    , combinedMap = combineMap sources installedMap
    , ctxEnvConfig = econfig
    , callStack = []
    , wanted = Map.keysSet (smtTargets $ smTargets sourceMap)
    , localNames = Map.keysSet (smProject sourceMap)
    , mcurator = mcur
    , pathEnvVar = pathEnvVar'
    }

  prunedGlobalDeps = flip Map.mapMaybe (smGlobal sourceMap) $ \gp ->
    case gp of
      ReplacedGlobalPackage deps ->
        let pruned = filter (not . inSourceMap) deps
        in  if null pruned then Nothing else Just pruned
      GlobalPackage _ -> Nothing

  inSourceMap pname = pname `Map.member` smDeps sourceMap ||
                      pname `Map.member` smProject sourceMap

  getSources globalCabalVersion = do
    let loadLocalPackage' pp = do
          lp <- loadLocalPackage pp
          pure lp { lpPackage = applyForceCustomBuild globalCabalVersion $ lpPackage lp }
    pPackages <- for (smProject sourceMap) $ \pp -> do
      lp <- loadLocalPackage' pp
      pure $ PSFilePath lp
    bopts <- view $ configL.to configBuild
    deps <- for (smDeps sourceMap) $ \dp ->
      case dpLocation dp of
        PLImmutable loc ->
          pure $
            PSRemote loc (getPLIVersion loc) (dpFromSnapshot dp) (dpCommon dp)
        PLMutable dir -> do
          pp <- mkProjectPackage YesPrintWarnings dir (shouldHaddockDeps bopts)
          lp <- loadLocalPackage' pp
          pure $ PSFilePath lp
    pure $ pPackages <> deps

-- | Throw an exception if there are any snapshot packages in the plan.
errorOnSnapshot :: Plan -> RIO env Plan
errorOnSnapshot plan@(Plan tasks _finals _unregister installExes) = do
  let snapTasks = Map.keys $ Map.filter (\t -> taskLocation t == Snap) tasks
  let snapExes = Map.keys $ Map.filter (== Snap) installExes
  unless (null snapTasks && null snapExes) $ throwIO $
    NotOnlyLocal snapTasks snapExes
  pure plan

data NotOnlyLocal
  = NotOnlyLocal [PackageName] [Text]
  deriving (Show, Typeable)

instance Exception NotOnlyLocal where
  displayException (NotOnlyLocal packages exes) = concat
    [ "Error: [S-1727]\n"
    , "Specified only-locals, but I need to build snapshot contents:\n"
    , if null packages then "" else concat
        [ "Packages: "
        , L.intercalate ", " (map packageNameString packages)
        , "\n"
        ]
    , if null exes then "" else concat
        [ "Executables: "
        , L.intercalate ", " (map T.unpack exes)
        , "\n"
        ]
    ]

-- | State to be maintained during the calculation of local packages
-- to unregister.
data UnregisterState = UnregisterState
  { usToUnregister :: !(Map GhcPkgId (PackageIdentifier, Text))
  , usKeep :: ![DumpPackage]
  , usAnyAdded :: !Bool
  }

-- | Determine which packages to unregister based on the given tasks and
-- already registered local packages
mkUnregisterLocal ::
     Map PackageName Task
  -- ^ Tasks
  -> Map PackageName Text
  -- ^ Reasons why packages are dirty and must be rebuilt
  -> [DumpPackage]
  -- ^ Local package database dump
  -> Bool
  -- ^ If true, we're doing a special initialBuildSteps build - don't unregister
  -- target packages.
  -> Map GhcPkgId (PackageIdentifier, Text)
mkUnregisterLocal tasks dirtyReason localDumpPkgs initialBuildSteps =
  -- We'll take multiple passes through the local packages. This
  -- will allow us to detect that a package should be unregistered,
  -- as well as all packages directly or transitively depending on
  -- it.
  loop Map.empty localDumpPkgs
 where
  loop toUnregister keep
    -- If any new packages were added to the unregister Map, we need to loop
    -- through the remaining packages again to detect if a transitive dependency
    -- is being unregistered.
    | usAnyAdded us = loop (usToUnregister us) (usKeep us)
    -- Nothing added, so we've already caught them all. Return the Map we've
    -- already calculated.
    | otherwise = usToUnregister us
   where
    -- Run the unregister checking function on all packages we currently think
    -- we'll be keeping.
    us = execState (mapM_ go keep) UnregisterState
      { usToUnregister = toUnregister
      , usKeep = []
      , usAnyAdded = False
      }

  go dp = do
    us <- get
    case go' (usToUnregister us) ident deps of
      -- Not unregistering, add it to the keep list
      Nothing -> put us { usKeep = dp : usKeep us }
      -- Unregistering, add it to the unregister Map and indicate that a package
      -- was in fact added to the unregister Map so we loop again.
      Just reason -> put us
        { usToUnregister = Map.insert gid (ident, reason) (usToUnregister us)
        , usAnyAdded = True
        }
   where
    gid = dpGhcPkgId dp
    ident = dpPackageIdent dp
    deps = dpDepends dp

  go' toUnregister ident deps
    -- If we're planning on running a task on it, then it must be unregistered,
    -- unless it's a target and an initial-build-steps build is being done.
    | Just task <- Map.lookup name tasks
        = if initialBuildSteps && taskIsTarget task && taskProvides task == ident
            then Nothing
            else Just $ fromMaybe "" $ Map.lookup name dirtyReason
    -- Check if a dependency is going to be unregistered
    | (dep, _):_ <- mapMaybe (`Map.lookup` toUnregister) deps
        = Just $ "Dependency being unregistered: " <> T.pack (packageIdentifierString dep)
    -- None of the above, keep it!
    | otherwise = Nothing
    where
      name :: PackageName
      name = pkgName ident

-- | Given a 'LocalPackage' and its 'lpTestBench', adds a 'Task' for
-- running its tests and benchmarks.
--
-- If @isAllInOne@ is 'True', then this means that the build step will
-- also build the tests. Otherwise, this indicates that there's a cyclic
-- dependency and an additional build step needs to be done.
--
-- This will also add all the deps needed to build the tests /
-- benchmarks. If @isAllInOne@ is 'True' (the common case), then all of
-- these should have already been taken care of as part of the build
-- step.
addFinal :: LocalPackage -> Package -> Bool -> Bool -> M ()
addFinal lp package isAllInOne buildHaddocks = do
  depsRes <- addPackageDeps package
  res <- case depsRes of
    Left e -> pure $ Left e
    Right (missing, present, _minLoc) -> do
      ctx <- ask
      pure $ Right Task
        { taskProvides = PackageIdentifier
            (packageName package)
            (packageVersion package)
        , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
            let allDeps = Map.union present missing'
            in  configureOpts
                  (view envConfigL ctx)
                  (baseConfigOpts ctx)
                  allDeps
                  True -- local
                  Mutable
                  package
        , taskBuildHaddock = buildHaddocks
        , taskPresent = present
        , taskType = TTLocalMutable lp
        , taskAllInOne = isAllInOne
        , taskCachePkgSrc = CacheSrcLocal (toFilePath (parent (lpCabalFile lp)))
        , taskAnyMissing = not $ Set.null missing
        , taskBuildTypeConfig = packageBuildTypeConfig package
        }
  tell mempty { wFinals = Map.singleton (packageName package) res }

-- | Given a 'PackageName', adds all of the build tasks to build the
-- package, if needed.
--
-- 'constructPlan' invokes this on all the target packages, setting
-- @treatAsDep'@ to False, because those packages are direct build
-- targets. 'addPackageDeps' invokes this while recursing into the
-- dependencies of a package. As such, it sets @treatAsDep'@ to True,
-- forcing this package to be marked as a dependency, even if it is
-- directly wanted. This makes sense - if we left out packages that are
-- deps, it would break the --only-dependencies build plan.
addDep :: PackageName
       -> M (Either ConstructPlanException AddDepRes)
addDep name = do
  ctx <- ask
  m <- get
  case Map.lookup name m of
    Just res -> do
      planDebug $
        "addDep: Using cached result for " ++ show name ++ ": " ++ show res
      pure res
    Nothing -> do
      res <- if name `elem` callStack ctx
        then do
          planDebug $
               "addDep: Detected cycle "
            <> show name
            <> ": "
            <> show (callStack ctx)
          pure $ Left $ DependencyCycleDetected $ name : callStack ctx
        else local (\ctx' -> ctx' { callStack = name : callStack ctx' }) $ do
          let mpackageInfo = Map.lookup name $ combinedMap ctx
          planDebug $
               "addDep: Package info for "
            <> show name
            <> ": "
            <> show mpackageInfo
          case mpackageInfo of
            -- TODO look up in the package index and see if there's a
            -- recommendation available
            Nothing -> pure $ Left $ UnknownPackage name
            Just (PIOnlyInstalled loc installed) -> do
              -- FIXME Slightly hacky, no flags since
              -- they likely won't affect executable
              -- names. This code does not feel right.
              let version = installedVersion installed
                  askPkgLoc = liftRIO $ do
                    mrev <- getLatestHackageRevision
                              YesRequireHackageIndex name version
                    case mrev of
                      Nothing -> do
                        -- this could happen for GHC boot libraries missing from Hackage
                        logWarn $
                             "No latest package revision found for: "
                          <> fromString (packageNameString name)
                          <> ", dependency callstack: "
                          <> displayShow (map packageNameString $ callStack ctx)
                        pure Nothing
                      Just (_rev, cfKey, treeKey) ->
                        pure . Just $
                          PLIHackage (PackageIdentifier name version) cfKey treeKey
              tellExecutablesUpstream name askPkgLoc loc Map.empty
              pure $ Right $ ADRFound loc installed
            Just (PIOnlySource ps) -> do
              tellExecutables name ps
              installPackage name ps Nothing
            Just (PIBoth ps installed) -> do
              tellExecutables name ps
              installPackage name ps (Just installed)
      updateLibMap name res
      pure res

-- FIXME what's the purpose of this? Add a Haddock!
tellExecutables :: PackageName -> PackageSource -> M ()
tellExecutables _name (PSFilePath lp)
  | lpWanted lp = tellExecutablesPackage Local $ lpPackage lp
  | otherwise = pure ()
-- Ignores ghcOptions because they don't matter for enumerating
-- executables.
tellExecutables name (PSRemote pkgloc _version _fromSnapshot cp) =
  tellExecutablesUpstream name (pure $ Just pkgloc) Snap (cpFlags cp)

tellExecutablesUpstream ::
     PackageName
  -> M (Maybe PackageLocationImmutable)
  -> InstallLocation
  -> Map FlagName Bool
  -> M ()
tellExecutablesUpstream name retrievePkgLoc loc flags = do
  ctx <- ask
  when (name `Set.member` wanted ctx) $ do
    mPkgLoc <- retrievePkgLoc
    forM_ mPkgLoc $ \pkgLoc -> do
      p <- loadPackage ctx pkgLoc flags [] []
      tellExecutablesPackage loc p

tellExecutablesPackage :: InstallLocation -> Package -> M ()
tellExecutablesPackage loc p = do
  cm <- asks combinedMap
  -- Determine which components are enabled so we know which ones to copy
  let myComps =
        case Map.lookup (packageName p) cm of
          Nothing -> assert False Set.empty
          Just (PIOnlyInstalled _ _) -> Set.empty
          Just (PIOnlySource ps) -> goSource ps
          Just (PIBoth ps _) -> goSource ps

      goSource (PSFilePath lp)
        | lpWanted lp = exeComponents (lpComponents lp)
        | otherwise = Set.empty
      goSource PSRemote{} = Set.empty

  tell mempty
    { wInstall = Map.fromList $
        map (, loc) $ Set.toList $ filterComps myComps $ packageExes p
    }
 where
  filterComps myComps x
    | Set.null myComps = x
    | otherwise = Set.intersection x myComps

-- | Given a 'PackageSource' and perhaps an 'Installed' value, adds
-- build 'Task's for the package and its dependencies.
installPackage :: PackageName
               -> PackageSource
               -> Maybe Installed
               -> M (Either ConstructPlanException AddDepRes)
installPackage name ps minstalled = do
  ctx <- ask
  case ps of
    PSRemote pkgLoc _version _fromSnapshot cp -> do
      planDebug $
           "installPackage: Doing all-in-one build for upstream package "
        <> show name
      package <- loadPackage
        ctx pkgLoc (cpFlags cp) (cpGhcOptions cp) (cpCabalConfigOpts cp)
      resolveDepsAndInstall True (cpHaddocks cp) ps package minstalled
    PSFilePath lp -> do
      case lpTestBench lp of
        Nothing -> do
          planDebug $
               "installPackage: No test / bench component for "
            <> show name
            <> " so doing an all-in-one build."
          resolveDepsAndInstall
            True (lpBuildHaddocks lp) ps (lpPackage lp) minstalled
        Just tb -> do
          -- Attempt to find a plan which performs an all-in-one build. Ignore
          -- the writer action + reset the state if it fails.
          s <- get
          res <- pass $ do
            res <- addPackageDeps tb
            let writerFunc w = case res of
                  Left _ -> mempty
                  _ -> w
            pure (res, writerFunc)
          case res of
            Right deps -> do
              planDebug $
                   "installPackage: For "
                <> show name
                <> ", successfully added package deps"
              -- in curator builds we can't do all-in-one build as
              -- test/benchmark failure could prevent library from being
              -- available to its dependencies but when it's already available
              -- it's OK to do that
              splitRequired <- expectedTestOrBenchFailures <$> asks mcurator
              let isAllInOne = not splitRequired
              adr <- installPackageGivenDeps
                isAllInOne (lpBuildHaddocks lp) ps tb minstalled deps
              let finalAllInOne = case adr of
                    ADRToInstall _ | splitRequired -> False
                    _ -> True
              -- FIXME: this redundantly adds the deps (but they'll all just
              -- get looked up in the map)
              addFinal lp tb finalAllInOne False
              pure $ Right adr
            Left _ -> do
              -- Reset the state to how it was before attempting to find an
              -- all-in-one build plan.
              planDebug $
                   "installPackage: Before trying cyclic plan, resetting lib \
                   \result map to "
                <> show s
              put s
              -- Otherwise, fall back on building the
              -- tests / benchmarks in a separate step.
              res' <- resolveDepsAndInstall
                False (lpBuildHaddocks lp) ps (lpPackage lp) minstalled
              when (isRight res') $ do
                -- Insert it into the map so that it's
                -- available for addFinal.
                updateLibMap name res'
                addFinal lp tb False False
              pure res'
 where
  expectedTestOrBenchFailures maybeCurator = fromMaybe False $ do
    curator <- maybeCurator
    pure $ Set.member name (curatorExpectTestFailure curator) ||
           Set.member name (curatorExpectBenchmarkFailure curator)

resolveDepsAndInstall :: Bool
                      -> Bool
                      -> PackageSource
                      -> Package
                      -> Maybe Installed
                      -> M (Either ConstructPlanException AddDepRes)
resolveDepsAndInstall isAllInOne buildHaddocks ps package minstalled = do
  res <- addPackageDeps package
  case res of
    Left err -> pure $ Left err
    Right deps ->
      liftM Right $
        installPackageGivenDeps
          isAllInOne buildHaddocks ps package minstalled deps

-- | Checks if we need to install the given 'Package', given the results
-- of 'addPackageDeps'. If dependencies are missing, the package is
-- dirty, or it's not installed, then it needs to be installed.
installPackageGivenDeps :: Bool
                        -> Bool
                        -> PackageSource
                        -> Package
                        -> Maybe Installed
                        -> ( Set PackageIdentifier
                           , Map PackageIdentifier GhcPkgId
                           , IsMutable )
                        -> M AddDepRes
installPackageGivenDeps isAllInOne buildHaddocks ps package minstalled
  (missing, present, minMutable) = do
    let name = packageName package
    ctx <- ask
    mRightVersionInstalled <- case (minstalled, Set.null missing) of
      (Just installed, True) -> do
        shouldInstall <-
          checkDirtiness ps installed package present buildHaddocks
        pure $ if shouldInstall then Nothing else Just installed
      (Just _, False) -> do
        let t = T.intercalate ", " $
                  map (T.pack . packageNameString . pkgName) (Set.toList missing)
        tell mempty
          { wDirty =
              Map.singleton name $ "missing dependencies: " <> addEllipsis t
          }
        pure Nothing
      (Nothing, _) -> pure Nothing
    let loc = psLocation ps
        mutable = installLocationIsMutable loc <> minMutable
    pure $ case mRightVersionInstalled of
      Just installed -> ADRFound loc installed
      Nothing -> ADRToInstall Task
        { taskProvides = PackageIdentifier
            (packageName package)
            (packageVersion package)
        , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
            let allDeps = Map.union present missing'
            in  configureOpts
                  (view envConfigL ctx)
                  (baseConfigOpts ctx)
                  allDeps
                  (psLocal ps)
                  mutable
                  package
        , taskBuildHaddock = buildHaddocks
        , taskPresent = present
        , taskType =
            case ps of
              PSFilePath lp ->
                TTLocalMutable lp
              PSRemote pkgLoc _version _fromSnapshot _cp ->
                TTRemotePackage mutable package pkgLoc
        , taskAllInOne = isAllInOne
        , taskCachePkgSrc = toCachePkgSrc ps
        , taskAnyMissing = not $ Set.null missing
        , taskBuildTypeConfig = packageBuildTypeConfig package
        }

-- | Is the build type of the package Configure
packageBuildTypeConfig :: Package -> Bool
packageBuildTypeConfig pkg = packageBuildType pkg == Configure

-- Update response in the lib map. If it is an error, and there's
-- already an error about cyclic dependencies, prefer the cyclic error.
updateLibMap :: PackageName -> Either ConstructPlanException AddDepRes -> M ()
updateLibMap name val = modify $ \mp ->
  case (M.lookup name mp, val) of
    (Just (Left DependencyCycleDetected{}), Left _) -> mp
    _ -> M.insert name val mp

addEllipsis :: Text -> Text
addEllipsis t
  | T.length t < 100 = t
  | otherwise = T.take 97 t <> "..."

-- | Given a package, recurses into all of its dependencies. The results
-- indicate which packages are missing, meaning that their 'GhcPkgId's
-- will be figured out during the build, after they've been built. The
-- 2nd part of the tuple result indicates the packages that are already
-- installed which will be used.
--
-- The 3rd part of the tuple is an 'InstallLocation'. If it is 'Local',
-- then the parent package must be installed locally. Otherwise, if it
-- is 'Snap', then it can either be installed locally or in the
-- snapshot.
addPackageDeps ::
     Package
  -> M ( Either
           ConstructPlanException
           ( Set PackageIdentifier
           , Map PackageIdentifier GhcPkgId
           , IsMutable
           )
       )
addPackageDeps package = do
  ctx <- ask
  checkAndWarnForUnknownTools package
  let deps' = packageDeps package
  deps <- forM (Map.toList deps') $ \(depname, DepValue range depType) -> do
    eres <- addDep depname
    let getLatestApplicableVersionAndRev :: M (Maybe (Version, BlobKey))
        getLatestApplicableVersionAndRev = do
          vsAndRevs <-
            runRIO ctx $
              getHackagePackageVersions
                YesRequireHackageIndex UsePreferredVersions depname
          pure $ do
            lappVer <- latestApplicableVersion range $ Map.keysSet vsAndRevs
            revs <- Map.lookup lappVer vsAndRevs
            (cabalHash, _) <- Map.maxView revs
            Just (lappVer, cabalHash)
    case eres of
      Left e -> do
        addParent depname range Nothing
        let bd = case e of
              UnknownPackage name -> assert (name == depname) NotInBuildPlan
              DependencyCycleDetected names -> BDDependencyCycleDetected names
              -- ultimately we won't show any
              -- information on this to the user, we'll
              -- allow the dependency failures alone to
              -- display to avoid spamming the user too
              -- much
              DependencyPlanFailures _ _  ->
                Couldn'tResolveItsDependencies (packageVersion package)
        mlatestApplicable <- getLatestApplicableVersionAndRev
        pure $ Left (depname, (range, mlatestApplicable, bd))
      Right adr | depType == AsLibrary && not (adrHasLibrary adr) ->
        pure $ Left (depname, (range, Nothing, HasNoLibrary))
      Right adr -> do
        addParent depname range Nothing
        inRange <- if adrVersion adr `withinRange` range
          then pure True
          else do
            let warn_ reason = tell mempty { wWarnings = (msg:) }
                 where
                  msg = T.concat
                    [ "WARNING: Ignoring "
                    , T.pack $ packageNameString $ packageName package
                    , "'s bounds on "
                    , T.pack $ packageNameString depname
                    , " ("
                    , versionRangeText range
                    , "); using "
                    , T.pack $ packageIdentifierString $
                        PackageIdentifier depname (adrVersion adr)
                    , ".\nReason: "
                    , reason
                    , "."
                    ]
            allowNewer <- view $ configL.to configAllowNewer
            allowNewerDeps <- view $ configL.to configAllowNewerDeps
            let inSnapshotCheck = do
                  -- We ignore dependency information for packages in a snapshot
                  x <- inSnapshot (packageName package) (packageVersion package)
                  y <- inSnapshot depname (adrVersion adr)
                  if x && y
                    then do
                      warn_ "trusting snapshot over Cabal file dependency information"
                      pure True
                    else pure False
            if allowNewer
              then do
                warn_ "allow-newer enabled"
                case allowNewerDeps of
                  Nothing -> pure True
                  Just boundsIgnoredDeps ->
                      pure $ packageName package `elem` boundsIgnoredDeps
              else do
                when (isJust allowNewerDeps) $
                    warn_ "allow-newer-deps are specified but allow-newer isn't enabled"
                inSnapshotCheck
        if inRange
          then case adr of
            ADRToInstall task -> pure $ Right
              ( Set.singleton $ taskProvides task
              , Map.empty, taskTargetIsMutable task
              )
            ADRFound loc (Executable _) -> pure $ Right
              ( Set.empty, Map.empty
              , installLocationIsMutable loc
              )
            ADRFound loc (Library ident gid _) -> pure $ Right
              ( Set.empty, Map.singleton ident gid
              , installLocationIsMutable loc
              )
          else do
            mlatestApplicable <- getLatestApplicableVersionAndRev
            pure $ Left
              ( depname
              , ( range
                , mlatestApplicable
                , DependencyMismatch $ adrVersion adr
                )
              )
  case partitionEithers deps of
    -- Note that the Monoid for 'InstallLocation' means that if any
    -- is 'Local', the result is 'Local', indicating that the parent
    -- package must be installed locally. Otherwise the result is
    -- 'Snap', indicating that the parent can either be installed
    -- locally or in the snapshot.
    ([], pairs) -> pure $ Right $ mconcat pairs
    (errs, _) -> pure $ Left $ DependencyPlanFailures
      package
      (Map.fromList errs)
 where
  adrVersion (ADRToInstall task) = pkgVersion $ taskProvides task
  adrVersion (ADRFound _ installed) = installedVersion installed
  -- Update the parents map, for later use in plan construction errors
  -- - see 'getShortestDepsPath'.
  addParent depname range mversion =
    tell mempty { wParents = MonoidMap $ M.singleton depname val }
   where
    val = (First mversion, [(packageIdentifier package, range)])

  adrHasLibrary :: AddDepRes -> Bool
  adrHasLibrary (ADRToInstall task) = taskHasLibrary task
  adrHasLibrary (ADRFound _ Library{}) = True
  adrHasLibrary (ADRFound _ Executable{}) = False

  taskHasLibrary :: Task -> Bool
  taskHasLibrary task =
    case taskType task of
      TTLocalMutable lp -> packageHasLibrary $ lpPackage lp
      TTRemotePackage _ p _ -> packageHasLibrary p

  -- make sure we consider internal libraries as libraries too
  packageHasLibrary :: Package -> Bool
  packageHasLibrary p =
    not (Set.null (packageInternalLibraries p)) ||
    case packageLibraries p of
      HasLibraries _ -> True
      NoLibraries -> False

checkDirtiness :: PackageSource
               -> Installed
               -> Package
               -> Map PackageIdentifier GhcPkgId
               -> Bool
               -> M Bool
checkDirtiness ps installed package present buildHaddocks = do
  ctx <- ask
  moldOpts <- runRIO ctx $ tryGetFlagCache installed
  let configOpts = configureOpts
        (view envConfigL ctx)
        (baseConfigOpts ctx)
        present
        (psLocal ps)
        (installLocationIsMutable $ psLocation ps) -- should be Local i.e. mutable always
        package
      wantConfigCache = ConfigCache
        { configCacheOpts = configOpts
        , configCacheDeps = Set.fromList $ Map.elems present
        , configCacheComponents =
            case ps of
                PSFilePath lp -> Set.map (encodeUtf8 . renderComponent) $ lpComponents lp
                PSRemote{} -> Set.empty
        , configCacheHaddock = buildHaddocks
        , configCachePkgSrc = toCachePkgSrc ps
        , configCachePathEnvVar = pathEnvVar ctx
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
          tell mempty { wDirty = Map.singleton (packageName package) reason }
          pure True

describeConfigDiff :: Config -> ConfigCache -> ConfigCache -> Maybe Text
describeConfigDiff config old new
  | configCachePkgSrc old /= configCachePkgSrc new = Just $
      "switching from " <>
      pkgSrcName (configCachePkgSrc old) <> " to " <>
      pkgSrcName (configCachePkgSrc new)
  | not (configCacheDeps new `Set.isSubsetOf` configCacheDeps old) =
      Just "dependencies changed"
  | not $ Set.null newComponents =
      Just $ "components added: " `T.append` T.intercalate ", "
          (map (decodeUtf8With lenientDecode) (Set.toList newComponents))
  | not (configCacheHaddock old) && configCacheHaddock new =
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
           . (if configRebuildGhcOptions config
                 then id
                 else stripGhcOptions)
           . map T.pack
           . (\(ConfigureOpts x y) -> x ++ y)
           . configCacheOpts

  (oldOpts, newOpts) = removeMatching (userOpts old) (userOpts new)

  removeMatching (x:xs) (y:ys)
    | x == y = removeMatching xs ys
  removeMatching xs ys = (xs, ys)

  newComponents =
    configCacheComponents new `Set.difference` configCacheComponents old

  pkgSrcName (CacheSrcLocal fp) = T.pack fp
  pkgSrcName CacheSrcUpstream = "upstream source"

psForceDirty :: PackageSource -> Bool
psForceDirty (PSFilePath lp) = lpForceDirty lp
psForceDirty PSRemote{} = False

psDirty ::
     (MonadIO m, HasEnvConfig env, MonadReader env m)
  => PackageSource
  -> m (Maybe (Set FilePath))
psDirty (PSFilePath lp) = runMemoizedWith $ lpDirtyFiles lp
psDirty PSRemote {} = pure Nothing -- files never change in a remote package

psLocal :: PackageSource -> Bool
psLocal (PSFilePath _ ) = True
psLocal PSRemote{} = False

psLocation :: PackageSource -> InstallLocation
psLocation (PSFilePath _) = Local
psLocation PSRemote{} = Snap

-- | Get all of the dependencies for a given package, including build
-- tool dependencies.
checkAndWarnForUnknownTools :: Package -> M ()
checkAndWarnForUnknownTools p = do
  -- Check whether the tool is on the PATH before warning about it.
  warnings <- fmap catMaybes $ forM (Set.toList $ packageUnknownTools p) $
    \name@(ExeName toolName) -> do
      let settings = minimalEnvSettings { esIncludeLocals = True }
      config <- view configL
      menv <- liftIO $ configProcessContextSettings config settings
      mfound <- runRIO menv $ findExecutable $ T.unpack toolName
      case mfound of
        Left _ -> pure $ Just $ ToolWarning name (packageName p)
        Right _ -> pure Nothing
  tell mempty { wWarnings = (map toolWarningText warnings ++) }
  pure ()

-- | Warn about tools in the snapshot definition. States the tool name
-- expected and the package name using it.
data ToolWarning
  = ToolWarning ExeName PackageName
  deriving Show

toolWarningText :: ToolWarning -> Text
toolWarningText (ToolWarning (ExeName toolName) pkgName') =
     "No packages found in snapshot which provide a "
  <> T.pack (show toolName)
  <> " executable, which is a build-tool dependency of "
  <> T.pack (packageNameString pkgName')

-- | Strip out anything from the @Plan@ intended for the local database
stripLocals :: Plan -> Plan
stripLocals plan = plan
  { planTasks = Map.filter checkTask $ planTasks plan
  , planFinals = Map.empty
  , planUnregisterLocal = Map.empty
  , planInstallExes = Map.filter (/= Local) $ planInstallExes plan
  }
 where
  checkTask task = taskLocation task == Snap

stripNonDeps :: Set PackageName -> Plan -> Plan
stripNonDeps deps plan = plan
  { planTasks = Map.filter checkTask $ planTasks plan
  , planFinals = Map.empty
  , planInstallExes = Map.empty -- TODO maybe don't disable this?
  }
 where
  checkTask task = taskProvides task `Set.member` missingForDeps
  providesDep task = pkgName (taskProvides task) `Set.member` deps
  missing = Map.fromList $ map (taskProvides &&& tcoMissing . taskConfigOpts) $
            Map.elems (planTasks plan)
  missingForDeps = flip execState mempty $ do
    for_ (Map.elems $ planTasks plan) $ \task ->
      when (providesDep task) $
        collectMissing mempty (taskProvides task)

  collectMissing dependents pid = do
    when (pid `elem` dependents) $
      impureThrow $ TaskCycleBug pid
    modify' (<> Set.singleton pid)
    mapM_ (collectMissing (pid:dependents)) (fromMaybe mempty $ M.lookup pid missing)

-- | Is the given package/version combo defined in the snapshot or in the global
-- database?
inSnapshot :: PackageName -> Version -> M Bool
inSnapshot name version = do
  ctx <- ask
  pure $ fromMaybe False $ do
    ps <- Map.lookup name (combinedMap ctx)
    case ps of
      PIOnlySource (PSRemote _ srcVersion FromSnapshot _) ->
        pure $ srcVersion == version
      PIBoth (PSRemote _ srcVersion FromSnapshot _) _ ->
        pure $ srcVersion == version
      -- OnlyInstalled occurs for global database
      PIOnlyInstalled loc (Library pid _gid _lic) ->
        assert (loc == Snap) $
        assert (pkgVersion pid == version) $
        Just True
      _ -> pure False

-- TODO: Consider intersecting version ranges for multiple deps on a
-- package.  This is why VersionRange is in the parent map.

-- Switch this to 'True' to enable some debugging putStrLn in this module
planDebug :: MonadIO m => String -> m ()
planDebug = if False then liftIO . putStrLn else \_ -> pure ()
