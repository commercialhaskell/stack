{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Construct a @Plan@ for how to build
module Stack.Build.ConstructPlan
  ( constructPlan
  ) where

import           Control.Monad.Trans.Maybe ( MaybeT (..) )
import qualified Data.List as L
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import           Data.Monoid.Map ( MonoidMap(..) )
import qualified Data.Set as Set
import qualified Data.Text as T
import           Distribution.Types.BuildType ( BuildType (Configure) )
import           Distribution.Types.PackageName ( mkPackageName )
import           Generics.Deriving.Monoid ( memptydefault, mappenddefault )
import           Path ( parent )
import qualified RIO.NonEmpty as NE
import           RIO.Process ( HasProcessContext (..), findExecutable )
import           RIO.State
                   ( State, StateT (..), execState, get, modify, modify', put )
import           RIO.Writer ( WriterT (..), pass, tell )
import           Stack.Build.Cache ( tryGetFlagCache )
import           Stack.Build.Haddock ( shouldHaddockDeps )
import           Stack.Build.Source ( loadLocalPackage )
import           Stack.Constants ( compilerOptionsCabalFlag )
import           Stack.Package ( applyForceCustomBuild )
import           Stack.Prelude hiding ( loadPackage )
import           Stack.SourceMap ( getPLIVersion, mkProjectPackage )
import           Stack.Types.Build
                   ( CachePkgSrc (..), ConfigCache (..), Plan (..), Task (..)
                   , TaskConfigOpts (..), TaskType (..)
                   , installLocationIsMutable, taskIsTarget, taskLocation
                   , taskProvides, taskTargetIsMutable, toCachePkgSrc
                   )
import           Stack.Types.Build.Exception
                   ( BadDependency (..), BuildException (..)
                   , BuildPrettyException (..), ConstructPlanException (..)
                   )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..), stackYamlL )
import           Stack.Types.BuildOpts
                   ( BuildOpts (..), BuildOptsCLI (..), BuildSubset (..) )
import           Stack.Types.Compiler ( WhichCompiler (..) )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), HasCompiler (..) )
import           Stack.Types.Config ( Config (..), HasConfig (..), stackRootL )
import           Stack.Types.ConfigureOpts
                   ( BaseConfigOpts (..), ConfigureOpts (..), configureOpts )
import           Stack.Types.Curator ( Curator (..) )
import           Stack.Types.Dependency
                   ( DepValue (DepValue), DepType (AsLibrary) )
import           Stack.Types.DumpPackage ( DumpPackage (..) )
import           Stack.Types.EnvConfig
                   ( EnvConfig (..), HasEnvConfig (..), HasSourceMap (..) )
import           Stack.Types.EnvSettings
                   ( EnvSettings (..), minimalEnvSettings )
import           Stack.Types.GHCVariant ( HasGHCVariant (..) )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.GlobalOpts ( GlobalOpts (..) )
import           Stack.Types.IsMutable ( IsMutable (..) )
import           Stack.Types.NamedComponent ( exeComponents, renderComponent )
import           Stack.Types.Package
                   ( ExeName (..), InstallLocation (..), Installed (..)
                   , InstalledMap, LocalPackage (..), Package (..)
                   , PackageLibraries (..), PackageSource (..), installedVersion
                   , packageIdentifier, psVersion, runMemoizedWith
                   )
import           Stack.Types.ParentMap ( ParentMap )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.ProjectConfig ( isPCGlobalProject )
import           Stack.Types.Runner ( HasRunner (..), globalOptsL )
import           Stack.Types.SourceMap
                   ( CommonPackage (..), DepPackage (..), FromSnapshot (..)
                   , GlobalPackage (..), SMTargets (..), SourceMap (..)
                   )
import           Stack.Types.Version
                   ( latestApplicableVersion, versionRangeText, withinRange )
import           System.Environment ( lookupEnv )

-- | Type representing information about packages, namely information about
-- whether or not a package is already installed and, unless the package is not
-- to be built (global packages), where its source code is located.
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

-- | A type synonym representing dictionaries of package names, and combined
-- information about the package in respect of whether or not it is already
-- installed and, unless the package is not to be built (global packages), where
-- its source code is located.
type CombinedMap = Map PackageName PackageInfo

-- | A function to yield a 'CombinedMap' value from: (1) a dictionary of package
-- names, and where the source code of the named package is located; and (2) an
-- 'InstalledMap' value.
combineMap :: Map PackageName PackageSource -> InstalledMap -> CombinedMap
combineMap = Map.merge
  (Map.mapMissing (\_ s -> PIOnlySource s))
  (Map.mapMissing (\_ i -> uncurry PIOnlyInstalled i))
  (Map.zipWithMatched (\_ s i -> combineSourceInstalled s i))

-- | Type synonym representing values used during the construction of a build
-- plan. The type is an instance of 'Monad', hence its name.
type M =
  WriterT
    W
    -- ^ The output to be collected
    ( StateT
        (Map PackageName (Either ConstructPlanException AddDepRes))
        -- ^ Library map
        (RIO Ctx)
    )

-- | Type representing values used as the output to be collected during the
-- construction of a build plan.
data W = W
  { wFinals :: !(Map PackageName (Either ConstructPlanException Task))
    -- ^ A dictionary of package names, and either a final task to perform when
    -- building the package or an exception.
  , wInstall :: !(Map Text InstallLocation)
    -- ^ A dictionary of executables to be installed, and location where the
    -- executable's binary is placed.
  , wDirty :: !(Map PackageName Text)
    -- ^ A dictionary of local packages, and the reason why the local package is
    -- considered dirty.
  , wWarnings :: !([StyleDoc] -> [StyleDoc])
    -- ^ Warnings.
  , wParents :: !ParentMap
    -- ^ A dictionary of package names, and a list of pairs of the identifier
    -- of a package depending on the package and the version range specified for
    -- the dependency by that package. Used in the reporting of failure to
    -- construct a build plan.
  }
  deriving Generic

instance Semigroup W where
  (<>) = mappenddefault

instance Monoid W where
  mempty = memptydefault
  mappend = (<>)

-- | Type representing results of 'addDep'.
data AddDepRes
  = ADRToInstall Task
    -- ^ A task must be performed to provide the package name.
  | ADRFound InstallLocation Installed
    -- ^ An existing installation provides the package name.
  deriving Show

toTask :: AddDepRes -> Maybe Task
toTask (ADRToInstall task) = Just task
toTask (ADRFound _ _) = Nothing

-- | Type representing values used as the environment to be read from during the
-- construction of a build plan (the \'context\').
data Ctx = Ctx
  { baseConfigOpts :: !BaseConfigOpts
    -- ^ Basic information used to determine configure options
  , loadPackage    :: !(  PackageLocationImmutable
                       -> Map FlagName Bool
                       -> [Text]
                          -- ^ GHC options.
                       -> [Text]
                          -- ^ Cabal configure options.
                       -> M Package
                       )
  , combinedMap    :: !CombinedMap
    -- ^ A dictionary of package names, and combined information about the
    -- package in respect of whether or not it is already installed and, unless
    -- the package is not to be built (global packages), where its source code
    -- is located.
  , ctxEnvConfig   :: !EnvConfig
    -- ^ Configuration after the environment has been setup.
  , callStack      :: ![PackageName]
  , wanted         :: !(Set PackageName)
  , localNames     :: !(Set PackageName)
  , mcurator       :: !(Maybe Curator)
  , pathEnvVar     :: !Text
  }

instance HasPlatform Ctx where
  platformL = configL.platformL
  {-# INLINE platformL #-}
  platformVariantL = configL.platformVariantL
  {-# INLINE platformVariantL #-}

instance HasGHCVariant Ctx where
  ghcVariantL = configL.ghcVariantL
  {-# INLINE ghcVariantL #-}

instance HasLogFunc Ctx where
  logFuncL = configL.logFuncL

instance HasRunner Ctx where
  runnerL = configL.runnerL

instance HasStylesUpdate Ctx where
  stylesUpdateL = runnerL.stylesUpdateL

instance HasTerm Ctx where
  useColorL = runnerL.useColorL
  termWidthL = runnerL.termWidthL

instance HasConfig Ctx where
  configL = buildConfigL.lens bcConfig (\x y -> x { bcConfig = y })
  {-# INLINE configL #-}

instance HasPantryConfig Ctx where
  pantryConfigL = configL.pantryConfigL

instance HasProcessContext Ctx where
  processContextL = configL.processContextL

instance HasBuildConfig Ctx where
  buildConfigL = envConfigL.lens
    envConfigBuildConfig
    (\x y -> x { envConfigBuildConfig = y })

instance HasSourceMap Ctx where
  sourceMapL = envConfigL.sourceMapL

instance HasCompiler Ctx where
  compilerPathsL = envConfigL.compilerPathsL

instance HasEnvConfig Ctx where
  envConfigL = lens ctxEnvConfig (\x y -> x { ctxEnvConfig = y })

-- | Computes a build plan. This means figuring out which build 'Task's to take,
-- and the interdependencies among the build 'Task's. In particular:
--
-- 1) It determines which packages need to be built, based on the transitive
-- deps of the current targets. For local packages, this is indicated by the
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
    globalCabalVersion <- view $ compilerPathsL.to cpCabalVersion
    sources <- getSources globalCabalVersion
    mcur <- view $ buildConfigL.to bcCurator
    pathEnvVar' <- liftIO $ maybe mempty T.pack <$> lookupEnv "PATH"
    let ctx = mkCtx econfig globalCabalVersion sources mcur pathEnvVar'
        targetPackageNames = Map.keys $ smtTargets $ smTargets sourceMap
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
          { planTasks = tasks
          , planFinals = Map.fromList finals
          , planUnregisterLocal =
              mkUnregisterLocal tasks dirtyReason localDumpPkgs initialBuildSteps
          , planInstallExes =
              if    boptsInstallExes (bcoBuildOpts baseConfigOpts0)
                 || boptsInstallCompilerTool (bcoBuildOpts baseConfigOpts0)
                then installExes
                else Map.empty
          }
      else do
        stackYaml <- view stackYamlL
        stackRoot <- view stackRootL
        isImplicitGlobal <-
          view $ configL.to (isPCGlobalProject . configProject)
        prettyThrowM $ ConstructPlanFailed
          errs
          stackYaml
          stackRoot
          isImplicitGlobal
          parents
          (wanted ctx)
          prunedGlobalDeps
 where
  sourceProject = smProject sourceMap
  sourceDeps = smDeps sourceMap

  hasBaseInDeps = Map.member (mkPackageName "base") sourceDeps

  mkCtx econfig globalCabalVersion sources mcur pathEnvVar' = Ctx
    { baseConfigOpts = baseConfigOpts0
    , loadPackage = \w x y z -> runRIO econfig $
        applyForceCustomBuild globalCabalVersion <$> loadPackage0 w x y z
    , combinedMap = combineMap sources installedMap
    , ctxEnvConfig = econfig
    , callStack = []
    , wanted = Map.keysSet (smtTargets $ smTargets sourceMap)
    , localNames = Map.keysSet sourceProject
    , mcurator = mcur
    , pathEnvVar = pathEnvVar'
    }

  toEither :: (k, Either e v) -> Either e (k, v)
  toEither (_, Left e)  = Left e
  toEither (k, Right v) = Right (k, v)

  toMaybe :: (k, Maybe v) -> Maybe (k, v)
  toMaybe (_, Nothing) = Nothing
  toMaybe (k, Just v) = Just (k, v)

  takeSubset :: Plan -> RIO env Plan
  takeSubset = case boptsCLIBuildSubset $ bcoBuildOptsCLI baseConfigOpts0 of
    BSAll -> pure
    BSOnlySnapshot -> stripLocals
    BSOnlyDependencies -> stripNonDeps
    BSOnlyLocals -> errorOnSnapshot

  -- | Strip out anything from the 'Plan' intended for the local database.
  stripLocals :: Plan -> RIO env Plan
  stripLocals plan = pure plan
    { planTasks = Map.filter checkTask $ planTasks plan
    , planFinals = Map.empty
    , planUnregisterLocal = Map.empty
    , planInstallExes = Map.filter (/= Local) $ planInstallExes plan
    }
   where
    checkTask task = taskLocation task == Snap

  stripNonDeps :: Plan -> RIO env Plan
  stripNonDeps plan = pure plan
    { planTasks = Map.filter checkTask $ planTasks plan
    , planFinals = Map.empty
    , planInstallExes = Map.empty -- TODO maybe don't disable this?
    }
   where
    deps = Map.keysSet sourceDeps
    checkTask task = taskProvides task `Set.member` missingForDeps
    providesDep task = pkgName (taskProvides task) `Set.member` deps
    tasks = Map.elems $ planTasks plan
    missing =
      Map.fromList $ map (taskProvides &&& tcoMissing . taskConfigOpts) tasks
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
      throwIO $ NotOnlyLocal snapTasks snapExes
    pure plan

  prunedGlobalDeps :: Map PackageName [PackageName]
  prunedGlobalDeps = flip Map.mapMaybe (smGlobal sourceMap) $
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
                applyForceCustomBuild globalCabalVersion $ lpPackage lp
          pure lp { lpPackage = lpPackage' }
    pPackages <- for sourceProject $ \pp -> do
      lp <- loadLocalPackage' pp
      pure $ PSFilePath lp
    bopts <- view $ configL.to configBuild
    deps <- for sourceDeps $ \dp ->
      case dpLocation dp of
        PLImmutable loc ->
          pure $
            PSRemote loc (getPLIVersion loc) (dpFromSnapshot dp) (dpCommon dp)
        PLMutable dir -> do
          pp <- mkProjectPackage YesPrintWarnings dir (shouldHaddockDeps bopts)
          lp <- loadLocalPackage' pp
          pure $ PSFilePath lp
    pure $ pPackages <> deps

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
-- already registered local packages.
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
    | usAnyAdded us = loop (usToUnregister us) (usKeep us)
    -- Nothing added, so we've already caught them all. Return the Map we've
    -- already calculated.
    | otherwise = usToUnregister us
   where
    -- Run the unregister checking function on all packages we currently think
    -- we'll be keeping.
    us = execState (mapM_ go keep) initialUnregisterState
    initialUnregisterState = UnregisterState
      { usToUnregister = toUnregister
      , usKeep = []
      , usAnyAdded = False
      }

  go :: DumpPackage -> State UnregisterState ()
  go dp = do
    us <- get
    case maybeUnregisterReason (usToUnregister us) ident mParentLibId deps of
      -- Not unregistering, add it to the keep list.
      Nothing -> put us { usKeep = dp : usKeep us }
      -- Unregistering, add it to the unregister Map; and indicate that a
      -- package was in fact added to the unregister Map, so we loop again.
      Just reason -> put us
        { usToUnregister = Map.insert gid (ident, reason) (usToUnregister us)
        , usAnyAdded = True
        }
   where
    gid = dpGhcPkgId dp
    ident = dpPackageIdent dp
    mParentLibId = dpParentLibIdent dp
    deps = dpDepends dp

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
addFinal lp package isAllInOne buildHaddocks = do
  depsRes <- addPackageDeps package
  res <- case depsRes of
    Left e -> pure $ Left e
    Right (missing, present, _minLoc) -> do
      ctx <- ask
      pure $ Right Task
        { taskConfigOpts = TaskConfigOpts missing $ \missing' ->
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
        , taskBuildTypeConfig = packageBuildTypeConfig package
        }
  tell mempty { wFinals = Map.singleton (packageName package) res }

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
        <> fromString (packageNameString name)
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
  res <- if name `elem` callStack ctx
    then do
      logDebugPlanS "checkCallStackAndAddDep" $
           "Detected cycle "
        <> fromString (packageNameString name)
        <> ": "
        <> fromString (show $ map packageNameString (callStack ctx))
      pure $ Left $ DependencyCycleDetected $ name : callStack ctx
    else case Map.lookup name $ combinedMap ctx of
      -- TODO look up in the package index and see if there's a
      -- recommendation available
      Nothing -> do
        logDebugPlanS "checkCallStackAndAddDep" $
             "No package info for "
          <> fromString (packageNameString name)
          <> "."
        pure $ Left $ UnknownPackage name
      Just packageInfo ->
        -- Add the current package name to the head of the call stack.
        local (\ctx' -> ctx' { callStack = name : callStack ctx' }) $
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
    <> fromString (packageNameString name)
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
                cs <- asks (NE.nonEmpty . callStack)
                cs' <- maybe
                  (throwIO CallStackEmptyBug)
                  (pure . NE.tail)
                  cs
                prettyWarnL
                  $ flow "No latest package revision found for"
                  : style Current (fromString $ packageNameString name) <> ","
                  : flow "dependency callstack:"
                  : mkNarrativeList Nothing False
                      (map (fromString . packageNameString) cs' :: [StyleDoc])
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
  | lpWanted lp = tellExecutablesPackage Local $ lpPackage lp
  | otherwise = pure ()
-- Ignores ghcOptions because they don't matter for enumerating executables.
tellExecutables name (PSRemote pkgloc _version _fromSnapshot cp) =
  tellExecutablesUpstream name (pure $ Just pkgloc) Snap (cpFlags cp)

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
  when (name `Set.member` wanted ctx) $ do
    mPkgLoc <- retrievePkgLoc
    forM_ mPkgLoc $ \pkgLoc -> do
      p <- loadPackage ctx pkgLoc flags [] []
      tellExecutablesPackage loc p

-- | For given 'InstallLocation' and 'Package' values, adds relevant executables
-- to the collected output. In most cases, the relevant executables are all the
-- executables of the package. If the package is a wanted local one, the
-- executables are those executables that are wanted executables.
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
        <> fromString (packageNameString name)
        <> "."
      package <- loadPackage
        ctx pkgLoc (cpFlags cp) (cpGhcOptions cp) (cpCabalConfigOpts cp)
      resolveDepsAndInstall True (cpHaddocks cp) ps package minstalled
    PSFilePath lp -> do
      case lpTestBench lp of
        Nothing -> do
          logDebugPlanS "installPackage" $
               "No test or bench component for "
            <> fromString (packageNameString name)
            <> " so doing an all-in-one build."
          resolveDepsAndInstall
            True (lpBuildHaddocks lp) ps (lpPackage lp) minstalled
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
                <> fromString (packageNameString name)
                <> ", successfully added package deps."
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
              logDebugPlanS "installPackage" $
                   "Before trying cyclic plan, resetting lib result map to: "
                <> fromString (show libMap)
              put libMap
              -- Otherwise, fall back on building the tests / benchmarks in a
              -- separate step.
              res' <- resolveDepsAndInstall
                False (lpBuildHaddocks lp) ps (lpPackage lp) minstalled
              when (isRight res') $ do
                -- Insert it into the map so that it's available for addFinal.
                updateLibMap name res'
                addFinal lp tb False False
              pure res'
 where
  expectedTestOrBenchFailures maybeCurator = fromMaybe False $ do
    curator <- maybeCurator
    pure $ Set.member name (curatorExpectTestFailure curator) ||
           Set.member name (curatorExpectBenchmarkFailure curator)

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

-- | Checks if we need to install the given 'Package', given the results
-- of 'addPackageDeps'. If dependencies are missing, the package is dirty, or
-- it's not installed, then it needs to be installed.
installPackageGivenDeps ::
     Bool
     -- ^ will the build step also build any tests?
  -> Bool
     -- ^ Should Haddock documentation be built?
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
        { taskConfigOpts = TaskConfigOpts missing $ \missing' ->
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
        , taskBuildTypeConfig = packageBuildTypeConfig package
        }

-- | Is the build type of the package Configure
packageBuildTypeConfig :: Package -> Bool
packageBuildTypeConfig pkg = packageBuildType pkg == Configure

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
  let deps' = Map.toList $ packageDeps package
  deps <- forM deps' $ \(depname, DepValue range depType) -> do
    eres <- getCachedDepOrAddDep depname
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
        addParent depname range
        let bd = case e of
              UnknownPackage name -> assert (name == depname) NotInBuildPlan
              DependencyCycleDetected names -> BDDependencyCycleDetected names
              -- ultimately we won't show any information on this to the user,
              -- we'll allow the dependency failures alone to display to avoid
              -- spamming the user too much
              DependencyPlanFailures _ _  ->
                Couldn'tResolveItsDependencies (packageVersion package)
        mlatestApplicable <- getLatestApplicableVersionAndRev
        pure $ Left (depname, (range, mlatestApplicable, bd))
      Right adr | depType == AsLibrary && not (adrHasLibrary adr) ->
        pure $ Left (depname, (range, Nothing, HasNoLibrary))
      Right adr -> do
        addParent depname range
        inRange <- if adrVersion adr `withinRange` range
          then pure True
          else do
            let warn_ isIgnoring reason = tell mempty { wWarnings = (msg:) }
                 where
                  msg =
                       fillSep
                         [ if isIgnoring then "Ignoring" else flow "Not ignoring"
                         , style Current (fromString . packageNameString $ packageName package) <> "'s"
                         , flow "bounds on"
                         , style Current (fromString $ packageNameString depname)
                         , parens (fromString . T.unpack $ versionRangeText range)
                         , flow "and using"
                         , style Current (fromString . packageIdentifierString $
                             PackageIdentifier depname (adrVersion adr)) <> "."
                         ]
                    <> line
                    <> fillSep
                         [ "Reason:"
                         , reason <> "."
                         ]
            allowNewer <- view $ configL.to configAllowNewer
            allowNewerDeps <- view $ configL.to configAllowNewerDeps
            let inSnapshotCheck = do
                  -- We ignore dependency information for packages in a snapshot
                  x <- inSnapshot (packageName package) (packageVersion package)
                  y <- inSnapshot depname (adrVersion adr)
                  if x && y
                    then do
                      warn_ True
                        ( flow "trusting snapshot over Cabal file dependency \
                               \information"
                        )
                      pure True
                    else pure False
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
                  let pkgName = packageName package
                      pkgName' = fromString $ packageNameString pkgName
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
                inSnapshotCheck
        if inRange
          then case adr of
            ADRToInstall task -> pure $ Right
              ( Set.singleton $ taskProvides task
              , Map.empty
              , taskTargetIsMutable task
              )
            ADRFound loc (Executable _) -> pure $ Right
              ( Set.empty
              , Map.empty
              , installLocationIsMutable loc
              )
            ADRFound loc (Library ident gid _) -> pure $ Right
              ( Set.empty
              , Map.singleton ident gid
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
    -- Note that the Monoid for 'IsMutable' means that if any is 'Mutable',
    -- the result is 'Mutable'. Otherwise the result is 'Immutable'.
    ([], pairs) -> pure $ Right $ mconcat pairs
    (errs, _) ->
      pure $ Left $ DependencyPlanFailures package (Map.fromList errs)
 where
  adrVersion (ADRToInstall task) = pkgVersion $ taskProvides task
  adrVersion (ADRFound _ installed) = installedVersion installed
  -- Update the parents map, for later use in plan construction errors
  -- - see 'getShortestDepsPath'.
  addParent depname range = tell mempty { wParents = MonoidMap parentMap }
   where
    parentMap = Map.singleton depname [(packageIdentifier package, range)]

  adrHasLibrary :: AddDepRes -> Bool
  adrHasLibrary (ADRToInstall task) = taskHasLibrary task
  adrHasLibrary (ADRFound _ Library{}) = True
  adrHasLibrary (ADRFound _ Executable{}) = False

  taskHasLibrary :: Task -> Bool
  taskHasLibrary task =
    case taskType task of
      TTLocalMutable lp -> packageHasLibrary $ lpPackage lp
      TTRemotePackage _ p _ -> packageHasLibrary p

  -- make sure we consider sub-libraries as libraries too
  packageHasLibrary :: Package -> Bool
  packageHasLibrary p =
    not (Set.null (packageSubLibraries p)) ||
    case packageLibraries p of
      HasLibraries _ -> True
      NoLibraries -> False

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
              PSFilePath lp ->
                Set.map (encodeUtf8 . renderComponent) $ lpComponents lp
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

-- | For the given package, warn about any unknown tools that are not on the
-- PATH and not one of the executables of the package.
checkAndWarnForUnknownTools :: Package -> M ()
checkAndWarnForUnknownTools p = do
  let unknownTools = Set.toList $ packageUnknownTools p
  -- Check whether the tool is on the PATH or a package executable before
  -- warning about it.
  warnings <-
    fmap catMaybes $ forM unknownTools $ \name@(ExeName toolName) ->
      runMaybeT $ notOnPath toolName *> notPackageExe toolName *> warn name
  tell mempty { wWarnings = (map toolWarningText warnings ++) }
  pure ()
 where
  -- From Cabal 2.0, build-tools can specify a pre-built executable that should
  -- already be on the PATH.
  notOnPath toolName = MaybeT $ do
    let settings = minimalEnvSettings { esIncludeLocals = True }
    config <- view configL
    menv <- liftIO $ configProcessContextSettings config settings
    eFound <- runRIO menv $ findExecutable $ T.unpack toolName
    skipIf $ isRight eFound
  -- From Cabal 1.12, build-tools can specify another executable in the same
  -- package.
  notPackageExe toolName = MaybeT $ skipIf $ toolName `Set.member` packageExes p
  warn name = MaybeT . pure . Just $ ToolWarning name (packageName p)
  skipIf p' = pure $ if p' then Nothing else Just ()

-- | Warn about tools in the snapshot definition. States the tool name
-- expected and the package name using it.
data ToolWarning
  = ToolWarning ExeName PackageName
  deriving Show

toolWarningText :: ToolWarning -> StyleDoc
toolWarningText (ToolWarning (ExeName toolName) pkgName') = fillSep
  [ flow "No packages found in snapshot which provide a"
  , style PkgComponent (fromString $ show toolName)
  , flow "executable, which is a build-tool dependency of"
  , style Current (fromString $ packageNameString pkgName')
  ]

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

logDebugPlanS ::
     (HasCallStack, HasRunner env, MonadIO m, MonadReader env m)
  => LogSource
  -> Utf8Builder
  -> m ()
logDebugPlanS s msg = do
  debugPlan <- view $ globalOptsL.to globalPlanInLog
  when debugPlan $ logDebugS s msg
