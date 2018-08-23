{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Construct a @Plan@ for how to build
module Stack.Build.ConstructPlan
    ( constructPlan
    ) where

import           Stack.Prelude hiding (Display (..), loadPackage)
import           Control.Monad.RWS.Strict hiding ((<>))
import           Control.Monad.State.Strict (execState)
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import           Data.Monoid.Map (MonoidMap(..))
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Distribution.Text as Cabal
import qualified Distribution.Version as Cabal
import           Distribution.Types.BuildType (BuildType (Configure))
import           Distribution.Types.PackageName (mkPackageName)
import           Distribution.Version (mkVersion)
import           Generics.Deriving.Monoid (memptydefault, mappenddefault)
import           Path (parent)
import qualified RIO
import           Stack.Build.Cache
import           Stack.Build.Haddock
import           Stack.Build.Installed
import           Stack.Build.Source
import           Stack.Constants
import           Stack.Package
import           Stack.PackageDump
import           Stack.PrettyPrint
import           Stack.Types.Build
import           Stack.Types.BuildPlan
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.GhcPkgId
import           Stack.Types.NamedComponent
import           Stack.Types.Package
import           Stack.Types.Runner
import           Stack.Types.Version
import           System.IO (putStrLn)
import           RIO.Process (findExecutable, HasProcessContext (..))

data PackageInfo
    =
      -- | This indicates that the package is already installed, and
      -- that we shouldn't build it from source. This is always the case
      -- for snapshot packages.
      PIOnlyInstalled InstallLocation Installed
      -- | This indicates that the package isn't installed, and we know
      -- where to find its source (either a hackage package or a local
      -- directory).
    | PIOnlySource PackageSource
      -- | This indicates that the package is installed and we know
      -- where to find its source. We may want to reinstall from source.
    | PIBoth PackageSource Installed
    deriving (Show)

combineSourceInstalled :: PackageSource
                       -> (InstallLocation, Installed)
                       -> PackageInfo
combineSourceInstalled ps (location, installed) =
    assert (piiVersion ps == installedVersion installed) $
    assert (piiLocation ps == location) $
    case location of
        -- Always trust something in the snapshot
        Snap -> PIOnlyInstalled location installed
        Local -> PIBoth ps installed

type CombinedMap = Map PackageName PackageInfo

combineMap :: SourceMap -> InstalledMap -> CombinedMap
combineMap = Map.mergeWithKey
    (\_ s i -> Just $ combineSourceInstalled s i)
    (fmap PIOnlySource)
    (fmap (uncurry PIOnlyInstalled))

data AddDepRes
    = ADRToInstall Task
    | ADRFound InstallLocation Installed
    deriving Show

type ParentMap = MonoidMap PackageName (First Version, [(PackageIdentifier, VersionRange)])

data W = W
    { wFinals :: !(Map PackageName (Either ConstructPlanException Task))
    , wInstall :: !(Map Text InstallLocation)
    -- ^ executable to be installed, and location where the binary is placed
    , wDirty :: !(Map PackageName Text)
    -- ^ why a local package is considered dirty
    , wDeps :: !(Set PackageName)
    -- ^ Packages which count as dependencies
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
    { ls             :: !LoadedSnapshot
    , baseConfigOpts :: !BaseConfigOpts
    , loadPackage    :: !(PackageLocationImmutable -> Map FlagName Bool -> [Text] -> M Package)
    , combinedMap    :: !CombinedMap
    , ctxEnvConfig   :: !EnvConfig
    , callStack      :: ![PackageName]
    , extraToBuild   :: !(Set PackageName)
    , wanted         :: !(Set PackageName)
    , localNames     :: !(Set PackageName)
    }

instance HasPlatform Ctx
instance HasGHCVariant Ctx
instance HasLogFunc Ctx where
    logFuncL = configL.logFuncL
instance HasRunner Ctx where
    runnerL = configL.runnerL
instance HasConfig Ctx
instance HasPantryConfig Ctx where
    pantryConfigL = configL.pantryConfigL
instance HasProcessContext Ctx where
    processContextL = configL.processContextL
instance HasBuildConfig Ctx
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
constructPlan :: forall env. HasEnvConfig env
              => LoadedSnapshot
              -> BaseConfigOpts
              -> [LocalPackage]
              -> Set PackageName -- ^ additional packages that must be built
              -> [DumpPackage () () ()] -- ^ locally registered
              -> (PackageLocationImmutable -> Map FlagName Bool -> [Text] -> RIO EnvConfig Package) -- ^ load upstream package
              -> SourceMap
              -> InstalledMap
              -> Bool
              -> RIO env Plan
constructPlan ls0 baseConfigOpts0 locals extraToBuild0 localDumpPkgs loadPackage0 sourceMap installedMap initialBuildSteps = do
    logDebug "Constructing the build plan"

    bconfig <- view buildConfigL
    when (hasBaseInDeps bconfig) $
      prettyWarn $ flow "You are trying to upgrade/downgrade base, which is almost certainly not what you really want. Please, consider using another GHC version if you need a certain version of base, or removing base from extra-deps. See more at https://github.com/commercialhaskell/stack/issues/3940." <> line

    econfig <- view envConfigL
    let onWanted = void . addDep False . packageName . lpPackage
    let inner = do
            mapM_ onWanted $ filter lpWanted locals
            mapM_ (addDep False) $ Set.toList extraToBuild0
    let ctx = mkCtx econfig
    ((), m, W efinals installExes dirtyReason deps warnings parents) <-
        liftIO $ runRWST inner ctx M.empty
    mapM_ (logWarn . RIO.display) (warnings [])
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
                        BSAll -> id
                        BSOnlySnapshot -> stripLocals
                        BSOnlyDependencies -> stripNonDeps deps
            return $ takeSubset Plan
                { planTasks = tasks
                , planFinals = M.fromList finals
                , planUnregisterLocal = mkUnregisterLocal tasks dirtyReason localDumpPkgs sourceMap initialBuildSteps
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
            prettyErrorNoIndent $ pprintExceptions errs stackYaml stackRoot parents (wanted ctx)
            throwM $ ConstructPlanFailed "Plan construction failed."
  where
    hasBaseInDeps bconfig =
        mkPackageName "base" `elem`
        [n | (PLImmutable (PLIHackage (PackageIdentifierRevision n _ _) _)) <- bcDependencies bconfig]

    mkCtx econfig = Ctx
        { ls = ls0
        , baseConfigOpts = baseConfigOpts0
        , loadPackage = \x y z -> runRIO econfig $ loadPackage0 x y z
        , combinedMap = combineMap sourceMap installedMap
        , ctxEnvConfig = econfig
        , callStack = []
        , extraToBuild = extraToBuild0
        , wanted = wantedLocalPackages locals <> extraToBuild0
        , localNames = Set.fromList $ map (packageName . lpPackage) locals
        }

-- | State to be maintained during the calculation of local packages
-- to unregister.
data UnregisterState = UnregisterState
    { usToUnregister :: !(Map GhcPkgId (PackageIdentifier, Text))
    , usKeep :: ![DumpPackage () () ()]
    , usAnyAdded :: !Bool
    }

-- | Determine which packages to unregister based on the given tasks and
-- already registered local packages
mkUnregisterLocal :: Map PackageName Task
                  -- ^ Tasks
                  -> Map PackageName Text
                  -- ^ Reasons why packages are dirty and must be rebuilt
                  -> [DumpPackage () () ()]
                  -- ^ Local package database dump
                  -> SourceMap
                  -> Bool
                  -- ^ If true, we're doing a special initialBuildSteps
                  -- build - don't unregister target packages.
                  -> Map GhcPkgId (PackageIdentifier, Text)
mkUnregisterLocal tasks dirtyReason localDumpPkgs sourceMap initialBuildSteps =
    -- We'll take multiple passes through the local packages. This
    -- will allow us to detect that a package should be unregistered,
    -- as well as all packages directly or transitively depending on
    -- it.
    loop Map.empty localDumpPkgs
  where
    loop toUnregister keep
        -- If any new packages were added to the unregister Map, we
        -- need to loop through the remaining packages again to detect
        -- if a transitive dependency is being unregistered.
        | usAnyAdded us = loop (usToUnregister us) (usKeep us)
        -- Nothing added, so we've already caught them all. Return the
        -- Map we've already calculated.
        | otherwise = usToUnregister us
      where
        -- Run the unregister checking function on all packages we
        -- currently think we'll be keeping.
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
            -- Unregistering, add it to the unregister Map and
            -- indicate that a package was in fact added to the
            -- unregister Map so we loop again.
            Just reason -> put us
                { usToUnregister = Map.insert gid (ident, reason) (usToUnregister us)
                , usAnyAdded = True
                }
      where
        gid = dpGhcPkgId dp
        ident = dpPackageIdent dp
        deps = dpDepends dp

    go' toUnregister ident deps
      -- If we're planning on running a task on it, then it must be
      -- unregistered, unless it's a target and an initial-build-steps
      -- build is being done.
      | Just task <- Map.lookup name tasks
          = if initialBuildSteps && taskIsTarget task && taskProvides task == ident
              then Nothing
              else Just $ fromMaybe "" $ Map.lookup name dirtyReason
      -- Check if we're no longer using the local version
      | Just (piiLocation -> Snap) <- Map.lookup name sourceMap
          = Just "Switching to snapshot installed package"
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
addFinal :: LocalPackage -> Package -> Bool -> M ()
addFinal lp package isAllInOne = do
    depsRes <- addPackageDeps False package
    res <- case depsRes of
        Left e -> return $ Left e
        Right (missing, present, _minLoc) -> do
            ctx <- ask
            return $ Right Task
                { taskProvides = PackageIdentifier
                    (packageName package)
                    (packageVersion package)
                , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                    let allDeps = Map.union present missing'
                     in configureOpts
                            (view envConfigL ctx)
                            (baseConfigOpts ctx)
                            allDeps
                            True -- local
                            Local
                            package
                , taskPresent = present
                , taskType = TTFilePath lp Local -- FIXME we can rely on this being Local, right?
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
addDep :: Bool -- ^ is this being used by a dependency?
       -> PackageName
       -> M (Either ConstructPlanException AddDepRes)
addDep treatAsDep' name = do
    ctx <- ask
    let treatAsDep = treatAsDep' || name `Set.notMember` wanted ctx
    when treatAsDep $ markAsDep name
    m <- get
    case Map.lookup name m of
        Just res -> do
            planDebug $ "addDep: Using cached result for " ++ show name ++ ": " ++ show res
            return res
        Nothing -> do
            res <- if name `elem` callStack ctx
                then do
                    planDebug $ "addDep: Detected cycle " ++ show name ++ ": " ++ show (callStack ctx)
                    return $ Left $ DependencyCycleDetected $ name : callStack ctx
                else local (\ctx' -> ctx' { callStack = name : callStack ctx' }) $ do
                    let mpackageInfo = Map.lookup name $ combinedMap ctx
                    planDebug $ "addDep: Package info for " ++ show name ++ ": " ++ show mpackageInfo
                    case mpackageInfo of
                        -- TODO look up in the package index and see if there's a
                        -- recommendation available
                        Nothing -> return $ Left $ UnknownPackage name
                        Just (PIOnlyInstalled loc installed) -> do
                            -- FIXME Slightly hacky, no flags since
                            -- they likely won't affect executable
                            -- names. This code does not feel right.
                            tellExecutablesUpstream
                              (PackageIdentifier name (installedVersion installed))
                              (PLIHackage (PackageIdentifierRevision name (installedVersion installed) CFILatest) Nothing)
                              loc
                              Map.empty
                            return $ Right $ ADRFound loc installed
                        Just (PIOnlySource ps) -> do
                            tellExecutables ps
                            installPackage treatAsDep name ps Nothing
                        Just (PIBoth ps installed) -> do
                            tellExecutables ps
                            installPackage treatAsDep name ps (Just installed)
            updateLibMap name res
            return res

-- FIXME what's the purpose of this? Add a Haddock!
tellExecutables :: PackageSource -> M ()
tellExecutables (PSFilePath lp _)
    | lpWanted lp = tellExecutablesPackage Local $ lpPackage lp
    | otherwise = return ()
-- Ignores ghcOptions because they don't matter for enumerating
-- executables.
tellExecutables (PSRemote loc flags _ghcOptions pkgloc ident) =
    tellExecutablesUpstream ident pkgloc loc flags

tellExecutablesUpstream :: PackageIdentifier -> PackageLocationImmutable -> InstallLocation -> Map FlagName Bool -> M ()
tellExecutablesUpstream (PackageIdentifier name _) pkgloc loc flags = do
    ctx <- ask
    when (name `Set.member` extraToBuild ctx) $ do
        p <- loadPackage ctx pkgloc flags []
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

        goSource (PSFilePath lp _)
            | lpWanted lp = exeComponents (lpComponents lp)
            | otherwise = Set.empty
        goSource PSRemote{} = Set.empty

    tell mempty { wInstall = Map.fromList $ map (, loc) $ Set.toList $ filterComps myComps $ packageExes p }
  where
    filterComps myComps x
        | Set.null myComps = x
        | otherwise = Set.intersection x myComps

-- | Given a 'PackageSource' and perhaps an 'Installed' value, adds
-- build 'Task's for the package and its dependencies.
installPackage :: Bool -- ^ is this being used by a dependency?
               -> PackageName
               -> PackageSource
               -> Maybe Installed
               -> M (Either ConstructPlanException AddDepRes)
installPackage treatAsDep name ps minstalled = do
    ctx <- ask
    case ps of
        PSRemote _ flags ghcOptions pkgLoc _version -> do
            planDebug $ "installPackage: Doing all-in-one build for upstream package " ++ show name
            package <- loadPackage ctx pkgLoc flags ghcOptions
            resolveDepsAndInstall True treatAsDep ps package minstalled
        PSFilePath lp _ ->
            case lpTestBench lp of
                Nothing -> do
                    planDebug $ "installPackage: No test / bench component for " ++ show name ++ " so doing an all-in-one build."
                    resolveDepsAndInstall True treatAsDep ps (lpPackage lp) minstalled
                Just tb -> do
                    -- Attempt to find a plan which performs an all-in-one
                    -- build.  Ignore the writer action + reset the state if
                    -- it fails.
                    s <- get
                    res <- pass $ do
                        res <- addPackageDeps treatAsDep tb
                        let writerFunc w = case res of
                                Left _ -> mempty
                                _ -> w
                        return (res, writerFunc)
                    case res of
                        Right deps -> do
                          planDebug $ "installPackage: For " ++ show name ++ ", successfully added package deps"
                          adr <- installPackageGivenDeps True ps tb minstalled deps
                          -- FIXME: this redundantly adds the deps (but
                          -- they'll all just get looked up in the map)
                          addFinal lp tb True
                          return $ Right adr
                        Left _ -> do
                            -- Reset the state to how it was before
                            -- attempting to find an all-in-one build
                            -- plan.
                            planDebug $ "installPackage: Before trying cyclic plan, resetting lib result map to " ++ show s
                            put s
                            -- Otherwise, fall back on building the
                            -- tests / benchmarks in a separate step.
                            res' <- resolveDepsAndInstall False treatAsDep ps (lpPackage lp) minstalled
                            when (isRight res') $ do
                                -- Insert it into the map so that it's
                                -- available for addFinal.
                                updateLibMap name res'
                                addFinal lp tb False
                            return res'

resolveDepsAndInstall :: Bool
                      -> Bool
                      -> PackageSource
                      -> Package
                      -> Maybe Installed
                      -> M (Either ConstructPlanException AddDepRes)
resolveDepsAndInstall isAllInOne treatAsDep ps package minstalled = do
    res <- addPackageDeps treatAsDep package
    case res of
        Left err -> return $ Left err
        Right deps -> liftM Right $ installPackageGivenDeps isAllInOne ps package minstalled deps

-- | Checks if we need to install the given 'Package', given the results
-- of 'addPackageDeps'. If dependencies are missing, the package is
-- dirty, or it's not installed, then it needs to be installed.
installPackageGivenDeps :: Bool
                        -> PackageSource
                        -> Package
                        -> Maybe Installed
                        -> ( Set PackageIdentifier
                           , Map PackageIdentifier GhcPkgId
                           , InstallLocation )
                        -> M AddDepRes
installPackageGivenDeps isAllInOne ps package minstalled (missing, present, minLoc) = do
    let name = packageName package
    ctx <- ask
    mRightVersionInstalled <- case (minstalled, Set.null missing) of
        (Just installed, True) -> do
            shouldInstall <- checkDirtiness ps installed package present (wanted ctx)
            return $ if shouldInstall then Nothing else Just installed
        (Just _, False) -> do
            let t = T.intercalate ", " $ map (T.pack . packageNameString . pkgName) (Set.toList missing)
            tell mempty { wDirty = Map.singleton name $ "missing dependencies: " <> addEllipsis t }
            return Nothing
        (Nothing, _) -> return Nothing
    return $ case mRightVersionInstalled of
        Just installed -> ADRFound (piiLocation ps) installed
        Nothing -> ADRToInstall Task
            { taskProvides = PackageIdentifier
                (packageName package)
                (packageVersion package)
            , taskConfigOpts = TaskConfigOpts missing $ \missing' ->
                let allDeps = Map.union present missing'
                    destLoc = piiLocation ps <> minLoc
                 in configureOpts
                        (view envConfigL ctx)
                        (baseConfigOpts ctx)
                        allDeps
                        (psLocal ps)
                        -- An assertion to check for a recurrence of
                        -- https://github.com/commercialhaskell/stack/issues/345
                        (assert (destLoc == piiLocation ps) destLoc)
                        package
            , taskPresent = present
            , taskType =
                case ps of
                    PSFilePath lp loc -> TTFilePath lp (loc <> minLoc)
                    PSRemote loc _ _ pkgLoc _version -> TTRemote package (loc <> minLoc) pkgLoc
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
addPackageDeps :: Bool -- ^ is this being used by a dependency?
               -> Package -> M (Either ConstructPlanException (Set PackageIdentifier, Map PackageIdentifier GhcPkgId, InstallLocation))
addPackageDeps treatAsDep package = do
    ctx <- ask
    deps' <- packageDepsWithTools package
    deps <- forM (Map.toList deps') $ \(depname, DepValue range depType) -> do
        eres <- addDep treatAsDep depname
        let getLatestApplicableVersionAndRev :: M (Maybe (Version, BlobKey))
            getLatestApplicableVersionAndRev = do
              vsAndRevs <- runRIO ctx $ getHackagePackageVersions YesPreferredVersions depname
              pure $ do
                lappVer <- latestApplicableVersion range $ Map.keysSet vsAndRevs
                revs <- Map.lookup lappVer vsAndRevs
                (cabalHash, _) <- Map.maxView revs
                Just (lappVer, cabalHash)
        case eres of
            Left e -> do
                addParent depname range Nothing
                let bd =
                        case e of
                            UnknownPackage name -> assert (name == depname) NotInBuildPlan
                            DependencyCycleDetected names -> BDDependencyCycleDetected names
                            -- ultimately we won't show any
                            -- information on this to the user, we'll
                            -- allow the dependency failures alone to
                            -- display to avoid spamming the user too
                            -- much
                            DependencyPlanFailures _ _  -> Couldn'tResolveItsDependencies (packageVersion package)
                mlatestApplicable <- getLatestApplicableVersionAndRev
                return $ Left (depname, (range, mlatestApplicable, bd))
            Right adr | depType == AsLibrary && not (adrHasLibrary adr) ->
                return $ Left (depname, (range, Nothing, HasNoLibrary))
            Right adr -> do
                addParent depname range Nothing
                inRange <- if adrVersion adr `withinRange` range
                    then return True
                    else do
                        let warn_ reason =
                                tell mempty { wWarnings = (msg:) }
                              where
                                msg = T.concat
                                    [ "WARNING: Ignoring out of range dependency"
                                    , reason
                                    , ": "
                                    , T.pack $ packageIdentifierString $ PackageIdentifier depname (adrVersion adr)
                                    , ". "
                                    , T.pack $ packageNameString $ packageName package
                                    , " requires: "
                                    , versionRangeText range
                                    ]
                        allowNewer <- view $ configL.to configAllowNewer
                        if allowNewer
                            then do
                                warn_ " (allow-newer enabled)"
                                return True
                            else do
                                x <- inSnapshot (packageName package) (packageVersion package)
                                y <- inSnapshot depname (adrVersion adr)
                                if x && y
                                    then do
                                        warn_ " (trusting snapshot over Hackage revisions)"
                                        return True
                                    else return False
                if inRange
                    then case adr of
                        ADRToInstall task -> return $ Right
                            (Set.singleton $ taskProvides task, Map.empty, taskLocation task)
                        ADRFound loc (Executable _) -> return $ Right
                            (Set.empty, Map.empty, loc)
                        ADRFound loc (Library ident gid _) -> return $ Right
                            (Set.empty, Map.singleton ident gid, loc)
                    else do
                        mlatestApplicable <- getLatestApplicableVersionAndRev
                        return $ Left (depname, (range, mlatestApplicable, DependencyMismatch $ adrVersion adr))
    case partitionEithers deps of
        -- Note that the Monoid for 'InstallLocation' means that if any
        -- is 'Local', the result is 'Local', indicating that the parent
        -- package must be installed locally. Otherwise the result is
        -- 'Snap', indicating that the parent can either be installed
        -- locally or in the snapshot.
        ([], pairs) -> return $ Right $ mconcat pairs
        (errs, _) -> return $ Left $ DependencyPlanFailures
            package
            (Map.fromList errs)
  where
    adrVersion (ADRToInstall task) = pkgVersion $ taskProvides task
    adrVersion (ADRFound _ installed) = installedVersion installed
    -- Update the parents map, for later use in plan construction errors
    -- - see 'getShortestDepsPath'.
    addParent depname range mversion = tell mempty { wParents = MonoidMap $ M.singleton depname val }
      where
        val = (First mversion, [(packageIdentifier package, range)])

    adrHasLibrary :: AddDepRes -> Bool
    adrHasLibrary (ADRToInstall task) = taskHasLibrary task
    adrHasLibrary (ADRFound _ Library{}) = True
    adrHasLibrary (ADRFound _ Executable{}) = False

    taskHasLibrary :: Task -> Bool
    taskHasLibrary task =
      case taskType task of
        TTFilePath lp _ -> packageHasLibrary $ lpPackage lp
        TTRemote p _ _ -> packageHasLibrary p

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
               -> Set PackageName
               -> M Bool
checkDirtiness ps installed package present wanted = do
    ctx <- ask
    moldOpts <- runRIO ctx $ tryGetFlagCache installed
    let configOpts = configureOpts
            (view envConfigL ctx)
            (baseConfigOpts ctx)
            present
            (psLocal ps)
            (piiLocation ps) -- should be Local always
            package
        buildOpts = bcoBuildOpts (baseConfigOpts ctx)
        wantConfigCache = ConfigCache
            { configCacheOpts = configOpts
            , configCacheDeps = Set.fromList $ Map.elems present
            , configCacheComponents =
                case ps of
                    PSFilePath lp _ -> Set.map (encodeUtf8 . renderComponent) $ lpComponents lp
                    PSRemote{} -> Set.empty
            , configCacheHaddock =
                shouldHaddockPackage buildOpts wanted (packageName package) ||
                -- Disabling haddocks when old config had haddocks doesn't make dirty.
                maybe False configCacheHaddock moldOpts
            , configCachePkgSrc = toCachePkgSrc ps
            }
        config = view configL ctx
    mreason <-
      case moldOpts of
        Nothing -> pure $ Just "old configure information not found"
        Just oldOpts
          | Just reason <- describeConfigDiff config oldOpts wantConfigCache -> pure $ Just reason
          | True <- psForceDirty ps -> pure $ Just "--force-dirty specified"
          | otherwise -> do
              dirty <- psDirty ps
              pure $
                case dirty of
                  Just files -> Just $ "local file changes: " <> addEllipsis (T.pack $ unwords $ Set.toList files)
                  Nothing -> Nothing
    case mreason of
        Nothing -> return False
        Just reason -> do
            tell mempty { wDirty = Map.singleton (packageName package) reason }
            return True

describeConfigDiff :: Config -> ConfigCache -> ConfigCache -> Maybe Text
describeConfigDiff config old new
    | configCachePkgSrc old /= configCachePkgSrc new = Just $
        "switching from " <>
        pkgSrcName (configCachePkgSrc old) <> " to " <>
        pkgSrcName (configCachePkgSrc new)
    | not (configCacheDeps new `Set.isSubsetOf` configCacheDeps old) = Just "dependencies changed"
    | not $ Set.null newComponents =
        Just $ "components added: " `T.append` T.intercalate ", "
            (map (decodeUtf8With lenientDecode) (Set.toList newComponents))
    | not (configCacheHaddock old) && configCacheHaddock new = Just "rebuilding with haddocks"
    | oldOpts /= newOpts = Just $ T.pack $ concat
        [ "flags changed from "
        , show oldOpts
        , " to "
        , show newOpts
        ]
    | otherwise = Nothing
  where
    stripGhcOptions =
        go
      where
        go [] = []
        go ("--ghc-option":x:xs) = go' Ghc x xs
        go ("--ghc-options":x:xs) = go' Ghc x xs
        go ((T.stripPrefix "--ghc-option=" -> Just x):xs) = go' Ghc x xs
        go ((T.stripPrefix "--ghc-options=" -> Just x):xs) = go' Ghc x xs
        go ("--ghcjs-option":x:xs) = go' Ghcjs x xs
        go ("--ghcjs-options":x:xs) = go' Ghcjs x xs
        go ((T.stripPrefix "--ghcjs-option=" -> Just x):xs) = go' Ghcjs x xs
        go ((T.stripPrefix "--ghcjs-options=" -> Just x):xs) = go' Ghcjs x xs
        go (x:xs) = x : go xs

        go' wc x xs = checkKeepers wc x $ go xs

        checkKeepers wc x xs =
            case filter isKeeper $ T.words x of
                [] -> xs
                keepers -> T.pack (compilerOptionsCabalFlag wc) : T.unwords keepers : xs

        -- GHC options which affect build results and therefore should always
        -- force a rebuild
        --
        -- For the most part, we only care about options generated by Stack
        -- itself
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

    newComponents = configCacheComponents new `Set.difference` configCacheComponents old

    pkgSrcName (CacheSrcLocal fp) = T.pack fp
    pkgSrcName CacheSrcUpstream = "upstream source"

psForceDirty :: PackageSource -> Bool
psForceDirty (PSFilePath lp _) = lpForceDirty lp
psForceDirty PSRemote{} = False

psDirty :: MonadIO m => PackageSource -> m (Maybe (Set FilePath))
psDirty (PSFilePath lp _) = runIOThunk $ lpDirtyFiles lp
psDirty PSRemote {} = pure Nothing -- files never change in a remote package

psLocal :: PackageSource -> Bool
psLocal (PSFilePath _ loc) = loc == Local -- FIXME this is probably not the right logic, see configureOptsNoDir. We probably want to check if this appears in packages:
psLocal PSRemote{} = False

-- | Get all of the dependencies for a given package, including build
-- tool dependencies.
packageDepsWithTools :: Package -> M (Map PackageName DepValue)
packageDepsWithTools p = do
    -- Check whether the tool is on the PATH before warning about it.
    warnings <- fmap catMaybes $ forM (Set.toList $ packageUnknownTools p) $
      \name@(ExeName toolName) -> do
        let settings = minimalEnvSettings { esIncludeLocals = True }
        config <- view configL
        menv <- liftIO $ configProcessContextSettings config settings
        mfound <- runRIO menv $ findExecutable $ T.unpack toolName
        case mfound of
            Left _ -> return $ Just $ ToolWarning name (packageName p)
            Right _ -> return Nothing
    tell mempty { wWarnings = (map toolWarningText warnings ++) }
    return $ packageDeps p

-- | Warn about tools in the snapshot definition. States the tool name
-- expected and the package name using it.
data ToolWarning = ToolWarning ExeName PackageName
  deriving Show

toolWarningText :: ToolWarning -> Text
toolWarningText (ToolWarning (ExeName toolName) pkgName) =
    "No packages found in snapshot which provide a " <>
    T.pack (show toolName) <>
    " executable, which is a build-tool dependency of " <>
    T.pack (packageNameString pkgName)

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
    checkTask task = pkgName (taskProvides task) `Set.member` deps

markAsDep :: PackageName -> M ()
markAsDep name = tell mempty { wDeps = Set.singleton name }

-- | Is the given package/version combo defined in the snapshot?
inSnapshot :: PackageName -> Version -> M Bool
inSnapshot name version = do
    p <- asks ls
    ls <- asks localNames
    return $ fromMaybe False $ do
        guard $ not $ name `Set.member` ls
        lpi <- Map.lookup name (lsPackages p)
        return $ lpiVersion lpi == version

data ConstructPlanException
    = DependencyCycleDetected [PackageName]
    | DependencyPlanFailures Package (Map PackageName (VersionRange, LatestApplicableVersion, BadDependency))
    | UnknownPackage PackageName -- TODO perhaps this constructor will be removed, and BadDependency will handle it all
    -- ^ Recommend adding to extra-deps, give a helpful version number?
    deriving (Typeable, Eq, Ord, Show)

deriving instance Ord VersionRange

-- | The latest applicable version and it's latest cabal file revision.
-- For display purposes only, Nothing if package not found
type LatestApplicableVersion = Maybe (Version, BlobKey)

-- | Reason why a dependency was not used
data BadDependency
    = NotInBuildPlan
    | Couldn'tResolveItsDependencies Version
    | DependencyMismatch Version
    | HasNoLibrary
    -- ^ See description of 'DepType'
    | BDDependencyCycleDetected ![PackageName]
    deriving (Typeable, Eq, Ord, Show)

-- TODO: Consider intersecting version ranges for multiple deps on a
-- package.  This is why VersionRange is in the parent map.

pprintExceptions
    :: [ConstructPlanException]
    -> Path Abs File
    -> Path Abs Dir
    -> ParentMap
    -> Set PackageName
    -> StyleDoc
pprintExceptions exceptions stackYaml stackRoot parentMap wanted =
    mconcat $
      [ flow "While constructing the build plan, the following exceptions were encountered:"
      , line <> line
      , mconcat (intersperse (line <> line) (mapMaybe pprintException exceptions'))
      , line <> line
      , flow "Some different approaches to resolving this:"
      , line <> line
      ] ++
      (if not onlyHasDependencyMismatches then [] else
         [ "  *" <+> align (flow "Set 'allow-newer: true' in " <+> toStyleDoc (display (defaultUserConfigPath stackRoot)) <+> "to ignore all version constraints and build anyway.")
         , line <> line
         ]
      ) ++
      [ "  *" <+> align (flow "Consider trying 'stack solver', which uses the cabal-install solver to attempt to find some working build configuration. This can be convenient when dealing with many complicated constraint errors, but results may be unpredictable.")
      , line <> line
      ] ++ addExtraDepsRecommendations

  where
    exceptions' = nubOrd exceptions

    addExtraDepsRecommendations
      | Map.null extras = []
      | (Just _) <- Map.lookup (mkPackageName "base") extras =
          [ "  *" <+> align (flow "Build requires unattainable version of base. Since base is a part of GHC, you most likely need to use a different GHC version with the matching base.")
           , line
          ]
      | otherwise =
         [ "  *" <+> align
           (style Recommendation (flow "Recommended action:") <+>
            flow "try adding the following to your extra-deps in" <+>
            toStyleDoc (display stackYaml) <> ":")
         , line <> line
         , vsep (map pprintExtra (Map.toList extras))
         , line
         ]

    extras = Map.unions $ map getExtras exceptions'
    getExtras DependencyCycleDetected{} = Map.empty
    getExtras UnknownPackage{} = Map.empty
    getExtras (DependencyPlanFailures _ m) =
       Map.unions $ map go $ Map.toList m
     where
       -- TODO: Likely a good idea to distinguish these to the user.  In particular, for DependencyMismatch
       go (name, (_range, Just (version,cabalHash), NotInBuildPlan)) =
           Map.singleton name (version,cabalHash)
       go (name, (_range, Just (version,cabalHash), DependencyMismatch{})) =
           Map.singleton name (version, cabalHash)
       go _ = Map.empty
    pprintExtra (name, (version, BlobKey cabalHash cabalSize)) =
      let cfInfo = CFIHash cabalHash (Just cabalSize)
          packageIdRev = PackageIdentifierRevision name version cfInfo
       in fromString $ T.unpack $ utf8BuilderToText $ RIO.display packageIdRev

    allNotInBuildPlan = Set.fromList $ concatMap toNotInBuildPlan exceptions'
    toNotInBuildPlan (DependencyPlanFailures _ pDeps) =
      map fst $ filter (\(_, (_, _, badDep)) -> badDep == NotInBuildPlan) $ Map.toList pDeps
    toNotInBuildPlan _ = []

    -- This checks if 'allow-newer: true' could resolve all issues.
    onlyHasDependencyMismatches = all go exceptions'
      where
        go DependencyCycleDetected{} = False
        go UnknownPackage{} = False
        go (DependencyPlanFailures _ m) =
          all (\(_, _, depErr) -> isMismatch depErr) (M.elems m)
        isMismatch DependencyMismatch{} = True
        isMismatch Couldn'tResolveItsDependencies{} = True
        isMismatch _ = False

    pprintException (DependencyCycleDetected pNames) = Just $
        flow "Dependency cycle detected in packages:" <> line <>
        indent 4 (encloseSep "[" "]" "," (map (style Error . fromString . packageNameString) pNames))
    pprintException (DependencyPlanFailures pkg pDeps) =
        case mapMaybe pprintDep (Map.toList pDeps) of
            [] -> Nothing
            depErrors -> Just $
                flow "In the dependencies for" <+> pkgIdent <>
                pprintFlags (packageFlags pkg) <> ":" <> line <>
                indent 4 (vsep depErrors) <>
                case getShortestDepsPath parentMap wanted (packageName pkg) of
                    Nothing -> line <> flow "needed for unknown reason - stack invariant violated."
                    Just [] -> line <> flow "needed since" <+> pkgName <+> flow "is a build target."
                    Just (target:path) -> line <> flow "needed due to" <+> encloseSep "" "" " -> " pathElems
                      where
                        pathElems =
                            [style Target . fromString . packageIdentifierString $ target] ++
                            map (fromString . packageIdentifierString) path ++
                            [pkgIdent]
              where
                pkgName = style Current . fromString . packageNameString $ packageName pkg
                pkgIdent = style Current . fromString . packageIdentifierString $ packageIdentifier pkg
    -- Skip these when they are redundant with 'NotInBuildPlan' info.
    pprintException (UnknownPackage name)
        | name `Set.member` allNotInBuildPlan = Nothing
        | name `Set.member` wiredInPackages =
            Just $ flow "Can't build a package with same name as a wired-in-package:" <+> (style Current . fromString . packageNameString $ name)
        | otherwise = Just $ flow "Unknown package:" <+> (style Current . fromString . packageNameString $ name)

    pprintFlags flags
        | Map.null flags = ""
        | otherwise = parens $ sep $ map pprintFlag $ Map.toList flags
    pprintFlag (name, True) = "+" <> fromString (show name)
    pprintFlag (name, False) = "-" <> fromString (show name)

    pprintDep (name, (range, mlatestApplicable, badDep)) = case badDep of
        NotInBuildPlan -> Just $
            style Error (fromString $ packageNameString name) <+>
            align ((if range == Cabal.anyVersion
                      then flow "needed"
                      else flow "must match" <+> goodRange) <> "," <> softline <>
                   flow "but the stack configuration has no specified version" <+>
                   latestApplicable Nothing)
        -- TODO: For local packages, suggest editing constraints
        DependencyMismatch version -> Just $
            (style Error . fromString . packageIdentifierString) (PackageIdentifier name version) <+>
            align (flow "from stack configuration does not match" <+> goodRange <+>
                   latestApplicable (Just version))
        -- I think the main useful info is these explain why missing
        -- packages are needed. Instead lets give the user the shortest
        -- path from a target to the package.
        Couldn'tResolveItsDependencies _version -> Nothing
        HasNoLibrary -> Just $
            style Error (fromString $ packageNameString name) <+>
            align (flow "is a library dependency, but the package provides no library")
        BDDependencyCycleDetected names -> Just $
            style Error (fromString $ packageNameString name) <+>
            align (flow $ "dependency cycle detected: " ++ intercalate ", " (map packageNameString names))
      where
        goodRange = style Good (fromString (Cabal.display range))
        latestApplicable mversion =
            case mlatestApplicable of
                Nothing
                    | isNothing mversion ->
                        flow "(no package with that name found, perhaps there is a typo in a package's build-depends or an omission from the stack.yaml packages list?)"
                    | otherwise -> ""
                Just (laVer, _)
                    | Just laVer == mversion -> softline <>
                        flow "(latest matching version is specified)"
                    | otherwise -> softline <>
                        flow "(latest matching version is" <+> style Good (fromString $ versionString laVer) <> ")"

-- | Get the shortest reason for the package to be in the build plan. In
-- other words, trace the parent dependencies back to a 'wanted'
-- package.
getShortestDepsPath
    :: ParentMap
    -> Set PackageName
    -> PackageName
    -> Maybe [PackageIdentifier]
getShortestDepsPath (MonoidMap parentsMap) wanted name =
    if Set.member name wanted
        then Just []
        else case M.lookup name parentsMap of
            Nothing -> Nothing
            Just (_, parents) -> Just $ findShortest 256 paths0
              where
                paths0 = M.fromList $ map (\(ident, _) -> (pkgName ident, startDepsPath ident)) parents
  where
    -- The 'paths' map is a map from PackageName to the shortest path
    -- found to get there. It is the frontier of our breadth-first
    -- search of dependencies.
    findShortest :: Int -> Map PackageName DepsPath -> [PackageIdentifier]
    findShortest fuel _ | fuel <= 0 =
        [PackageIdentifier (mkPackageName "stack-ran-out-of-jet-fuel") (mkVersion [0])]
    findShortest _ paths | M.null paths = []
    findShortest fuel paths =
        case targets of
            [] -> findShortest (fuel - 1) $ M.fromListWith chooseBest $ concatMap extendPath recurses
            _ -> let (DepsPath _ _ path) = minimum (map snd targets) in path
      where
        (targets, recurses) = partition (\(n, _) -> n `Set.member` wanted) (M.toList paths)
    chooseBest :: DepsPath -> DepsPath -> DepsPath
    chooseBest x y = if x > y then x else y
    -- Extend a path to all its parents.
    extendPath :: (PackageName, DepsPath) -> [(PackageName, DepsPath)]
    extendPath (n, dp) =
        case M.lookup n parentsMap of
            Nothing -> []
            Just (_, parents) -> map (\(pkgId, _) -> (pkgName pkgId, extendDepsPath pkgId dp)) parents

data DepsPath = DepsPath
    { dpLength :: Int -- ^ Length of dpPath
    , dpNameLength :: Int -- ^ Length of package names combined
    , dpPath :: [PackageIdentifier] -- ^ A path where the packages later
                                    -- in the list depend on those that
                                    -- come earlier
    }
    deriving (Eq, Ord, Show)

startDepsPath :: PackageIdentifier -> DepsPath
startDepsPath ident = DepsPath
    { dpLength = 1
    , dpNameLength = length (packageNameString (pkgName ident))
    , dpPath = [ident]
    }

extendDepsPath :: PackageIdentifier -> DepsPath -> DepsPath
extendDepsPath ident dp = DepsPath
    { dpLength = dpLength dp + 1
    , dpNameLength = dpNameLength dp + length (packageNameString (pkgName ident))
    , dpPath = [ident]
    }

-- Switch this to 'True' to enable some debugging putStrLn in this module
planDebug :: MonadIO m => String -> m ()
planDebug = if False then liftIO . putStrLn else \_ -> return ()
