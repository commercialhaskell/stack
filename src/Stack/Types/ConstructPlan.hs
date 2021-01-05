{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack.Types.ConstructPlan (
  PackageInfo(..),
  psForceDirty,
  psDirty,
  psLocal,
  psLocation,
  combineSourceInstalled,
  CombinedMap,
  combineMap,
  AddDepRes(..),
  ParentMap,
  PlanDraft(..),
  ConstructPlanMonad,
  ConstructPlanState,
  getLatestApplicableVersionAndRev,
  adrHasLibrary,
  Ctx(..),
  UnregisterState(..),
  NotOnlyLocal(..),
  ToolWarning(..),
  toolWarningText,
  DepsPath(..),
  startDepsPath,
  extendDepsPath,
  ConstructPlanException(..),
  LatestApplicableVersion,
  BadDependency(..),
  -- * The ConstructPlan state related functions.
  iterateOnPackageDeps
) where

import Stack.Prelude
import Stack.Types.Config
    (DumpPackage,  Curator,
      EnvConfig,
      HasBuildConfig,
      HasCompiler(..),
      HasConfig(..),
      HasEnvConfig(..),
      HasGHCVariant,
      HasPlatform,
      HasRunner(..),
      HasSourceMap(..) )
import Stack.Types.NamedComponent
    ( ComponentMapName,
      ComponentMap(exeComp, libComp),
      ComponentBuildInfo(cbiDependencyList, cbiExeDependencyList),
      NamedComponent,
      intersectComponentMap,
      toComponentNameList )
import Stack.Types.GhcPkgId ( GhcPkgId )
import Stack.Types.Version ( latestApplicableVersion )
import Stack.Types.Build
    ( psVersion,
      InstallLocation(..),
      Installed(..),
      LocalPackage(lpPackage, lpDirtyFiles, lpForceDirty),
      BaseConfigOpts,
      Task(taskType),
      TaskType(TTRemotePackage, TTLocalMutable) )
import Stack.Types.Package
    ( InstalledMap,
      PackageSource(..),
      DepType(AsBuildTool),
      DepValue(DepValue, dvType),
      Package(packageComponentBuildInfo, packageDeps, packageLibraries,
              packageInternalLibraries),
      ExeName(ExeName),
      PackageLibraries(NoLibraries, HasLibraries),
      runMemoizedWith,
      installedVersion )
import qualified Data.Map.Strict as Map
import Control.Monad.Trans.RWS.Strict (RWST)
import Distribution.Version (VersionRange)
import Data.Monoid.Map ( MonoidMap )
import Generics.Deriving.Monoid (memptydefault, mappenddefault)
import RIO.PrettyPrint ( HasStylesUpdate(..), HasTerm(..) )
import RIO.Process ( HasProcessContext(..) )
import qualified Data.Text as T
import Data.List (intercalate)
import qualified Data.Set as Set
import Distribution.Types.ExeDependency
    ( ExeDependency(ExeDependency) )
import Distribution.PackageDescription
    ( LibraryName(LMainLibName) )
import Distribution.Simple (Dependency(Dependency))

data PackageInfo
  = -- | This indicates that the package is already installed, and
    -- that we shouldn't build it from source. This is only the case
    -- for global packages.
    PIOnlyInstalled InstallLocation Installed
  | -- | This indicates that the package isn't installed, and we know
    -- where to find its source.
    PIOnlySource PackageSource
  | -- | This indicates that the package is installed and we know
    -- where to find its source. We may want to reinstall from source.
    PIBoth PackageSource Installed
  deriving (Show)

psForceDirty :: PackageSource -> Bool
psForceDirty (PSFilePath lp) = lpForceDirty lp
psForceDirty PSRemote{} = False

psDirty
  :: (MonadIO m, HasEnvConfig env, MonadReader env m)
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

-- | This is the result when you add a package to
-- the 'PlanDraft'. You look for a pre-installed
-- one or create a new task to install it. 
data AddDepRes
    = ADRToInstall !Task
    | ADRFound !InstallLocation !Installed
    deriving Show

type ParentMap = MonoidMap PackageName (First Version, [(PackageIdentifier, VersionRange)])
 
-- | This is a temporary object used to build the final 'Plan' object.
-- It's the outcome of running the 'ConstructPlanMonad'.
--
-- The monoid/semigroup generic instances enable the common @tell mempty{pdX = ..}@
-- idiom in the 'ConstructPlanMonad' without losing information.
data PlanDraft = PlanDraft
    { pdFinals :: !(Map PackageName (Either ConstructPlanException Task))
    -- ^ The finals exist because we want to run benchmarks and tests
    -- after having built all the libraries/executables. So this should be all about
    -- benchmark and tests.
    , pdInstall :: !(Map Text InstallLocation)
    -- ^ executable to be installed, and location where the binary is placed
    , pdDirty :: !(Map PackageName Text)
    -- ^ why a local package is considered dirty
    , pdWarnings :: !([Text] -> [Text])
    -- ^ Warnings
    , pdParents :: !ParentMap
    -- ^ Which packages a given package depends on, along with the package's version
    } deriving Generic
instance Semigroup PlanDraft where
    (<>) = mappenddefault
instance Monoid PlanDraft where
    mempty = memptydefault
    mappend = (<>)

-- | A monad transformer reading an environment of type 'Ctx',
-- collecting an output of type 'PlanDraft' and updating a state of type 
-- 'ConstructPlanState' to an inner monad 'IO'.
--      - <https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader-Class.html R stands for read>,
--      - <http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Writer-Class.html W stands for write>,
--      - <http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Class.html S stands for state>,
type ConstructPlanMonad = RWST -- TODO replace with more efficient WS stack on top of StackT
    Ctx
    PlanDraft
    ConstructPlanState
    IO

getLatestApplicableVersionAndRev :: PackageName -> VersionRange -> ConstructPlanMonad (Maybe (Version, BlobKey))
getLatestApplicableVersionAndRev depname range = do
  ctx <- ask
  vsAndRevs <- runRIO ctx $ getHackagePackageVersions YesRequireHackageIndex UsePreferredVersions depname
  pure $ do
    lappVer <- latestApplicableVersion range $ Map.keysSet vsAndRevs
    revs <- Map.lookup lappVer vsAndRevs
    (cabalHash, _) <- Map.maxView revs
    Just (lappVer, cabalHash)

adrHasLibrary :: AddDepRes -> Bool
adrHasLibrary addDepRes = case addDepRes of
  (ADRToInstall task) -> taskHasLibrary task
  (ADRFound _ Library{}) -> True
  (ADRFound _ Executable{}) -> False
  where
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

data Ctx = Ctx
    { baseConfigOpts :: !BaseConfigOpts
    , loadPackage    :: !(PackageLocationImmutable -> Map FlagName Bool -> [Text] -> [Text] -> ConstructPlanMonad Package)
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

-- | State to be maintained during the calculation of local packages
-- to unregister.
data UnregisterState = UnregisterState
    { usToUnregister :: !(Map GhcPkgId (PackageIdentifier, Text))
    , usKeep :: ![DumpPackage]
    , usAnyAdded :: !Bool
    }

-- | Only used when throwing an exception in 'errorOnSnapshot'
data NotOnlyLocal = NotOnlyLocal [PackageName] [Text]

instance Show NotOnlyLocal where
  show (NotOnlyLocal packages exes) = concat
    [ "Specified only-locals, but I need to build snapshot contents:\n"
    , if null packages then "" else concat
        [ "Packages: "
        , intercalate ", " (map packageNameString packages)
        , "\n"
        ]
    , if null exes then "" else concat
        [ "Executables: "
        , intercalate ", " (map T.unpack exes)
        , "\n"
        ]
    ]
instance Exception NotOnlyLocal

-- | Warn about tools in the snapshot definition. States the tool name
-- expected and the package name using it.
data ToolWarning = ToolWarning ExeName PackageName
  deriving Show

toolWarningText :: ToolWarning -> Text
toolWarningText (ToolWarning (ExeName toolName) pkgName') =
    "No packages found in snapshot which provide a " <>
    T.pack (show toolName) <>
    " executable, which is a build-tool dependency of " <>
    T.pack (packageNameString pkgName')

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

-- | This datatype contains all the potential errors when constructing the
-- 'Plan'. Its structure is awkward given the pretty similar cases in 
-- its 'DependencyPlanFailures' constructor part : 'BadDependency'
data ConstructPlanException
    = DependencyCycleDetected [PackageName]
    -- ^ Circular reference between package dependencies.
    -- e.g. A depends on B and B depends on A where A and B are package names.
    -- This is meant to be global, it also exists in 'DependencyPlanFailures' at the
    -- 'Package' dependencies level.
    | DependencyPlanFailures Package (Map PackageName (VersionRange, LatestApplicableVersion, BadDependency))
    -- ^ This is used to group all the potential errors while
    -- adding the dependency of a given 'Package'.
    -- The 'BadDependency' type replicate 'UnknownPackage' constructor and
    -- 'DependencyCycleDetected' constructor to some extent.
    | UnknownPackage PackageName -- TODO perhaps this constructor will be removed, and BadDependency will handle it all
    -- ^ Recommend adding to extra-deps, give a helpful version number?
    -- This happens when you reference a package in your cabal file (or package.yaml)
    -- which does not exist in the snapshot + added deps.
    -- It's the equivalent of 'NotInBuildPlan' for a 'BadDependency'.
    deriving (Typeable, Eq, Show)

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

--
-- * The package dependencies state.
--


-- | switch to 
type ConstructPlanState = Map PackageName (Either ConstructPlanException AddDepRes) 
type ConstructPlanState2 = Map PackageName (Either ConstructPlanException (IntersectionSet NamedComponent AddDepRes))

-- | A list of mutually exclusive sets along with a value.
newtype IntersectionSet a b = IntersectionSet [(Set a, b)] deriving (Show)

-- | Search for the presence of an intersection between the given set and any of the list
-- elements. If none is found, return the list with a new set/value pair, otherwise, return the
-- intersection set.
--
-- >>> let set = Set.singleton
-- >>> let current = IntersectionSet [(set 1, "a"), (set 2, "b")]
-- >>> let addNoIntersect = addToIntersectionSet (set 3) "c" current
-- >>> let addWithIntersect = addToIntersectionSet (set 2) "d" current
-- >>> (addNoIntersect, addWithIntersect)
-- (Right (IntersectionSet [(fromList [3],"c"),(fromList [1],"a"),(fromList [2],"b")]),Left (fromList [2]))
addToIntersectionSet :: Ord a =>
  Set a -> b -> IntersectionSet a b -> Either (Set a) (IntersectionSet a b)
addToIntersectionSet givenSet val (IntersectionSet currentList)
  | Set.null globalIntersection = Right . IntersectionSet $ (givenSet, val) : currentList
  | otherwise = Left globalIntersection
  where
    globalIntersection = foldMap hasIntersection currentList
    hasIntersection tupleWithSet = givenSet `Set.intersection` (fst tupleWithSet)

-- | Add the current package & component set to the construct plan state,
-- trigger a failure in case of overlapping component sets.
-- If a failure already happened for the package keep it as-is.
addPackageTask ::
    PackageName ->
    Set NamedComponent ->
    AddDepRes ->
    ConstructPlanState2 ->
    ConstructPlanState2
addPackageTask pName compSet adr currentState = Map.alter insertion pName currentState
  where
    insertion maybeOldVal = case maybeOldVal of
      Nothing -> Just . Right . IntersectionSet $ [(compSet, adr)]
      Just (Left err) -> Just (Left err)
      Just (Right oldIntersectionSet) -> Just $ first (const $ DependencyCycleDetected []) addedRes
        where
          addedRes = addToIntersectionSet compSet adr oldIntersectionSet

-- | Retrieves all package deps and iterate on them.
iterateOnPackageDeps :: Monad m =>
  ((PackageName, DepValue, ComponentMapName) -> m (Either a b)) ->
  Package ->
  -- | The set of activated components.
  ComponentMapName ->
  m [Either a b]
iterateOnPackageDeps func package targetedCompMap = do
  packageSetupDepsRes <- forM (packageSetupDepsList) (\(pn, dV) -> func (pn, dV, mempty{libComp=mainLibDefault}))
  listOfListOfEither <- traverse iteration componentList
  pure $ join (packageSetupDepsRes : listOfListOfEither) -- forM (Map.toList $ packageDeps package) func
  where
    packageSetupDepsList = Map.toList $ (
      Map.filter (\DepValue{dvType= dT} -> dT == AsBuildTool) $ packageDeps package
      )
    iteration (_, bi) = do
      exeRes <- traverse handleExeDeps $ cbiExeDependencyList bi
      libRes <- traverse handleLibDeps $ cbiDependencyList bi
      pure $ exeRes <> libRes
    componentList = toComponentNameList restrictedPackageMap
    targetedCompDefaulted = if targetedCompMap == mempty then
      mempty{
        libComp = Map.mapMaybeWithKey (\a _ -> if a == LMainLibName then Just () else Nothing) $ libComp rawPackageCompMap,
        exeComp = Map.mapWithKey (\_ _ -> ()) $ exeComp rawPackageCompMap
        }
      else
      targetedCompMap
    restrictedPackageMap = intersectComponentMap rawPackageCompMap targetedCompDefaulted
    rawPackageCompMap = packageComponentBuildInfo package
    versionTable = packageDeps package
    handleLibDeps (Dependency pName _ libSet) = case Map.lookup pName versionTable of
      Nothing -> error "todo : no version for this package ??"
      Just depVal  -> func (pName, depVal, mempty{
          -- If nothing is specified we infer the deps is on the main library.
          libComp = if null libSet then Map.singleton LMainLibName () else foldMap (\k -> Map.singleton k ()) libSet }
          )
    handleExeDeps (ExeDependency pName compName _) = case Map.lookup pName versionTable of
      Nothing -> error "todo : no version for this package ??"
      Just depVal  -> func (pName, depVal, mempty{exeComp = Map.singleton compName ()})
    mainLibDefault = Map.singleton LMainLibName ()