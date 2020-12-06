{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stack.Types.ConstructPlan where

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
    (ExeName(ExeName),  installedVersion,
      runMemoizedWith,
      InstalledMap,
      Package(packageLibraries, packageInternalLibraries),
      PackageLibraries(NoLibraries, HasLibraries),
      PackageSource(..) )
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

type ConstructPlanState = Map PackageName (Either ConstructPlanException AddDepRes) 

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