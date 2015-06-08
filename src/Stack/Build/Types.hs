{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}

-- | All data types.

module Stack.Build.Types where

import Control.DeepSeq
import Control.Exception
import Data.Aeson
import Data.Binary (Binary(..))
import qualified Data.ByteString as S
import Data.Char (isSpace)
import Data.Data
import Data.Hashable
import Data.List (dropWhileEnd, nub)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Distribution.Package (Dependency)
import Distribution.Text (display)
import GHC.Generics
import Path (Path, Abs, File, Dir)
import Prelude hiding (FilePath)
import Stack.Package
import Stack.Types
import System.Exit (ExitCode)

----------------------------------------------
-- Exceptions
data StackBuildException
  = MissingTool Dependency
  | Couldn'tFindPkgId PackageName
  | MissingDep Package PackageName VersionRange
  | MissingDep2 PackageName PackageName VersionRange
  | MismatchedLocalDep PackageName Version PackageName VersionRange
  | MismatchedDep PackageName Version PackageName VersionRange
  | StackageDepVerMismatch PackageName Version VersionRange
  | StackageVersionMismatch PackageName Version Version
  | DependencyIssues [StackBuildException]
  | GHCVersionMismatch (Maybe Version) Version (Maybe (Path Abs File))
  -- ^ Path to the stack.yaml file
  | Couldn'tParseTargets [Text]
  | UnknownTargets [PackageName]
  | TestSuiteFailure (Path Abs File) (Maybe (Path Abs File)) ExitCode
  deriving (Typeable,Show)

instance Exception StackBuildException

data ConstructPlanException
    = SnapshotPackageDependsOnLocal PackageName PackageIdentifier
    -- ^ Recommend adding to extra-deps
    | DependencyCycleDetected [PackageName]
    | DependencyPlanFailures PackageName (Set PackageName) -- FIXME include version range violation info?
    | UnknownPackage PackageName
    -- ^ Recommend adding to extra-deps, give a helpful version number?
    | VersionOutsideRange PackageName PackageIdentifier VersionRange
    deriving (Typeable, Eq)

instance Show ConstructPlanException where
  show e =
    let details = case e of
         (SnapshotPackageDependsOnLocal pName pIdentifier) ->
           "Exception: Stack.Build.SnapshotPackageDependsOnLocal\n" ++
           "  Local package " ++ show pIdentifier ++ " is a dependency of snapshot package " ++ show pName ++ ".\n" ++
           "  Snapshot packages cannot depend on local packages,\n " ++
           "  should you add " ++ show pName ++ " to [extra-deps] in the project's stack.yaml?"
         (DependencyCycleDetected pNames) ->
           "Exception: Stack.Build.DependencyCycle\n" ++
           "  While checking call stack,\n" ++
           "  dependency cycle detected in packages:" ++ indent (appendLines pNames)
         (DependencyPlanFailures pName (Set.toList -> pDeps)) ->
           "Exception: Stack.Build.DependencyPlanFailures\n" ++
           "  Failure when adding dependencies:" ++ doubleIndent (appendLines pDeps) ++ "\n" ++
           "  needed for package: " ++ show pName
         (UnknownPackage pName) ->
             "Exception: Stack.Build.UnknownPackage\n" ++
             "  While attempting to add dependency,\n" ++
             "  Could not find package " ++ show pName  ++ "in known packages"
         (VersionOutsideRange pName pIdentifier versionRange) ->
             "Exception: Stack.Build.VersionOutsideRange\n" ++
             "  While adding dependency for package " ++ show pName ++ ",\n" ++
             "  " ++ dropQuotes (show pIdentifier) ++ " was found to be outside its allowed version range.\n" ++
             "  Allowed version range is " ++ display versionRange ++ ",\n" ++
             "  should you correct the version range for " ++ dropQuotes (show pIdentifier) ++ ", found in [extra-deps] in the project's stack.yaml?"
    in indent details
     where
      appendLines = foldr (\pName-> (++) ("\n" ++ show pName)) ""
      indent = dropWhileEnd isSpace . unlines . fmap (\line -> "  " ++ line) . lines
      dropQuotes = filter ((/=) '\"')
      doubleIndent = indent . indent

newtype ConstructPlanExceptions = ConstructPlanExceptions [ConstructPlanException]
    deriving (Typeable)
instance Exception ConstructPlanExceptions

instance Show ConstructPlanExceptions where
  show (ConstructPlanExceptions exceptions) =
    "Exception: Stack.Build.ConstuctPlanExceptions\n" ++
    "While constructing the BuildPlan the following exceptions were encountered:" ++
    appendExceptions (removeDuplicates exceptions)
     where
         appendExceptions = foldr (\e -> (++) ("\n\n--" ++ show e)) ""
         removeDuplicates = nub
 -- Supressing duplicate output

data UnpackedPackageHasWrongName = UnpackedPackageHasWrongName PackageIdentifier PackageName
    deriving (Show, Typeable)
instance Exception UnpackedPackageHasWrongName

data TestSuiteFailure2 = TestSuiteFailure2 PackageIdentifier (Map Text (Maybe ExitCode)) (Maybe (Path Abs File))
    deriving (Show, Typeable)
instance Exception TestSuiteFailure2

data CabalExitedUnsuccessfully = CabalExitedUnsuccessfully
    ExitCode
    PackageIdentifier
    (Path Abs File)  -- cabal Executable
    [String]         -- cabal arguments
    (Maybe (Path Abs File)) -- logfiles location
    S.ByteString     -- log contents
    deriving (Typeable)
instance Exception CabalExitedUnsuccessfully

instance Show CabalExitedUnsuccessfully where
  show (CabalExitedUnsuccessfully exitCode taskProvides' execName fullArgs logFiles _) =
    let fullCmd = (dropQuotes (show execName) ++ " " ++ (unwords fullArgs))
        logLocations = maybe "" (\fp -> "\n    Logs have been written to: " ++ show fp) logFiles
    in "\n--  Exception: CabalExitedUnsuccessfully\n" ++
       "    While building package " ++ dropQuotes (show taskProvides') ++ " using:\n" ++
       "      " ++ fullCmd ++ "\n" ++
       "    Process exited with code: " ++ show exitCode ++
       logLocations
     where
      -- appendLines = foldr (\pName-> (++) ("\n" ++ show pName)) ""
      -- indent = dropWhileEnd isSpace . unlines . fmap (\line -> "  " ++ line) . lines
      dropQuotes = filter ('\"' /=)
      -- doubleIndent = indent . indent


----------------------------------------------

-- | Configuration for building.
data BuildOpts =
  BuildOpts {boptsTargets :: !(Either [Text] [PackageName])
             -- ^ Right value indicates that we're only installing
             -- dependencies, no local packages
            ,boptsLibProfile :: !Bool
            ,boptsExeProfile :: !Bool
            ,boptsEnableOptimizations :: !(Maybe Bool)
            ,boptsFinalAction :: !FinalAction
            ,boptsDryrun :: !Bool
            ,boptsGhcOptions :: ![Text]
            ,boptsFlags :: !(Map PackageName (Map FlagName Bool))
            }
  deriving (Show)

-- | Configuration for testing.
data TestConfig =
  TestConfig {tconfigTargets :: ![Text]
             }
  deriving (Show)

-- | Configuration for haddocking.
data HaddockConfig =
  HaddockConfig {hconfigTargets :: ![Text]
                }
  deriving (Show)

-- | Configuration for benchmarking.
data BenchmarkConfig =
  BenchmarkConfig {benchTargets :: ![Text]
                  ,benchInDocker :: !Bool}
  deriving (Show)

-- | Generated config for a package build.
data GenConfig =
  GenConfig {gconfigOptimize :: !Bool
            ,gconfigLibProfiling :: !Bool
            ,gconfigExeProfiling :: !Bool
            ,gconfigGhcOptions :: ![Text]
            ,gconfigFlags :: !(Map FlagName Bool)
            ,gconfigPkgId :: Maybe GhcPkgId}
  deriving (Generic,Show)

instance FromJSON GenConfig
instance ToJSON GenConfig

defaultGenConfig :: GenConfig
defaultGenConfig =
    GenConfig {gconfigOptimize = False
              ,gconfigLibProfiling = False
              ,gconfigExeProfiling = False
              ,gconfigGhcOptions = []
              ,gconfigFlags = mempty
              ,gconfigPkgId = Nothing}

-- | Run a Setup.hs action after building a package, before installing.
data FinalAction
  = DoTests
  | DoBenchmarks
  | DoHaddock
  | DoNothing
  deriving (Eq,Bounded,Enum,Show)

data Dependencies =
  Dependencies {depsLibraries :: [PackageName]
               ,depsTools :: [PackageName]}
  deriving (Show,Typeable,Data)

-- | Used for mutex locking on the install step. Beats magic ().
data InstallLock = InstallLock

-- | Mutex for reading/writing .config files in dist/ of
-- packages. Shake works in parallel, without this there are race
-- conditions.
data ConfigLock = ConfigLock

-- | Package dependency oracle.
newtype PkgDepsOracle =
    PkgDeps PackageName
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

-- | A location to install a package into, either snapshot or local
data Location = Snap | Local
    deriving (Show, Eq)

-- | Datatype which tells how which version of a package to install and where
-- to install it into
class PackageInstallInfo a where
    piiVersion :: a -> Version
    piiLocation :: a -> Location

-- | Information on a locally available package of source code
data LocalPackage = LocalPackage
    { lpPackage        :: !Package         -- ^ The @Package@ info itself, after resolution with package flags
    , lpWanted         :: !Bool            -- ^ Is this package a \"wanted\" target based on command line input
    , lpDir            :: !(Path Abs Dir)  -- ^ Directory of the package.
    , lpCabalFile      :: !(Path Abs File) -- ^ The .cabal file
    , lpLastConfigOpts :: !(Maybe [Text])  -- ^ configure options used during last Setup.hs configure, if available
    , lpDirtyFiles     :: !Bool            -- ^ are there files that have changed since the last build?
    }
    deriving Show

-- | A task to perform when building
data Task = Task
    { taskProvides        :: !PackageIdentifier       -- ^ the package/version to be built
    , taskRequiresMissing :: !(Set PackageIdentifier) -- ^ dependencies needed to make this package which are not present
    , taskRequiresPresent :: !(Set GhcPkgId)          -- ^ dependencies needed which are present
    , taskType            :: !TaskType                -- ^ the task type, telling us how to build this
    }
    deriving Show

-- | The type of a task, either building local code or something from the
-- package index (upstream)
data TaskType = TTLocal LocalPackage NeededSteps
              | TTUpstream Package Location
    deriving Show

-- | How many steps must be taken when building
data NeededSteps = AllSteps | SkipConfig | JustFinal
    deriving (Show, Eq)

-- | A complete plan of what needs to be built and how to do it
data Plan = Plan
    { planTasks :: !(Map PackageName Task)
    , planUnregisterLocal :: !(Set GhcPkgId)
    }

-- | Basic information used to calculate what the configure options are
data BaseConfigOpts = BaseConfigOpts
    { bcoSnapDB :: !(Path Abs Dir)
    , bcoLocalDB :: !(Path Abs Dir)
    , bcoSnapInstallRoot :: !(Path Abs Dir)
    , bcoLocalInstallRoot :: !(Path Abs Dir)
    , bcoLibProfiling :: !Bool
    , bcoExeProfiling :: !Bool
    , bcoFinalAction :: !FinalAction
    , bcoGhcOptions :: ![Text]
    }
