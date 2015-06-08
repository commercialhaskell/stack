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
import Data.List (dropWhileEnd, nub, intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Distribution.Package (Dependency)
import Distribution.Text (display)
import GHC.Generics
import Path (Path, Abs, File, Dir, mkRelDir, toFilePath, (</>))
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
  | ConstructPlanExceptions [ConstructPlanException]
  deriving Typeable

instance Show StackBuildException where
    show (MissingTool dep) = "Missing build tool: " <> display dep
    show (Couldn'tFindPkgId name) =
              ("After installing " <> packageNameString name <>
               ", the package id couldn't be found " <> "(via ghc-pkg describe " <>
               packageNameString name <> "). This shouldn't happen, " <>
               "please report as a bug")
    show (MissingDep p d range) =
              "Missing dependency for package " <>
              packageNameString (packageName p) <>
              ": " <>
              packageNameString d <>
              " " <>
              display range
    show (MissingDep2 user dep range) =
              "Local package " <>
              packageNameString user <>
              " depends on " <>
              packageNameString dep <>
              " (" <>
              display range <>
              "), but it wasn't found. Perhaps add it to your local package list?"
    show (MismatchedLocalDep dep version user range) =
              "Mismatched local dependencies, " <>
              packageNameString user <>
              " depends on " <>
              packageNameString dep <>
              " (" <>
              display range <>
              "), but " <>
              versionString version <>
              " is provided locally"
    show (MismatchedDep dep version user range) =
              "Mismatched dependencies, " <>
              packageNameString user <>
              " depends on " <>
              packageNameString dep <>
              " (" <>
              display range <>
              "), but " <>
              versionString version <>
              " is provided by your snapshot"
    show (StackageDepVerMismatch name ver range) =
              ("The package '" <> packageNameString name <>
               "' in this Stackage snapshot is " <> versionString ver <>
               ", but there is an (unsatisfiable) local constraint of " <>
               display range <> ". Suggestion: " <>
               "Check your local package constraints and make them consistent with your current Stackage")
    show (StackageVersionMismatch name this that) =
              ("There was a mismatch between an installed package, " <>
               packageNameString name <> "==" <> versionString this <>
               " but this Stackage snapshot should be " <> versionString that)
    show (DependencyIssues es) =
              ("Dependency issues:\n" ++
               intercalate "\n"
                           (map show es))
    show (GHCVersionMismatch mactual expected mstack) = concat
                [ case mactual of
                    Nothing -> "No GHC found, expected version "
                    Just actual ->
                        "GHC version mismatched, found " ++
                        versionString actual ++
                        ", but expected version "
                , versionString expected
                , " (based on "
                , case mstack of
                    Nothing -> "command line arguments"
                    Just stack -> "resolver setting in " ++ toFilePath stack
                , "). Try running stack setup"
                ]
    show (Couldn'tParseTargets targets) = unlines
                $ "The following targets could not be parsed as package names or directories:"
                : map T.unpack targets
    show (UnknownTargets targets) =
                "The following target packages were not found: " ++
                intercalate ", " (map packageNameString targets)
    show (TestSuiteFailure exe mlogFile ec) = concat
                [ "Test suite "
                , toFilePath exe
                , " exited with code "
                , show ec
                , case mlogFile of
                    Nothing -> ""
                    Just logFile ->
                        ", log available at: " ++ toFilePath logFile
                ]
    show (ConstructPlanExceptions exceptions) =
        "Exception: Stack.Build.ConstuctPlanExceptions\n" ++
        "While constructing the BuildPlan the following exceptions were encountered:" ++
        appendExceptions (removeDuplicates exceptions)
         where
             appendExceptions = foldr (\e -> (++) ("\n\n--" ++ show e)) ""
             removeDuplicates = nub
     -- Supressing duplicate output

instance Exception StackBuildException

data ConstructPlanException
    = SnapshotPackageDependsOnLocal PackageName PackageIdentifier
    -- ^ Recommend adding to extra-deps
    | DependencyCycleDetected [PackageName]
    | DependencyPlanFailures PackageName (Map PackageName (VersionRange, BadDependency))
    | UnknownPackage PackageName
    -- ^ Recommend adding to extra-deps, give a helpful version number?
    deriving (Typeable, Eq)

-- | Reason why a dependency was not used
data BadDependency
    = NotInBuildPlan -- TODO add recommended version so it can be added to extra-deps
    | Couldn'tResolveItsDependencies
    | DependencyMismatch Version
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
         (DependencyPlanFailures pName (Map.toList -> pDeps)) ->
           "Exception: Stack.Build.DependencyPlanFailures\n" ++
           "  Failure when adding dependencies:" ++ doubleIndent (appendDeps pDeps) ++ "\n" ++
           "  needed for package: " ++ show pName
         (UnknownPackage pName) ->
             "Exception: Stack.Build.UnknownPackage\n" ++
             "  While attempting to add dependency,\n" ++
             "  Could not find package " ++ show pName  ++ "in known packages"
    in indent details
     where
      appendLines = foldr (\pName-> (++) ("\n" ++ show pName)) ""
      indent = dropWhileEnd isSpace . unlines . fmap (\line -> "  " ++ line) . lines
      doubleIndent = indent . indent
      appendDeps = foldr (\dep-> (++) ("\n" ++ showDep dep)) ""
      showDep (name, (range, badDep)) = concat
        [ show name
        , ": needed ("
        , display range
        , "), but "
        , case badDep of
            NotInBuildPlan -> "not present in build plan"
            Couldn'tResolveItsDependencies -> "couldn't resolve its dependencies"
            DependencyMismatch version -> versionString version ++ " found"
        ]
         {- TODO Perhaps change the showDep function to look more like this:
          dropQuotes = filter ((/=) '\"')
         (VersionOutsideRange pName pIdentifier versionRange) ->
             "Exception: Stack.Build.VersionOutsideRange\n" ++
             "  While adding dependency for package " ++ show pName ++ ",\n" ++
             "  " ++ dropQuotes (show pIdentifier) ++ " was found to be outside its allowed version range.\n" ++
             "  Allowed version range is " ++ display versionRange ++ ",\n" ++
             "  should you correct the version range for " ++ dropQuotes (show pIdentifier) ++ ", found in [extra-deps] in the project's stack.yaml?"
             -}

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
  show (CabalExitedUnsuccessfully exitCode taskProvides' execName fullArgs logFiles bs) =
    let fullCmd = (dropQuotes (show execName) ++ " " ++ (unwords fullArgs))
        logLocations = maybe "" (\fp -> "\n    Logs have been written to: " ++ show fp) logFiles
    in "\n--  Exception: CabalExitedUnsuccessfully\n" ++
       "    While building package " ++ dropQuotes (show taskProvides') ++ " using:\n" ++
       "      " ++ fullCmd ++ "\n" ++
       "    Process exited with code: " ++ show exitCode ++
       logLocations ++
       (if S.null bs
            then ""
            else "\n\n" ++ doubleIndent (T.unpack $ decodeUtf8With lenientDecode bs))
     where
      -- appendLines = foldr (\pName-> (++) ("\n" ++ show pName)) ""
      indent = dropWhileEnd isSpace . unlines . fmap (\line -> "  " ++ line) . lines
      dropQuotes = filter ('\"' /=)
      doubleIndent = indent . indent


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
    , lpLastConfigOpts :: !(Maybe [S.ByteString])  -- ^ configure options used during last Setup.hs configure, if available
    , lpDirtyFiles     :: !Bool            -- ^ are there files that have changed since the last build?
    }
    deriving Show

-- | A task to perform when building
data Task = Task
    { taskProvides        :: !PackageIdentifier        -- ^ the package/version to be built
    , taskType            :: !TaskType                 -- ^ the task type, telling us how to build this
    , taskConfigOpts      :: !TaskConfigOpts
    }
    deriving Show

-- | Given the IDs of any missing packages, produce the configure options
data TaskConfigOpts = TaskConfigOpts
    { tcoMissing :: !(Set PackageIdentifier)
      -- ^ Dependencies for which we don't yet have an GhcPkgId
    , tcoOpts    :: !(Set GhcPkgId -> [Text])
      -- ^ Produce the list of options given the missing @GhcPkgId@s
    }
instance Show TaskConfigOpts where
    show (TaskConfigOpts missing f) = concat
        [ "Missing: "
        , show missing
        , ". Without those: "
        , show $ f Set.empty
        ]

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

-- | Render a @BaseConfigOpts@ to an actual list of options
configureOpts :: BaseConfigOpts
              -> Set GhcPkgId -- ^ dependencies
              -> Bool -- ^ wanted?
              -> Location
              -> Map FlagName Bool
              -> [Text]
configureOpts bco deps wanted loc flags = map T.pack $ concat
    [ ["--user", "--package-db=clear", "--package-db=global"]
    , map (("--package-db=" ++) . toFilePath) $ case loc of
        Snap -> [bcoSnapDB bco]
        Local -> [bcoSnapDB bco, bcoLocalDB bco]
    , depOptions
    , [ "--libdir=" ++ toFilePath (installRoot </> $(mkRelDir "lib"))
      , "--bindir=" ++ toFilePath (installRoot </> bindirSuffix)
      , "--datadir=" ++ toFilePath (installRoot </> $(mkRelDir "share"))
      , "--docdir=" ++ toFilePath (installRoot </> $(mkRelDir "doc"))
      ]
    , ["--enable-library-profiling" | bcoLibProfiling bco || bcoExeProfiling bco]
    , ["--enable-executable-profiling" | bcoLibProfiling bco]
    , ["--enable-tests" | wanted && bcoFinalAction bco == DoTests]
    , ["--enable-benchmarks" | wanted && bcoFinalAction bco == DoBenchmarks]
    , map (\(name,enabled) ->
                       "-f" <>
                       (if enabled
                           then ""
                           else "-") <>
                       flagNameString name)
                    (Map.toList flags)
    -- FIXME Chris: where does this come from now? , ["--ghc-options=-O2" | gconfigOptimize gconfig]
    , if wanted
        then concatMap (\x -> ["--ghc-options", T.unpack x]) (bcoGhcOptions bco)
        else []
    ]
  where
    installRoot =
        case loc of
            Snap -> bcoSnapInstallRoot bco
            Local -> bcoLocalInstallRoot bco

    depOptions = map toDepOption $ Set.toList deps

    {- TODO does this work with some versions of Cabal?
    toDepOption gid = T.pack $ concat
        [ "--dependency="
        , packageNameString $ packageIdentifierName $ ghcPkgIdPackageIdentifier gid
        , "="
        , ghcPkgIdString gid
        ]
    -}
    toDepOption gid = concat
        [ "--constraint="
        , packageNameString name
        , "=="
        , versionString version
        ]
      where
        PackageIdentifier name version = ghcPkgIdPackageIdentifier gid
