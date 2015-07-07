{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}

-- | All data types.

module Stack.Build.Types
    (StackBuildException(..)
    ,InstallLocation(..)
    ,ModTime
    ,modTime
    ,Installed(..)
    ,PackageInstallInfo(..)
    ,Task(..)
    ,LocalPackage(..)
    ,BaseConfigOpts(..)
    ,Plan(..)
    ,InstallExesPlan(..)
    ,InstallDest(..)
    ,FinalAction(..)
    ,BuildOpts(..)
    ,defaultBuildOpts
    ,TaskType(..)
    ,TaskConfigOpts(..)
    ,ConfigCache(..)
    ,ConstructPlanException(..)
    ,configureOpts
    ,BadDependency(..)
    ,wantedLocalPackages
    ,FileCacheInfo (..))
    where

import           Control.DeepSeq
import           Control.Exception

import           Data.Binary (Binary(..))
import qualified Data.ByteString as S
import           Data.Char (isSpace)
import           Data.Data
import           Data.Hashable
import           Data.List (dropWhileEnd, nub, intercalate)
import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Word (Word64)
import           Distribution.System (Arch)
import           Distribution.Text (display)
import           GHC.Generics
import           Path (Path, Abs, File, Dir, mkRelDir, toFilePath, parseRelDir, (</>))
import           Prelude
import           Stack.Package
import           Stack.Types
import           System.Exit (ExitCode)
import           System.FilePath (dropTrailingPathSeparator, pathSeparator)

----------------------------------------------
-- Exceptions
data StackBuildException
  = Couldn'tFindPkgId PackageName
  | GHCVersionMismatch (Maybe (Version, Arch)) (Version, Arch) (Maybe (Path Abs File))
  -- ^ Path to the stack.yaml file
  | Couldn'tParseTargets [Text]
  | UnknownTargets
    (Set PackageName) -- no known version
    (Map PackageName Version) -- not in snapshot, here's the most recent version in the index
    (Path Abs File) -- stack.yaml
  | TestSuiteFailure PackageIdentifier (Map Text (Maybe ExitCode)) (Maybe (Path Abs File)) S.ByteString
  | ConstructPlanExceptions
        [ConstructPlanException]
        (Path Abs File) -- stack.yaml
  | CabalExitedUnsuccessfully
        ExitCode
        PackageIdentifier
        (Path Abs File)  -- cabal Executable
        [String]         -- cabal arguments
        (Maybe (Path Abs File)) -- logfiles location
        S.ByteString     -- log contents
  | ExecutionFailure [SomeException]
  deriving Typeable

instance Show StackBuildException where
    show (Couldn'tFindPkgId name) =
              ("After installing " <> packageNameString name <>
               ", the package id couldn't be found " <> "(via ghc-pkg describe " <>
               packageNameString name <> "). This shouldn't happen, " <>
               "please report as a bug")
    show (GHCVersionMismatch mactual (expected, earch) mstack) = concat
                [ case mactual of
                    Nothing -> "No GHC found, expected version "
                    Just (actual, arch) -> concat
                        [ "GHC version mismatched, found "
                        , versionString actual
                        , " ("
                        , display arch
                        , ")"
                        , ", but expected version "
                        ]
                , versionString expected
                , " ("
                , display earch
                , ") (based on "
                , case mstack of
                    Nothing -> "command line arguments"
                    Just stack -> "resolver setting in " ++ toFilePath stack
                , "). Try running stack setup"
                ]
    show (Couldn'tParseTargets targets) = unlines
                $ "The following targets could not be parsed as package names or directories:"
                : map T.unpack targets
    show (UnknownTargets noKnown notInSnapshot stackYaml) =
        unlines $ noKnown' ++ notInSnapshot'
      where
        noKnown'
            | Set.null noKnown = []
            | otherwise = return $
                "The following target packages were not found: " ++
                intercalate ", " (map packageNameString $ Set.toList noKnown)
        notInSnapshot'
            | Map.null notInSnapshot = []
            | otherwise =
                  "The following packages are not in your snapshot, but exist"
                : "in your package index. Recommended action: add them to your"
                : ("extra-deps in " ++ toFilePath stackYaml)
                : "(Note: these are the most recent versions,"
                : "but there's no guarantee that they'll build together)."
                : ""
                : map
                    (\(name, version) -> "- " ++ packageIdentifierString
                        (PackageIdentifier name version))
                    (Map.toList notInSnapshot)
    show (TestSuiteFailure ident codes mlogFile bs) = unlines $ concat
        [ ["Test suite failure for package " ++ packageIdentifierString ident]
        , flip map (Map.toList codes) $ \(name, mcode) -> concat
            [ "    "
            , T.unpack name
            , ": "
            , case mcode of
                Nothing -> " executable not found"
                Just ec -> " exited with: " ++ show ec
            ]
        , return $ case mlogFile of
            Nothing -> "Logs printed to console"
            -- TODO Should we load up the full error output and print it here?
            Just logFile -> "Full log available at " ++ toFilePath logFile
        , if S.null bs
            then []
            else ["", "", doubleIndent $ T.unpack $ decodeUtf8With lenientDecode bs]
        ]
         where
          indent = dropWhileEnd isSpace . unlines . fmap (\line -> "  " ++ line) . lines
          doubleIndent = indent . indent
    show (ConstructPlanExceptions exceptions stackYaml) =
        "While constructing the BuildPlan the following exceptions were encountered:" ++
        appendExceptions exceptions' ++
        if Map.null extras then "" else (unlines
                $ ("\n\nRecommended action: try adding the following to your extra-deps in "
                    ++ toFilePath stackYaml)
                : map (\(name, version) -> concat
                    [ "- "
                    , packageNameString name
                    , "-"
                    , versionString version
                    ]) (Map.toList extras)
                    ++ ["", "You may also want to try the 'stack solver' command"]
                )
         where
             exceptions' = removeDuplicates exceptions
             appendExceptions = foldr (\e -> (++) ("\n\n--" ++ show e)) ""
             removeDuplicates = nub
             extras = Map.unions $ map getExtras exceptions'

             getExtras (DependencyCycleDetected _) = Map.empty
             getExtras (UnknownPackage _) = Map.empty
             getExtras (DependencyPlanFailures _ m) =
                Map.unions $ map go $ Map.toList m
              where
                go (name, (_range, Just version, NotInBuildPlan)) =
                    Map.singleton name version
                go _ = Map.empty
     -- Supressing duplicate output
    show (CabalExitedUnsuccessfully exitCode taskProvides' execName fullArgs logFiles bs) =
        let fullCmd = (dropQuotes (show execName) ++ " " ++ (unwords fullArgs))
            logLocations = maybe "" (\fp -> "\n    Logs have been written to: " ++ show fp) logFiles
        in "\n--  While building package " ++ dropQuotes (show taskProvides') ++ " using:\n" ++
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
    show (ExecutionFailure es) = intercalate "\n\n" $ map show es

instance Exception StackBuildException

data ConstructPlanException
    = DependencyCycleDetected [PackageName]
    | DependencyPlanFailures PackageIdentifier (Map PackageName (VersionRange, LatestVersion, BadDependency))
    | UnknownPackage PackageName -- TODO perhaps this constructor will be removed, and BadDependency will handle it all
    -- ^ Recommend adding to extra-deps, give a helpful version number?
    deriving (Typeable, Eq)

-- | For display purposes only, Nothing if package not found
type LatestVersion = Maybe Version

-- | Reason why a dependency was not used
data BadDependency
    = NotInBuildPlan
    | Couldn'tResolveItsDependencies
    | DependencyMismatch Version
    deriving (Typeable, Eq)

instance Show ConstructPlanException where
  show e =
    let details = case e of
         (DependencyCycleDetected pNames) ->
           "While checking call stack,\n" ++
           "  dependency cycle detected in packages:" ++ indent (appendLines pNames)
         (DependencyPlanFailures pIdent (Map.toList -> pDeps)) ->
           "Failure when adding dependencies:" ++ doubleIndent (appendDeps pDeps) ++ "\n" ++
           "  needed for package: " ++ packageIdentifierString pIdent
         (UnknownPackage pName) ->
             "While attempting to add dependency,\n" ++
             "  Could not find package " ++ show pName  ++ " in known packages"
    in indent details
     where
      appendLines = foldr (\pName-> (++) ("\n" ++ show pName)) ""
      indent = dropWhileEnd isSpace . unlines . fmap (\line -> "  " ++ line) . lines
      doubleIndent = indent . indent
      appendDeps = foldr (\dep-> (++) ("\n" ++ showDep dep)) ""
      showDep (name, (range, mlatest, badDep)) = concat
        [ show name
        , ": needed ("
        , display range
        , ")"
        , case mlatest of
            Nothing -> ""
            Just latest -> ", latest is " ++ versionString latest
        , ", but "
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


----------------------------------------------

-- | Configuration for building.
data BuildOpts =
  BuildOpts {boptsTargets :: ![Text]
            ,boptsLibProfile :: !Bool
            ,boptsExeProfile :: !Bool
            ,boptsEnableOptimizations :: !(Maybe Bool)
            ,boptsHaddock :: !Bool
            -- ^ Build haddocks?
            ,boptsHaddockDeps :: !(Maybe Bool)
            -- ^ Build haddocks for dependencies?
            ,boptsFinalAction :: !FinalAction
            ,boptsDryrun :: !Bool
            ,boptsGhcOptions :: ![Text]
            ,boptsFlags :: !(Map (Maybe PackageName) (Map FlagName Bool))
            ,boptsInstallExes :: InstallDest
            -- ^ Install executables to user path after building?
            ,boptsPreFetch :: !Bool
            -- ^ Fetch all packages immediately
            ,boptsTestArgs :: ![String]
            -- ^ Arguments to pass to the test suites if we're running them.
            ,boptsOnlySnapshot :: !Bool
            -- ^ Only install packages in the snapshot database, skipping
            -- packages intended for the local database.
            ,boptsCoverage :: !Bool
            -- ^ Enable code coverage report generation for test
            -- suites.
            ,boptsFileWatch :: !Bool
            -- ^ Watch files for changes and automatically rebuild
            ,boptsKeepGoing :: !(Maybe Bool)
            -- ^ Keep building/running after failure
            ,boptsNoTests :: !Bool
            -- ^ If set, don't run the tests
            }
  deriving (Show)

defaultBuildOpts :: BuildOpts
defaultBuildOpts = BuildOpts
    { boptsTargets = []
    , boptsLibProfile = False
    , boptsExeProfile = False
    , boptsEnableOptimizations = Nothing
    , boptsHaddock = False
    , boptsHaddockDeps = Nothing
    , boptsFinalAction = DoNothing
    , boptsDryrun = False
    , boptsGhcOptions = []
    , boptsFlags = Map.empty
    , boptsInstallExes = NoInstall
    , boptsPreFetch = False
    , boptsTestArgs = []
    , boptsOnlySnapshot = False
    , boptsCoverage = False
    , boptsFileWatch = False
    , boptsKeepGoing = Nothing
    , boptsNoTests = False
    }

-- | Run a Setup.hs action after building a package, before installing.
data FinalAction
  = DoTests
      Bool -- rerun tests which already passed?
  | DoBenchmarks
  | DoNothing
  deriving (Eq,Show)

-- | Package dependency oracle.
newtype PkgDepsOracle =
    PkgDeps PackageName
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

-- | A location to install a package into, either snapshot or local
data InstallLocation = Snap | Local
    deriving (Show, Eq)
instance Monoid InstallLocation where
    mempty = Snap
    mappend Local _ = Local
    mappend _ Local = Local
    mappend Snap Snap = Snap

-- | Datatype which tells how which version of a package to install and where
-- to install it into
class PackageInstallInfo a where
    piiVersion :: a -> Version
    piiLocation :: a -> InstallLocation

-- | Information on a locally available package of source code
data LocalPackage = LocalPackage
    { lpPackage        :: !Package         -- ^ The @Package@ info itself, after resolution with package flags, not including any final actions
    , lpPackageFinal   :: !Package         -- ^ Same as lpPackage, but with any test suites or benchmarks enabled as necessary
    , lpWanted         :: !Bool            -- ^ Is this package a \"wanted\" target based on command line input
    , lpDir            :: !(Path Abs Dir)  -- ^ Directory of the package.
    , lpCabalFile      :: !(Path Abs File) -- ^ The .cabal file
    , lpDirtyFiles     :: !Bool            -- ^ are there files that have changed since the last build?
    , lpNewBuildCache  :: !(Map FilePath FileCacheInfo) -- ^ current state of the files
    , lpFiles          :: !(Set (Path Abs File)) -- ^ all files used by this package
    , lpComponents     :: !(Set Text)      -- ^ components to build, passed directly to Setup.hs build
    }
    deriving Show

-- | Stored on disk to know whether the flags have changed or any
-- files have changed.
data ConfigCache = ConfigCache
    { configCacheOpts :: ![S.ByteString]
      -- ^ All options used for this package.
    , configCacheDeps :: !(Set GhcPkgId)
      -- ^ The GhcPkgIds of all of the dependencies. Since Cabal doesn't take
      -- the complete GhcPkgId (only a PackageIdentifier) in the configure
      -- options, just using the previous value is insufficient to know if
      -- dependencies have changed.
    , configCacheComponents :: !(Set S.ByteString)
      -- ^ The components to be built. It's a bit of a hack to include this in
      -- here, as it's not a configure option (just a build option), but this
      -- is a convenient way to force compilation when the components change.
    , configCacheHaddock :: !Bool
      -- ^ Are haddocks to be built?
    }
    deriving (Generic,Eq,Show)
instance Binary ConfigCache

-- | A task to perform when building
data Task = Task
    { taskProvides        :: !PackageIdentifier        -- ^ the package/version to be built
    , taskType            :: !TaskType                 -- ^ the task type, telling us how to build this
    , taskConfigOpts      :: !TaskConfigOpts
    , taskPresent         :: !(Set GhcPkgId)           -- ^ GhcPkgIds of already-installed dependencies
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
data TaskType = TTLocal LocalPackage
              | TTUpstream Package InstallLocation
    deriving Show

-- | A complete plan of what needs to be built and how to do it
data Plan = Plan
    { planTasks :: !(Map PackageName Task)
    , planFinals :: !(Map PackageName Task)
    -- ^ Final actions to be taken (test, benchmark, etc)
    , planUnregisterLocal :: !(Map GhcPkgId Text)
    -- ^ Text is reason we're unregistering, for display only
    , planInstallExes :: !InstallExesPlan
    -- ^ Executables that should be installed after successful building
    }
    deriving Show

data InstallExesPlan = InstallExesPlan
    { installExesPlanFlag :: !Bool
      -- ^ Install executables if True
    , installExesPlanDestDir :: !(Maybe (Path Abs Dir))
      -- ^ custom destination directory
    , installExesNamesLoc :: !(Map Text InstallLocation)
      -- ^ Map exename bindir. Default Map.Empty for configLocalBin
    }
    deriving Show

data InstallDest = NoInstall | DefaultInstall | InstallDir (Path Abs Dir)
    deriving Show

-- | Basic information used to calculate what the configure options are
data BaseConfigOpts = BaseConfigOpts
    { bcoSnapDB :: !(Path Abs Dir)
    , bcoLocalDB :: !(Path Abs Dir)
    , bcoSnapInstallRoot :: !(Path Abs Dir)
    , bcoLocalInstallRoot :: !(Path Abs Dir)
    , bcoBuildOpts :: !BuildOpts
    }

-- | Render a @BaseConfigOpts@ to an actual list of options
configureOpts :: EnvConfig
              -> BaseConfigOpts
              -> Set GhcPkgId -- ^ dependencies
              -> Bool -- ^ wanted?
              -> InstallLocation
              -> Package
              -> [Text]
configureOpts econfig bco deps wanted loc package = map T.pack $ concat
    [ ["--user", "--package-db=clear", "--package-db=global"]
    , map (("--package-db=" ++) . toFilePath) $ case loc of
        Snap -> [bcoSnapDB bco]
        Local -> [bcoSnapDB bco, bcoLocalDB bco]
    , depOptions
    , [ "--libdir=" ++ toFilePathNoTrailingSlash (installRoot </> $(mkRelDir "lib"))
      , "--bindir=" ++ toFilePathNoTrailingSlash (installRoot </> bindirSuffix)
      , "--datadir=" ++ toFilePathNoTrailingSlash (installRoot </> $(mkRelDir "share"))
      , "--docdir=" ++ toFilePathNoTrailingSlash docDir
      , "--htmldir=" ++ toFilePathNoTrailingSlash docDir
      , "--haddockdir=" ++ toFilePathNoTrailingSlash docDir]
    , ["--enable-library-profiling" | boptsLibProfile bopts || boptsExeProfile bopts]
    , ["--enable-executable-profiling" | boptsExeProfile bopts]
    , map (\(name,enabled) ->
                       "-f" <>
                       (if enabled
                           then ""
                           else "-") <>
                       flagNameString name)
                    (Map.toList (packageFlags package))
    -- FIXME Chris: where does this come from now? , ["--ghc-options=-O2" | gconfigOptimize gconfig]
    , if wanted
        then concatMap (\x -> ["--ghc-options", T.unpack x]) (boptsGhcOptions bopts)
        else []
    , map (("--extra-include-dirs=" ++) . T.unpack) (Set.toList (configExtraIncludeDirs config))
    , map (("--extra-lib-dirs=" ++) . T.unpack) (Set.toList (configExtraLibDirs config))
    ]
  where
    config = getConfig econfig
    bopts = bcoBuildOpts bco
    toFilePathNoTrailingSlash = dropTrailingPathSeparator . toFilePath
    docDir =
        case pkgVerDir of
            Nothing -> installRoot </> docdirSuffix
            Just dir -> installRoot </> docdirSuffix </> dir
    installRoot =
        case loc of
            Snap -> bcoSnapInstallRoot bco
            Local -> bcoLocalInstallRoot bco
    pkgVerDir =
        parseRelDir (packageIdentifierString (PackageIdentifier (packageName package)
                                                                (packageVersion package)) ++
                     [pathSeparator])

    depOptions = map toDepOption $ Set.toList deps
      where
        toDepOption =
            if envConfigCabalVersion econfig >= $(mkVersion "1.22")
                then toDepOption1_22
                else toDepOption1_18

    toDepOption1_22 gid = concat
        [ "--dependency="
        , packageNameString $ packageIdentifierName $ ghcPkgIdPackageIdentifier gid
        , "="
        , ghcPkgIdString gid
        ]

    toDepOption1_18 gid = concat
        [ "--constraint="
        , packageNameString name
        , "=="
        , versionString version
        ]
      where
        PackageIdentifier name version = ghcPkgIdPackageIdentifier gid

-- | Get set of wanted package names from locals.
wantedLocalPackages :: [LocalPackage] -> Set PackageName
wantedLocalPackages = Set.fromList . map (packageName . lpPackage) . filter lpWanted

-- | Used for storage and comparison.
newtype ModTime = ModTime (Integer,Rational)
  deriving (Ord,Show,Generic,Eq)
instance Binary ModTime

-- | One-way conversion to serialized time.
modTime :: UTCTime -> ModTime
modTime x =
    ModTime
        ( toModifiedJulianDay
              (utctDay x)
        , toRational
              (utctDayTime x))

data Installed = Library GhcPkgId | Executable PackageIdentifier
    deriving (Show, Eq, Ord)

data FileCacheInfo = FileCacheInfo
    { fciModTime :: !ModTime
    , fciSize :: !Word64
    , fciHash :: !S.ByteString
    }
    deriving (Generic, Show)
instance Binary FileCacheInfo
