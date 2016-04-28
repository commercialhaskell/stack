{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Build-specific types.

module Stack.Types.Build
    (StackBuildException(..)
    ,FlagSource(..)
    ,UnusedFlags(..)
    ,InstallLocation(..)
    ,ModTime
    ,modTime
    ,Installed(..)
    ,PackageInstallInfo(..)
    ,Task(..)
    ,taskLocation
    ,LocalPackage(..)
    ,BaseConfigOpts(..)
    ,Plan(..)
    ,TestOpts(..)
    ,BenchmarkOpts(..)
    ,FileWatchOpts(..)
    ,BuildOpts(..)
    ,BuildSubset(..)
    ,defaultBuildOpts
    ,TaskType(..)
    ,TaskConfigOpts(..)
    ,ConfigCache(..)
    ,ConstructPlanException(..)
    ,configureOpts
    ,isStackOpt
    ,BadDependency(..)
    ,wantedLocalPackages
    ,FileCacheInfo (..)
    ,ConfigureOpts (..)
    ,PrecompiledCache (..))
    where

import           Control.DeepSeq
import           Control.Exception

import           Data.Binary (getWord8, putWord8, gput, gget)
import           Data.Binary.VersionTagged
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
import           Distribution.System (Arch)
import           Distribution.PackageDescription (TestSuiteInterface)
import           Distribution.Text (display)
import           GHC.Generics (Generic, from, to)
import           Path (Path, Abs, File, Dir, mkRelDir, toFilePath, parseRelDir, (</>))
import           Path.Extra (toFilePathNoTrailingSep)
import           Prelude
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.FlagName
import           Stack.Types.GhcPkgId
import           Stack.Types.Package
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version
import           System.Exit (ExitCode (ExitFailure))
import           System.FilePath (pathSeparator)
import           System.Process.Log (showProcessArgDebug)

----------------------------------------------
-- Exceptions
data StackBuildException
  = Couldn'tFindPkgId PackageName
  | CompilerVersionMismatch
        (Maybe (CompilerVersion, Arch))
        (CompilerVersion, Arch)
        GHCVariant
        VersionCheck
        (Maybe (Path Abs File))
        Text -- recommended resolution
  -- ^ Path to the stack.yaml file
  | Couldn'tParseTargets [Text]
  | UnknownTargets
    (Set PackageName) -- no known version
    (Map PackageName Version) -- not in snapshot, here's the most recent version in the index
    (Path Abs File) -- stack.yaml
  | TestSuiteFailure PackageIdentifier (Map Text (Maybe ExitCode)) (Maybe (Path Abs File)) S.ByteString
  | TestSuiteTypeUnsupported TestSuiteInterface
  | ConstructPlanExceptions
        [ConstructPlanException]
        (Path Abs File) -- stack.yaml
  | CabalExitedUnsuccessfully
        ExitCode
        PackageIdentifier
        (Path Abs File)  -- cabal Executable
        [String]         -- cabal arguments
        (Maybe (Path Abs File)) -- logfiles location
        [Text]     -- log contents
  | ExecutionFailure [SomeException]
  | LocalPackageDoesn'tMatchTarget
        PackageName
        Version -- local version
        Version -- version specified on command line
  | NoSetupHsFound (Path Abs Dir)
  | InvalidFlagSpecification (Set UnusedFlags)
  | TargetParseException [Text]
  | DuplicateLocalPackageNames [(PackageName, [Path Abs Dir])]
  | SolverGiveUp String
  | SolverMissingCabalInstall
  | SomeTargetsNotBuildable [(PackageName, NamedComponent)]
  | TestSuiteExeMissing Bool String String String
  | CabalCopyFailed Bool String
  deriving Typeable

data FlagSource = FSCommandLine | FSStackYaml
    deriving (Show, Eq, Ord)

data UnusedFlags = UFNoPackage FlagSource PackageName
                 | UFFlagsNotDefined FlagSource Package (Set FlagName)
                 | UFSnapshot PackageName
    deriving (Show, Eq, Ord)

instance Show StackBuildException where
    show (Couldn'tFindPkgId name) =
              ("After installing " <> packageNameString name <>
               ", the package id couldn't be found " <> "(via ghc-pkg describe " <>
               packageNameString name <> "). This shouldn't happen, " <>
               "please report as a bug")
    show (CompilerVersionMismatch mactual (expected, earch) ghcVariant check mstack resolution) = concat
                [ case mactual of
                    Nothing -> "No compiler found, expected "
                    Just (actual, arch) -> concat
                        [ "Compiler version mismatched, found "
                        , compilerVersionString actual
                        , " ("
                        , display arch
                        , ")"
                        , ", but expected "
                        ]
                , case check of
                    MatchMinor -> "minor version match with "
                    MatchExact -> "exact version "
                    NewerMinor -> "minor version match or newer with "
                , compilerVersionString expected
                , " ("
                , display earch
                , ghcVariantSuffix ghcVariant
                , ") (based on "
                , case mstack of
                    Nothing -> "command line arguments"
                    Just stack -> "resolver setting in " ++ toFilePath stack
                , ").\n"
                , T.unpack resolution
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
    show (TestSuiteTypeUnsupported interface) =
              ("Unsupported test suite type: " <> show interface)
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
    show (CabalExitedUnsuccessfully exitCode taskProvides' execName fullArgs logFiles bss) =
        let fullCmd = unwords
                    $ dropQuotes (toFilePath execName)
                    : map (T.unpack . showProcessArgDebug) fullArgs
            logLocations = maybe "" (\fp -> "\n    Logs have been written to: " ++ toFilePath fp) logFiles
        in "\n--  While building package " ++ dropQuotes (show taskProvides') ++ " using:\n" ++
           "      " ++ fullCmd ++ "\n" ++
           "    Process exited with code: " ++ show exitCode ++
           (if exitCode == ExitFailure (-9)
                then " (THIS MAY INDICATE OUT OF MEMORY)"
                else "") ++
           logLocations ++
           (if null bss
                then ""
                else "\n\n" ++ doubleIndent (map T.unpack bss))
         where
          doubleIndent = dropWhileEnd isSpace . unlines . fmap (\line -> "    " ++ line)
          dropQuotes = filter ('\"' /=)
    show (ExecutionFailure es) = intercalate "\n\n" $ map show es
    show (LocalPackageDoesn'tMatchTarget name localV requestedV) = concat
        [ "Version for local package "
        , packageNameString name
        , " is "
        , versionString localV
        , ", but you asked for "
        , versionString requestedV
        , " on the command line"
        ]
    show (NoSetupHsFound dir) =
        "No Setup.hs or Setup.lhs file found in " ++ toFilePath dir
    show (InvalidFlagSpecification unused) = unlines
        $ "Invalid flag specification:"
        : map go (Set.toList unused)
      where
        showFlagSrc :: FlagSource -> String
        showFlagSrc FSCommandLine = " (specified on command line)"
        showFlagSrc FSStackYaml = " (specified in stack.yaml)"

        go :: UnusedFlags -> String
        go (UFNoPackage src name) = concat
            [ "- Package '"
            , packageNameString name
            , "' not found"
            , showFlagSrc src
            ]
        go (UFFlagsNotDefined src pkg flags) = concat
            [ "- Package '"
            , name
            , "' does not define the following flags"
            , showFlagSrc src
            , ":\n"
            , intercalate "\n"
                          (map (\flag -> "  " ++ flagNameString flag)
                               (Set.toList flags))
            , "\n- Flags defined by package '" ++ name ++ "':\n"
            , intercalate "\n"
                          (map (\flag -> "  " ++ name ++ ":" ++ flagNameString flag)
                               (Set.toList pkgFlags))
            ]
          where name = packageNameString (packageName pkg)
                pkgFlags = packageDefinedFlags pkg
        go (UFSnapshot name) = concat
            [ "- Attempted to set flag on snapshot package "
            , packageNameString name
            , ", please add to extra-deps"
            ]
    show (TargetParseException [err]) = "Error parsing targets: " ++ T.unpack err
    show (TargetParseException errs) = unlines
        $ "The following errors occurred while parsing the build targets:"
        : map (("- " ++) . T.unpack) errs

    show (DuplicateLocalPackageNames pairs) = concat
        $ "The same package name is used in multiple local packages\n"
        : map go pairs
      where
        go (name, dirs) = unlines
            $ ""
            : (packageNameString name ++ " used in:")
            : map goDir dirs
        goDir dir = "- " ++ toFilePath dir
    show (SolverGiveUp msg) = concat
        [ "\nSolver could not resolve package dependencies.\n"
        , "You can try the following:\n"
        , msg
        ]
    show SolverMissingCabalInstall = unlines
        [ "Solver requires that cabal be on your PATH"
        , "Try running 'stack install cabal-install'"
        ]
    show (SomeTargetsNotBuildable xs) =
        "The following components have 'buildable: False' set in the cabal configuration, and so cannot be targets:\n    " ++
        T.unpack (renderPkgComponents xs) ++
        "\nTo resolve this, either provide flags such that these components are buildable, or only specify buildable targets."
    show (TestSuiteExeMissing isSimpleBuildType exeName pkgName testName) =
        missingExeError isSimpleBuildType $ concat
            [ "Test suite executable \""
            , exeName
            , " not found for "
            , pkgName
            , ":test:"
            , testName
            ]
    show (CabalCopyFailed isSimpleBuildType innerMsg) =
        missingExeError isSimpleBuildType $ concat
            [ "'cabal copy' failed.  Error message:\n"
            , innerMsg
            , "\n"
            ]

missingExeError :: Bool -> String -> String
missingExeError isSimpleBuildType msg =
    unlines $ msg :
        case possibleCauses of
            [] -> []
            [cause] -> ["One possible cause of this issue is:\n* " <> cause]
            _ -> "Possible causes of this issue:" : map ("* " <>) possibleCauses
  where
    possibleCauses =
        "No module named \"Main\". The 'main-is' source file should usually have a header indicating that it's a 'Main' module." :
        if isSimpleBuildType
            then []
            else ["The Setup.hs file is changing the installation target dir."]

instance Exception StackBuildException

data ConstructPlanException
    = DependencyCycleDetected [PackageName]
    | DependencyPlanFailures Package (Map PackageName (VersionRange, LatestApplicableVersion, BadDependency))
    | UnknownPackage PackageName -- TODO perhaps this constructor will be removed, and BadDependency will handle it all
    -- ^ Recommend adding to extra-deps, give a helpful version number?
    deriving (Typeable, Eq)

-- | For display purposes only, Nothing if package not found
type LatestApplicableVersion = Maybe Version

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
         (DependencyPlanFailures pkg (Map.toList -> pDeps)) ->
           "Failure when adding dependencies:" ++ doubleIndent (appendDeps pDeps) ++ "\n" ++
           "  needed for package " ++ packageIdentifierString (packageIdentifier pkg) ++
           appendFlags (packageFlags pkg)
         (UnknownPackage pName) ->
             "While attempting to add dependency,\n" ++
             "  Could not find package " ++ show pName  ++ " in known packages"
    in indent details
     where
      appendLines = foldr (\pName-> (++) ("\n" ++ show pName)) ""
      indent = dropWhileEnd isSpace . unlines . fmap (\line -> "  " ++ line) . lines
      doubleIndent = indent . indent
      appendFlags flags =
          if Map.null flags
              then ""
              else " with flags:\n" ++
                  (doubleIndent . intercalate "\n" . map showFlag . Map.toList) flags
      showFlag (name, bool) = show name ++ ": " ++ show bool
      appendDeps = foldr (\dep-> (++) ("\n" ++ showDep dep)) ""
      showDep (name, (range, mlatestApplicable, badDep)) = concat
        [ show name
        , ": needed ("
        , display range
        , ")"
        , ", "
        , let latestApplicableStr =
                case mlatestApplicable of
                    Nothing -> ""
                    Just la -> " (latest applicable is " ++ versionString la ++ ")"
           in case badDep of
                NotInBuildPlan -> "stack configuration has no specified version" ++ latestApplicableStr
                Couldn'tResolveItsDependencies -> "couldn't resolve its dependencies"
                DependencyMismatch version ->
                    case mlatestApplicable of
                        Just la
                            | la == version ->
                                versionString version ++
                                " found (latest applicable version available)"
                        _ -> versionString version ++ " found" ++ latestApplicableStr
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


-- | Package dependency oracle.
newtype PkgDepsOracle =
    PkgDeps PackageName
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

-- | Stored on disk to know whether the flags have changed or any
-- files have changed.
data ConfigCache = ConfigCache
    { configCacheOpts :: !ConfigureOpts
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
instance Binary ConfigCache where
    put x = do
        -- magic string
        putWord8 1
        putWord8 3
        putWord8 4
        putWord8 8
        gput $ from x
    get = do
        1 <- getWord8
        3 <- getWord8
        4 <- getWord8
        8 <- getWord8
        fmap to gget
instance NFData ConfigCache
instance HasStructuralInfo ConfigCache
instance HasSemanticVersion ConfigCache

-- | A task to perform when building
data Task = Task
    { taskProvides        :: !PackageIdentifier
    -- ^ the package/version to be built
    , taskType            :: !TaskType
    -- ^ the task type, telling us how to build this
    , taskConfigOpts      :: !TaskConfigOpts
    , taskPresent         :: !(Map PackageIdentifier GhcPkgId)
    -- ^ GhcPkgIds of already-installed dependencies
    , taskAllInOne        :: !Bool
    -- ^ indicates that the package can be built in one step
    }
    deriving Show

-- | Given the IDs of any missing packages, produce the configure options
data TaskConfigOpts = TaskConfigOpts
    { tcoMissing :: !(Set PackageIdentifier)
      -- ^ Dependencies for which we don't yet have an GhcPkgId
    , tcoOpts    :: !(Map PackageIdentifier GhcPkgId -> ConfigureOpts)
      -- ^ Produce the list of options given the missing @GhcPkgId@s
    }
instance Show TaskConfigOpts where
    show (TaskConfigOpts missing f) = concat
        [ "Missing: "
        , show missing
        , ". Without those: "
        , show $ f Map.empty
        ]

-- | The type of a task, either building local code or something from the
-- package index (upstream)
data TaskType = TTLocal LocalPackage
              | TTUpstream Package InstallLocation
    deriving Show

taskLocation :: Task -> InstallLocation
taskLocation task =
    case taskType task of
        TTLocal _ -> Local
        TTUpstream _ loc -> loc

-- | A complete plan of what needs to be built and how to do it
data Plan = Plan
    { planTasks :: !(Map PackageName Task)
    , planFinals :: !(Map PackageName Task)
    -- ^ Final actions to be taken (test, benchmark, etc)
    , planUnregisterLocal :: !(Map GhcPkgId (PackageIdentifier, Maybe Text))
    -- ^ Text is reason we're unregistering, for display only
    , planInstallExes :: !(Map Text InstallLocation)
    -- ^ Executables that should be installed after successful building
    }
    deriving Show

-- | Basic information used to calculate what the configure options are
data BaseConfigOpts = BaseConfigOpts
    { bcoSnapDB :: !(Path Abs Dir)
    , bcoLocalDB :: !(Path Abs Dir)
    , bcoSnapInstallRoot :: !(Path Abs Dir)
    , bcoLocalInstallRoot :: !(Path Abs Dir)
    , bcoBuildOpts :: !BuildOpts
    , bcoBuildOptsCLI :: !BuildOptsCLI
    , bcoExtraDBs :: ![(Path Abs Dir)]
    }
    deriving Show

-- | Render a @BaseConfigOpts@ to an actual list of options
configureOpts :: EnvConfig
              -> BaseConfigOpts
              -> Map PackageIdentifier GhcPkgId -- ^ dependencies
              -> Bool -- ^ wanted?
              -> Bool -- ^ local non-extra-dep?
              -> InstallLocation
              -> Package
              -> ConfigureOpts
configureOpts econfig bco deps wanted isLocal loc package = ConfigureOpts
    { coDirs = configureOptsDirs bco loc package
    , coNoDirs = configureOptsNoDir econfig bco deps wanted isLocal package
    }

-- options set by stack
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
    , "--enable-library-profiling"
    , "--enable-executable-profiling"
    , "--exact-configuration"
    ] || elem t
    [ "--user"
    ]

configureOptsDirs :: BaseConfigOpts
                  -> InstallLocation
                  -> Package
                  -> [String]
configureOptsDirs bco loc package = concat
    [ ["--user", "--package-db=clear", "--package-db=global"]
    , map (("--package-db=" ++) . toFilePathNoTrailingSep) $ case loc of
        Snap -> bcoExtraDBs bco ++ [bcoSnapDB bco]
        Local -> bcoExtraDBs bco ++ [bcoSnapDB bco] ++ [bcoLocalDB bco]
    , [ "--libdir=" ++ toFilePathNoTrailingSep (installRoot </> $(mkRelDir "lib"))
      , "--bindir=" ++ toFilePathNoTrailingSep (installRoot </> bindirSuffix)
      , "--datadir=" ++ toFilePathNoTrailingSep (installRoot </> $(mkRelDir "share"))
      , "--libexecdir=" ++ toFilePathNoTrailingSep (installRoot </> $(mkRelDir "libexec"))
      , "--sysconfdir=" ++ toFilePathNoTrailingSep (installRoot </> $(mkRelDir "etc"))
      , "--docdir=" ++ toFilePathNoTrailingSep docDir
      , "--htmldir=" ++ toFilePathNoTrailingSep docDir
      , "--haddockdir=" ++ toFilePathNoTrailingSep docDir]
    ]
  where
    installRoot =
        case loc of
            Snap -> bcoSnapInstallRoot bco
            Local -> bcoLocalInstallRoot bco
    docDir =
        case pkgVerDir of
            Nothing -> installRoot </> docDirSuffix
            Just dir -> installRoot </> docDirSuffix </> dir
    pkgVerDir =
        parseRelDir (packageIdentifierString (PackageIdentifier (packageName package)
                                                                (packageVersion package)) ++
                     [pathSeparator])

-- | Same as 'configureOpts', but does not include directory path options
configureOptsNoDir :: EnvConfig
                   -> BaseConfigOpts
                   -> Map PackageIdentifier GhcPkgId -- ^ dependencies
                   -> Bool -- ^ wanted?
                   -> Bool -- ^ is this a local, non-extra-dep?
                   -> Package
                   -> [String]
configureOptsNoDir econfig bco deps wanted isLocal package = concat
    [ depOptions
    , ["--enable-library-profiling" | boptsLibProfile bopts || boptsExeProfile bopts]
    , ["--enable-executable-profiling" | boptsExeProfile bopts && isLocal]
    , ["--enable-split-objs" | boptsSplitObjs bopts]
    , map (\(name,enabled) ->
                       "-f" <>
                       (if enabled
                           then ""
                           else "-") <>
                       flagNameString name)
                    (Map.toList flags)
    , concatMap (\x -> ["--ghc-options", T.unpack x]) allGhcOptions
    , map (("--extra-include-dirs=" ++) . T.unpack) (Set.toList (configExtraIncludeDirs config))
    , map (("--extra-lib-dirs=" ++) . T.unpack) (Set.toList (configExtraLibDirs config))
    , if whichCompiler (envConfigCompilerVersion econfig) == Ghcjs
        then ["--ghcjs"]
        else []
    , if useExactConf then ["--exact-configuration"] else []
    ]
  where
    config = getConfig econfig
    bopts = bcoBuildOpts bco
    boptsCli = bcoBuildOptsCLI bco

    -- TODO: instead always enable this when the cabal version is new
    -- enough. That way we'll detect bugs with --exact-configuration
    -- earlier. Cabal also might do less work then.
    useExactConf = configAllowNewer config

    newerCabal = envConfigCabalVersion econfig >= $(mkVersion "1.22")

    -- Unioning atop defaults is needed so that all flags are specified
    -- with --exact-configuration.
    flags | useExactConf = packageFlags package `Map.union` packageDefaultFlags package
          | otherwise = packageFlags package

    depOptions = map (uncurry toDepOption) $ Map.toList deps
      where
        toDepOption = if newerCabal then toDepOption1_22 else toDepOption1_18

    toDepOption1_22 ident gid = concat
        [ "--dependency="
        , packageNameString $ packageIdentifierName ident
        , "="
        , ghcPkgIdString gid
        ]

    toDepOption1_18 ident _gid = concat
        [ "--constraint="
        , packageNameString name
        , "=="
        , versionString version
        ]
      where
        PackageIdentifier name version = ident

    allGhcOptions = concat
        [ Map.findWithDefault [] Nothing (configGhcOptions config)
        , Map.findWithDefault [] (Just $ packageName package) (configGhcOptions config)
        , concat [["-fhpc"] | isLocal && toCoverage (boptsTestOpts bopts)]
        , if (boptsLibProfile bopts || boptsExeProfile bopts)
             then ["-auto-all","-caf-all"]
             else []
        , if includeExtraOptions
            then boptsCLIGhcOptions boptsCli
            else []
        ]

    includeExtraOptions =
        case configApplyGhcOptions config of
            AGOTargets -> wanted
            AGOLocals -> isLocal
            AGOEverything -> True

-- | Get set of wanted package names from locals.
wantedLocalPackages :: [LocalPackage] -> Set PackageName
wantedLocalPackages = Set.fromList . map (packageName . lpPackage) . filter lpWanted

-- | One-way conversion to serialized time.
modTime :: UTCTime -> ModTime
modTime x =
    ModTime
        ( toModifiedJulianDay
              (utctDay x)
        , toRational
              (utctDayTime x))

-- | Configure options to be sent to Setup.hs configure
data ConfigureOpts = ConfigureOpts
    { coDirs :: ![String]
    -- ^ Options related to various paths. We separate these out since they do
    -- not have an impact on the contents of the compiled binary for checking
    -- if we can use an existing precompiled cache.
    , coNoDirs :: ![String]
    }
    deriving (Show, Eq, Generic)
instance Binary ConfigureOpts
instance HasStructuralInfo ConfigureOpts
instance NFData ConfigureOpts

-- | Information on a compiled package: the library conf file (if relevant),
-- and all of the executable paths.
data PrecompiledCache = PrecompiledCache
    -- Use FilePath instead of Path Abs File for Binary instances
    { pcLibrary :: !(Maybe FilePath)
    -- ^ .conf file inside the package database
    , pcExes    :: ![FilePath]
    -- ^ Full paths to executables
    }
    deriving (Show, Eq, Generic)
instance Binary PrecompiledCache
instance HasSemanticVersion PrecompiledCache
instance HasStructuralInfo PrecompiledCache
instance NFData PrecompiledCache
