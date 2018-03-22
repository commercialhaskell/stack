{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | Build-specific types.

module Stack.Types.Build
    (StackBuildException(..)
    ,FlagSource(..)
    ,UnusedFlags(..)
    ,InstallLocation(..)
    ,ModTime
    ,modTime
    ,Installed(..)
    ,piiVersion
    ,piiLocation
    ,Task(..)
    ,AllImmutable(..)
    ,taskIsTarget
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
    ,ttPackageLocation
    ,TaskConfigOpts(..)
    ,BuildCache(..)
    ,buildCacheVC
    ,ConfigCache(..)
    ,configCacheVC
    ,configureOpts
    ,CachePkgSrc (..)
    ,toCachePkgSrc
    ,isStackOpt
    ,wantedLocalPackages
    ,FileCacheInfo (..)
    ,ConfigureOpts (..)
    ,PrecompiledCache (..)
    ,precompiledCacheVC)
    where

import           Stack.Prelude
import qualified Data.ByteString                 as S
import           Data.Char                       (isSpace)
import           Data.List.Extra
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import           Data.Store.Version
import           Data.Store.VersionTagged
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8With)
import           Data.Text.Encoding.Error        (lenientDecode)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Distribution.PackageDescription (TestSuiteInterface)
import           Distribution.System             (Arch)
import qualified Distribution.Text               as C
import           Path                            (mkRelDir, parseRelDir, (</>))
import           Path.Extra                      (toFilePathNoTrailingSep)
import           Stack.Constants
import           Stack.Types.BuildPlan
import           Stack.Types.Compiler
import           Stack.Types.CompilerBuild
import           Stack.Types.Config
import           Stack.Types.FlagName
import           Stack.Types.GhcPkgId
import           Stack.Types.NamedComponent
import           Stack.Types.Package
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version
import           System.Exit                     (ExitCode (ExitFailure))
import           System.FilePath                 (pathSeparator)
import           RIO.Process                     (showProcessArgDebug)

----------------------------------------------
-- Exceptions
data StackBuildException
  = Couldn'tFindPkgId PackageName
  | CompilerVersionMismatch
        (Maybe (CompilerVersion 'CVActual, Arch)) -- found
        (CompilerVersion 'CVWanted, Arch) -- expected
        GHCVariant -- expected
        CompilerBuild -- expected
        VersionCheck
        (Maybe (Path Abs File)) -- Path to the stack.yaml file
        Text -- recommended resolution
  | Couldn'tParseTargets [Text]
  | UnknownTargets
    (Set PackageName) -- no known version
    (Map PackageName Version) -- not in snapshot, here's the most recent version in the index
    (Path Abs File) -- stack.yaml
  | TestSuiteFailure PackageIdentifier (Map Text (Maybe ExitCode)) (Maybe (Path Abs File)) S.ByteString
  | TestSuiteTypeUnsupported TestSuiteInterface
  | ConstructPlanFailed String
  | CabalExitedUnsuccessfully
        ExitCode
        PackageIdentifier
        (Path Abs File)  -- cabal Executable
        [String]         -- cabal arguments
        (Maybe (Path Abs File)) -- logfiles location
        [Text]     -- log contents
  | SetupHsBuildFailure
        ExitCode
        (Maybe PackageIdentifier) -- which package's custom setup, is simple setup if Nothing
        (Path Abs File)  -- ghc Executable
        [String]         -- ghc arguments
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
  | SolverGiveUp String
  | SolverMissingCabalInstall
  | SomeTargetsNotBuildable [(PackageName, NamedComponent)]
  | TestSuiteExeMissing Bool String String String
  | CabalCopyFailed Bool String
  | LocalPackagesPresent [PackageIdentifier]
  deriving Typeable

data FlagSource = FSCommandLine | FSStackYaml
    deriving (Show, Eq, Ord)

data UnusedFlags = UFNoPackage FlagSource PackageName
                 | UFFlagsNotDefined FlagSource Package (Set FlagName)
                 | UFSnapshot PackageName
    deriving (Show, Eq, Ord)

instance Show StackBuildException where
    show (Couldn'tFindPkgId name) =
              "After installing " <> packageNameString name <>
               ", the package id couldn't be found " <> "(via ghc-pkg describe " <>
               packageNameString name <> "). This shouldn't happen, " <>
               "please report as a bug"
    show (CompilerVersionMismatch mactual (expected, earch) ghcVariant ghcBuild check mstack resolution) = concat
                [ case mactual of
                    Nothing -> "No compiler found, expected "
                    Just (actual, arch) -> concat
                        [ "Compiler version mismatched, found "
                        , compilerVersionString actual
                        , " ("
                        , C.display arch
                        , ")"
                        , ", but expected "
                        ]
                , case check of
                    MatchMinor -> "minor version match with "
                    MatchExact -> "exact version "
                    NewerMinor -> "minor version match or newer with "
                , compilerVersionString expected
                , " ("
                , C.display earch
                , ghcVariantSuffix ghcVariant
                , compilerBuildSuffix ghcBuild
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
                intercalate ", " (map packageNameString $ Set.toList noKnown) ++
                "\nSee https://docs.haskellstack.org/en/v"
                <> versionString stackMinorVersion <>
                "/build_command/#target-syntax for details."
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
                    (\(name, version') -> "- " ++ packageIdentifierString
                        (PackageIdentifier name version'))
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
              "Unsupported test suite type: " <> show interface
     -- Supressing duplicate output
    show (CabalExitedUnsuccessfully exitCode taskProvides' execName fullArgs logFiles bss) =
      showBuildError False exitCode (Just taskProvides') execName fullArgs logFiles bss
    show (SetupHsBuildFailure exitCode mtaskProvides execName fullArgs logFiles bss) =
      showBuildError True exitCode mtaskProvides execName fullArgs logFiles bss
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
    show (ConstructPlanFailed msg) = msg
    show (LocalPackagesPresent locals) = unlines
      $ "Local packages are not allowed when using the script command. Packages found:"
      : map (\ident -> "- " ++ packageIdentifierString ident) locals

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
        "A cabal file that refers to nonexistent other files (e.g. a license-file that doesn't exist). Running 'cabal check' may point out these issues." :
        if isSimpleBuildType
            then []
            else ["The Setup.hs file is changing the installation target dir."]

showBuildError
  :: Bool
  -> ExitCode
  -> Maybe PackageIdentifier
  -> Path Abs File
  -> [String]
  -> Maybe (Path Abs File)
  -> [Text]
  -> String
showBuildError isBuildingSetup exitCode mtaskProvides execName fullArgs logFiles bss =
  let fullCmd = unwords
              $ dropQuotes (toFilePath execName)
              : map (T.unpack . showProcessArgDebug) fullArgs
      logLocations = maybe "" (\fp -> "\n    Logs have been written to: " ++ toFilePath fp) logFiles
  in "\n--  While building " ++
     (case (isBuildingSetup, mtaskProvides) of
       (False, Nothing) -> error "Invariant violated: unexpected case in showBuildError"
       (False, Just taskProvides') -> "package " ++ dropQuotes (show taskProvides')
       (True, Nothing) -> "simple Setup.hs"
       (True, Just taskProvides') -> "custom Setup.hs for package " ++ dropQuotes (show taskProvides')
     ) ++
     " using:\n      " ++ fullCmd ++ "\n" ++
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

instance Exception StackBuildException

----------------------------------------------

-- | Package dependency oracle.
newtype PkgDepsOracle =
    PkgDeps PackageName
    deriving (Show,Typeable,Eq,Hashable,Store,NFData)

-- | Stored on disk to know whether the files have changed.
newtype BuildCache = BuildCache
    { buildCacheTimes :: Map FilePath FileCacheInfo
      -- ^ Modification times of files.
    }
    deriving (Generic, Eq, Show, Data, Typeable)
instance NFData BuildCache
instance Store BuildCache

buildCacheVC :: VersionConfig BuildCache
buildCacheVC = storeVersionConfig "build-v1" "KVUoviSWWAd7tiRRGeWAvd0UIN4="

-- | Stored on disk to know whether the flags have changed.
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
    , configCachePkgSrc :: !CachePkgSrc
    }
    deriving (Generic, Eq, Show, Data, Typeable)
instance Store ConfigCache
instance NFData ConfigCache

data CachePkgSrc = CacheSrcUpstream | CacheSrcLocal FilePath
    deriving (Generic, Eq, Show, Data, Typeable)
instance Store CachePkgSrc
instance NFData CachePkgSrc

toCachePkgSrc :: PackageSource -> CachePkgSrc
toCachePkgSrc (PSFiles lp _) = CacheSrcLocal (toFilePath (lpDir lp))
toCachePkgSrc PSIndex{} = CacheSrcUpstream

configCacheVC :: VersionConfig ConfigCache
configCacheVC = storeVersionConfig "config-v3" "z7N_NxX7Gbz41Gi9AGEa1zoLE-4="

-- | A task to perform when building
data Task = Task
    { taskProvides        :: !PackageIdentifier -- FIXME turn this into a function on taskType?
    -- ^ the package/version to be built
    , taskType            :: !TaskType
    -- ^ the task type, telling us how to build this
    , taskConfigOpts      :: !TaskConfigOpts
    , taskPresent         :: !(Map PackageIdentifier GhcPkgId)
    -- ^ GhcPkgIds of already-installed dependencies
    , taskAllInOne        :: !Bool
    -- ^ indicates that the package can be built in one step
    , taskCachePkgSrc     :: !CachePkgSrc
    , taskAnyMissing      :: !Bool
    -- ^ Were any of the dependencies missing? The reason this is
    -- necessary is... hairy. And as you may expect, a bug in
    -- Cabal. See:
    -- <https://github.com/haskell/cabal/issues/4728#issuecomment-337937673>. The
    -- problem is that Cabal may end up generating the same package ID
    -- for a dependency, even if the ABI has changed. As a result,
    -- without this field, Stack would think that a reconfigure is
    -- unnecessary, when in fact we _do_ need to reconfigure. The
    -- details here suck. We really need proper hashes for package
    -- identifiers.
    , taskBuildTypeConfig :: !Bool
    -- ^ Is the build type of this package Configure. Check out
    -- ensureConfigureScript in Stack.Build.Execute for the motivation
    , taskAllImmutable :: !AllImmutable
    -- ^ See 'AllImmutable'
    }
    deriving Show

-- | Is this package, and all of its dependencies, from an immutable
-- package location? See
-- <https://github.com/commercialhaskell/stack/issues/3860>
data AllImmutable = AllImmutable | NotAllImmutable
  deriving Show
instance Monoid AllImmutable where
  mempty = AllImmutable
  mappend AllImmutable AllImmutable = AllImmutable
  mappend _ _ = NotAllImmutable

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
data TaskType = TTFiles LocalPackage InstallLocation
              | TTIndex Package InstallLocation PackageIdentifierRevision -- FIXME major overhaul for PackageLocation?
    deriving Show

ttPackageLocation :: TaskType -> PackageLocationIndex FilePath
ttPackageLocation (TTFiles lp _) = PLOther (lpLocation lp)
ttPackageLocation (TTIndex _ _ pir) = PLIndex pir

taskIsTarget :: Task -> Bool
taskIsTarget t =
    case taskType t of
        TTFiles lp _ -> lpWanted lp
        _ -> False

taskLocation :: Task -> InstallLocation
taskLocation task =
    case taskType task of
        TTFiles _ loc -> loc
        TTIndex _ loc _ -> loc

-- | A complete plan of what needs to be built and how to do it
data Plan = Plan
    { planTasks :: !(Map PackageName Task)
    , planFinals :: !(Map PackageName Task)
    -- ^ Final actions to be taken (test, benchmark, etc)
    , planUnregisterLocal :: !(Map GhcPkgId (PackageIdentifier, Text))
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
    , bcoExtraDBs :: ![Path Abs Dir]
    }
    deriving Show

-- | Render a @BaseConfigOpts@ to an actual list of options
configureOpts :: EnvConfig
              -> BaseConfigOpts
              -> Map PackageIdentifier GhcPkgId -- ^ dependencies
              -> Bool -- ^ local non-extra-dep?
              -> InstallLocation
              -> Package
              -> ConfigureOpts
configureOpts econfig bco deps isLocal loc package = ConfigureOpts
    { coDirs = configureOptsDirs bco loc package
    , coNoDirs = configureOptsNoDir econfig bco deps isLocal package
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
    , "--exact-configuration"
    -- Treat these as causing dirtiness, to resolve
    -- https://github.com/commercialhaskell/stack/issues/2984
    --
    -- , "--enable-library-profiling"
    -- , "--enable-executable-profiling"
    -- , "--enable-profiling"
    ] || t == "--user"

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
                   -> Bool -- ^ is this a local, non-extra-dep?
                   -> Package
                   -> [String]
configureOptsNoDir econfig bco deps isLocal package = concat
    [ depOptions
    , ["--enable-library-profiling" | boptsLibProfile bopts || boptsExeProfile bopts]
    -- Cabal < 1.21.1 does not support --enable-profiling, use --enable-executable-profiling instead
    , let profFlag = "--enable-" <> concat ["executable-" | not newerCabal] <> "profiling"
      in [ profFlag | boptsExeProfile bopts && isLocal]
    , ["--enable-split-objs" | boptsSplitObjs bopts]
    , ["--disable-library-stripping" | not $ boptsLibStrip bopts || boptsExeStrip bopts]
    , ["--disable-executable-stripping" | not (boptsExeStrip bopts) && isLocal]
    , map (\(name,enabled) ->
                       "-f" <>
                       (if enabled
                           then ""
                           else "-") <>
                       flagNameString name)
                    (Map.toList flags)
    , concatMap (\x -> [compilerOptionsCabalFlag wc, T.unpack x]) (packageGhcOptions package)
    , map ("--extra-include-dirs=" ++) (Set.toList (configExtraIncludeDirs config))
    , map ("--extra-lib-dirs=" ++) (Set.toList (configExtraLibDirs config))
    , maybe [] (\customGcc -> ["--with-gcc=" ++ toFilePath customGcc]) (configOverrideGccPath config)
    , ["--ghcjs" | wc == Ghcjs]
    , ["--exact-configuration" | useExactConf]
    ]
  where
    wc = view (actualCompilerVersionL.to whichCompiler) econfig
    config = view configL econfig
    bopts = bcoBuildOpts bco

    -- TODO: instead always enable this when the cabal version is new
    -- enough. That way we'll detect bugs with --exact-configuration
    -- earlier. Cabal also might do less work then.
    useExactConf = configAllowNewer config

    newerCabal = view cabalVersionL econfig >= $(mkVersion "1.22")

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
        , versionString version'
        ]
      where
        PackageIdentifier name version' = ident

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
    deriving (Show, Eq, Generic, Data, Typeable)
instance Store ConfigureOpts
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
    deriving (Show, Eq, Generic, Data, Typeable)
instance Store PrecompiledCache
instance NFData PrecompiledCache

precompiledCacheVC :: VersionConfig PrecompiledCache
precompiledCacheVC = storeVersionConfig "precompiled-v1" "eMzSOwaHJMamA5iNKs1A025frlQ="
