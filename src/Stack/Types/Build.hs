{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Build-specific types.

module Stack.Types.Build
    (StackBuildException(..)
    ,FlagSource(..)
    ,UnusedFlags(..)
    ,InstallLocation(..)
    ,Installed(..)
    ,psVersion
    ,Task(..)
    ,taskIsTarget
    ,taskLocation
    ,taskTargetIsMutable
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
    ,IsMutable(..)
    ,installLocationIsMutable
    ,TaskConfigOpts(..)
    ,BuildCache(..)
    ,ConfigCache(..)
    ,configureOpts
    ,CachePkgSrc (..)
    ,toCachePkgSrc
    ,isStackOpt
    ,wantedLocalPackages
    ,FileCacheInfo (..)
    ,ConfigureOpts (..)
    ,PrecompiledCache (..)
    )
    where

import           Stack.Prelude
import           Data.Aeson                      (ToJSON, FromJSON)
import qualified Data.ByteString                 as S
import           Data.Char                       (isSpace)
import           Data.List.Extra
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import           Database.Persist.Sql            (PersistField(..)
                                                 ,PersistFieldSql(..)
                                                 ,PersistValue(PersistText)
                                                 ,SqlType(SqlString))
import           Distribution.PackageDescription (TestSuiteInterface)
import           Distribution.System             (Arch)
import qualified Distribution.Text               as C
import           Distribution.Version            (mkVersion)
import           Path                            (parseRelDir, (</>), parent)
import           Path.Extra                      (toFilePathNoTrailingSep)
import           Stack.Constants
import           Stack.Types.Compiler
import           Stack.Types.CompilerBuild
import           Stack.Types.Config
import           Stack.Types.GhcPkgId
import           Stack.Types.NamedComponent
import           Stack.Types.Package
import           Stack.Types.Version
import           System.FilePath                 (pathSeparator)
import           RIO.Process                     (showProcessArgDebug)

----------------------------------------------
-- Exceptions
data StackBuildException
  = Couldn'tFindPkgId PackageName
  | CompilerVersionMismatch
        (Maybe (ActualCompiler, Arch)) -- found
        (WantedCompiler, Arch) -- expected
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
  | InvalidGhcOptionsSpecification [PackageName]
  | TargetParseException [Text]
  | SomeTargetsNotBuildable [(PackageName, NamedComponent)]
  | TestSuiteExeMissing Bool String String String
  | CabalCopyFailed Bool String
  | LocalPackagesPresent [PackageIdentifier]
  | CouldNotLockDistDir !(Path Abs File)
  deriving Typeable

data FlagSource = FSCommandLine | FSStackYaml
    deriving (Show, Eq, Ord)

data UnusedFlags = UFNoPackage FlagSource PackageName
                 | UFFlagsNotDefined
                       FlagSource
                       PackageName
                       (Set FlagName) -- defined in package
                       (Set FlagName) -- not defined
                 | UFSnapshot PackageName
    deriving (Show, Eq, Ord)

instance Show StackBuildException where
    show (Couldn'tFindPkgId name) =
              "After installing " <> packageNameString name <>
               ", the package id couldn't be found " <> "(via ghc-pkg describe " <>
               packageNameString name <> "). This shouldn't happen, " <>
               "please report as a bug"
    show (CompilerVersionMismatch mactual (expected, eArch) ghcVariant ghcBuild check mstack resolution) = concat
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
                , T.unpack $ utf8BuilderToText $ display expected
                , " ("
                , C.display eArch
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
            | otherwise = pure $
                "The following target packages were not found: " ++
                intercalate ", " (map packageNameString $ Set.toList noKnown) ++
                "\nSee https://docs.haskellstack.org/en/stable/build_command/#target-syntax for details."
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
        , pure $ case mlogFile of
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
     -- Suppressing duplicate output
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
        go (UFFlagsNotDefined src pname pkgFlags flags) = concat
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
          where name = packageNameString pname
        go (UFSnapshot name) = concat
            [ "- Attempted to set flag on snapshot package "
            , packageNameString name
            , ", please add to extra-deps"
            ]
    show (InvalidGhcOptionsSpecification unused) = unlines
        $ "Invalid GHC options specification:"
        : map showGhcOptionSrc unused
      where
        showGhcOptionSrc name = concat
            [ "- Package '"
            , packageNameString name
            , "' not found"
            ]
    show (TargetParseException [err]) = "Error parsing targets: " ++ T.unpack err
    show (TargetParseException errs) = unlines
        $ "The following errors occurred while parsing the build targets:"
        : map (("- " ++) . T.unpack) errs

    show (SomeTargetsNotBuildable xs) =
        "The following components have 'buildable: False' set in the cabal configuration, and so cannot be targets:\n    " ++
        T.unpack (renderPkgComponents xs) ++
        "\nTo resolve this, either provide flags such that these components are buildable, or only specify buildable targets."
    show (TestSuiteExeMissing isSimpleBuildType exeName pkgName' testName) =
        missingExeError isSimpleBuildType $ concat
            [ "Test suite executable \""
            , exeName
            , " not found for "
            , pkgName'
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
    show (CouldNotLockDistDir lockFile) = unlines
      [ "Locking the dist directory failed, try to lock file:"
      , "  " ++ toFilePath lockFile
      , "Maybe you're running another copy of Stack?"
      ]

missingExeError :: Bool -> String -> String
missingExeError isSimpleBuildType msg =
    unlines $ msg : "Possible causes of this issue:" :
              map ("* " <>) possibleCauses
  where
    possibleCauses =
        "No module named \"Main\". The 'main-is' source file should usually \
        \have a header indicating that it's a 'Main' module." :

        "A cabal file that refers to nonexistent other files (e.g. a \
        \license-file that doesn't exist). Running 'cabal check' may point \
        \out these issues." :

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
       (False, Just taskProvides') -> "package " ++ dropQuotes (packageIdentifierString taskProvides')
       (True, Nothing) -> "simple Setup.hs"
       (True, Just taskProvides') -> "custom Setup.hs for package " ++ dropQuotes (packageIdentifierString taskProvides')
     ) ++
     " (scroll up to its section to see the error) using:\n      " ++ fullCmd ++ "\n" ++
     "    Process exited with code: " ++ show exitCode ++
     (if exitCode == ExitFailure (-9)
          then " (THIS MAY INDICATE OUT OF MEMORY)"
          else "") ++
     logLocations ++
     (if null bss
          then ""
          else "\n\n" ++ removeTrailingSpaces (map T.unpack bss))
   where
    removeTrailingSpaces = dropWhileEnd isSpace . unlines
    dropQuotes = filter ('\"' /=)

instance Exception StackBuildException

----------------------------------------------

-- | Package dependency oracle.
newtype PkgDepsOracle =
    PkgDeps PackageName
    deriving (Show,Typeable,Eq,NFData)

-- | Stored on disk to know whether the files have changed.
newtype BuildCache = BuildCache
    { buildCacheTimes :: Map FilePath FileCacheInfo
      -- ^ Modification times of files.
    }
    deriving (Generic, Eq, Show, Typeable, ToJSON, FromJSON)
instance NFData BuildCache

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
    , configCachePathEnvVar :: !Text
    -- ^ Value of the PATH env var, see <https://github.com/commercialhaskell/stack/issues/3138>
    }
    deriving (Generic, Eq, Show, Data, Typeable)
instance NFData ConfigCache

data CachePkgSrc = CacheSrcUpstream | CacheSrcLocal FilePath
    deriving (Generic, Eq, Read, Show, Data, Typeable)
instance NFData CachePkgSrc

instance PersistField CachePkgSrc where
    toPersistValue CacheSrcUpstream = PersistText "upstream"
    toPersistValue (CacheSrcLocal fp) = PersistText ("local:" <> T.pack fp)
    fromPersistValue (PersistText t) = do
        if t == "upstream"
            then Right CacheSrcUpstream
            else case T.stripPrefix "local:" t of
                Just fp -> Right $ CacheSrcLocal (T.unpack fp)
                Nothing -> Left $ "Unexpected CachePkgSrc value: " <> t
    fromPersistValue _ = Left "Unexpected CachePkgSrc type"

instance PersistFieldSql CachePkgSrc where
    sqlType _ = SqlString

toCachePkgSrc :: PackageSource -> CachePkgSrc
toCachePkgSrc (PSFilePath lp) = CacheSrcLocal (toFilePath (parent (lpCabalFile lp)))
toCachePkgSrc PSRemote{} = CacheSrcUpstream

-- | A task to perform when building
data Task = Task
    { taskProvides        :: !PackageIdentifier -- FIXME turn this into a function on taskType?
    -- ^ the package/version to be built
    , taskType            :: !TaskType
    -- ^ the task type, telling us how to build this
    , taskConfigOpts      :: !TaskConfigOpts
    , taskBuildHaddock    :: !Bool
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
data TaskType
  = TTLocalMutable LocalPackage
  | TTRemotePackage IsMutable Package PackageLocationImmutable
    deriving Show

data IsMutable
    = Mutable
    | Immutable
    deriving (Eq, Show)

instance Semigroup IsMutable where
    Mutable <> _ = Mutable
    _ <> Mutable = Mutable
    Immutable <> Immutable = Immutable

instance Monoid IsMutable where
    mempty = Immutable
    mappend = (<>)

taskIsTarget :: Task -> Bool
taskIsTarget t =
    case taskType t of
        TTLocalMutable lp -> lpWanted lp
        _ -> False

taskLocation :: Task -> InstallLocation
taskLocation task =
    case taskType task of
        TTLocalMutable _ -> Local
        TTRemotePackage Mutable _ _ -> Local
        TTRemotePackage Immutable _ _ -> Snap

taskTargetIsMutable :: Task -> IsMutable
taskTargetIsMutable task =
    case taskType task of
        TTLocalMutable _ -> Mutable
        TTRemotePackage mutable _ _ -> mutable

installLocationIsMutable :: InstallLocation -> IsMutable
installLocationIsMutable Snap = Immutable
installLocationIsMutable Local = Mutable

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
              -> IsMutable
              -> Package
              -> ConfigureOpts
configureOpts econfig bco deps isLocal isMutable package = ConfigureOpts
    { coDirs = configureOptsDirs bco isMutable package
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
                  -> IsMutable
                  -> Package
                  -> [String]
configureOptsDirs bco isMutable package = concat
    [ ["--user", "--package-db=clear", "--package-db=global"]
    , map (("--package-db=" ++) . toFilePathNoTrailingSep) $ case isMutable of
        Immutable -> bcoExtraDBs bco ++ [bcoSnapDB bco]
        Mutable -> bcoExtraDBs bco ++ [bcoSnapDB bco] ++ [bcoLocalDB bco]
    , [ "--libdir=" ++ toFilePathNoTrailingSep (installRoot </> relDirLib)
      , "--bindir=" ++ toFilePathNoTrailingSep (installRoot </> bindirSuffix)
      , "--datadir=" ++ toFilePathNoTrailingSep (installRoot </> relDirShare)
      , "--libexecdir=" ++ toFilePathNoTrailingSep (installRoot </> relDirLibexec)
      , "--sysconfdir=" ++ toFilePathNoTrailingSep (installRoot </> relDirEtc)
      , "--docdir=" ++ toFilePathNoTrailingSep docDir
      , "--htmldir=" ++ toFilePathNoTrailingSep docDir
      , "--haddockdir=" ++ toFilePathNoTrailingSep docDir]
    ]
  where
    installRoot =
        case isMutable of
            Immutable -> bcoSnapInstallRoot bco
            Mutable -> bcoLocalInstallRoot bco
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
    , map T.unpack $ packageCabalConfigOpts package
    , processGhcOptions (packageGhcOptions package)
    , map ("--extra-include-dirs=" ++) (configExtraIncludeDirs config)
    , map ("--extra-lib-dirs=" ++) (configExtraLibDirs config)
    , maybe [] (\customGcc -> ["--with-gcc=" ++ toFilePath customGcc]) (configOverrideGccPath config)
    , ["--exact-configuration"]
    , ["--ghc-option=-fhide-source-paths" | hideSourcePaths cv]
    ]
  where
    -- This function parses the GHC options that are providing in the
    -- stack.yaml file. In order to handle RTS arguments correctly, we need
    -- to provide the RTS arguments as a single argument.
    processGhcOptions :: [Text] -> [String]
    processGhcOptions args =
        let
            (preRtsArgs, mid) =
                break ("+RTS" ==) args
            (rtsArgs, end) =
                break ("-RTS" ==) mid
            fullRtsArgs =
                case rtsArgs of
                    [] ->
                        -- This means that we didn't have any RTS args - no
                        -- `+RTS` - and therefore no need for a `-RTS`.
                        []
                    _ ->
                        -- In this case, we have some RTS args. `break`
                        -- puts the `"-RTS"` string in the `snd` list, so
                        -- we want to append it on the end of `rtsArgs`
                        -- here.
                        --
                        -- We're not checking that `-RTS` is the first
                        -- element of `end`. This is because the GHC RTS
                        -- allows you to omit a trailing -RTS if that's the
                        -- last of the arguments. This permits a GHC
                        -- options in stack.yaml that matches what you
                        -- might pass directly to GHC.
                        [T.unwords $ rtsArgs ++ ["-RTS"]]
            -- We drop the first element from `end`, because it is always
            -- either `"-RTS"` (and we don't want that as a separate
            -- argument) or the list is empty (and `drop _ [] = []`).
            postRtsArgs =
                drop 1 end
            newArgs =
                concat [preRtsArgs, fullRtsArgs, postRtsArgs]
        in
            concatMap (\x -> [compilerOptionsCabalFlag wc, T.unpack x]) newArgs

    wc = view (actualCompilerVersionL.to whichCompiler) econfig
    cv = view (actualCompilerVersionL.to getGhcVersion) econfig

    hideSourcePaths ghcVersion = ghcVersion >= mkVersion [8, 2] && configHideSourcePaths config

    config = view configL econfig
    bopts = bcoBuildOpts bco

    newerCabal = view cabalVersionL econfig >= mkVersion [1, 22]

    -- Unioning atop defaults is needed so that all flags are specified
    -- with --exact-configuration.
    flags = packageFlags package `Map.union` packageDefaultFlags package

    depOptions = map (uncurry toDepOption) $ Map.toList deps
      where
        toDepOption = if newerCabal then toDepOption1_22 else toDepOption1_18

    toDepOption1_22 (PackageIdentifier name _) gid = concat
        [ "--dependency="
        , packageNameString name
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

-- | Configure options to be sent to Setup.hs configure
data ConfigureOpts = ConfigureOpts
    { coDirs :: ![String]
    -- ^ Options related to various paths. We separate these out since they do
    -- not have an impact on the contents of the compiled binary for checking
    -- if we can use an existing precompiled cache.
    , coNoDirs :: ![String]
    }
    deriving (Show, Eq, Generic, Data, Typeable)
instance NFData ConfigureOpts

-- | Information on a compiled package: the library conf file (if relevant),
-- the sublibraries (if present) and all of the executable paths.
data PrecompiledCache base = PrecompiledCache
    { pcLibrary :: !(Maybe (Path base File))
    -- ^ .conf file inside the package database
    , pcSubLibs :: ![Path base File]
    -- ^ .conf file inside the package database, for each of the sublibraries
    , pcExes    :: ![Path base File]
    -- ^ Full paths to executables
    }
    deriving (Show, Eq, Generic, Typeable)
instance NFData (PrecompiledCache Abs)
instance NFData (PrecompiledCache Rel)
