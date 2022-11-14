{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Build-specific types.

module Stack.Types.Build
    (BuildException(..)
    ,BuildPrettyException(..)
    ,ConstructPlanException(..)
    ,BadDependency(..)
    ,ParentMap
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

import           Data.Aeson                      (ToJSON, FromJSON)
import qualified Data.ByteString                 as S
import           Data.Char                       (isSpace)
import           Data.List                       as L
import qualified Data.Map                        as Map
import qualified Data.Map.Strict                 as M
import           Data.Monoid.Map                 (MonoidMap(..))
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import           Database.Persist.Sql
                   ( PersistField (..), PersistFieldSql(..)
                   , PersistValue(PersistText), SqlType(SqlString)
                   )
import           Distribution.PackageDescription
                   ( TestSuiteInterface, mkPackageName )
import           Distribution.System             (Arch)
import qualified Distribution.Text               as C
import qualified Distribution.Version            as C
import           Path                            (parseRelDir, (</>), parent)
import           Path.Extra                      (toFilePathNoTrailingSep)
import           RIO.PrettyPrint
                   ( Style (..), StyleDoc, (<+>), align, encloseSep, flow
                   , indent, line, parens, sep, softline, style, vsep
                   )
import           Stack.Constants
import           Stack.Prelude
import           Stack.Types.Compiler
import           Stack.Types.CompilerBuild
import           Stack.Types.Config
import           Stack.Types.GhcPkgId
import           Stack.Types.NamedComponent
import           Stack.Types.Package
import           Stack.Types.Version
import           System.FilePath                 (pathSeparator)
import           RIO.Process                     (showProcessArgDebug)

-- | Type representing exceptions thrown by functions exported by modules with
-- names beginning @Stack.Build@.
data BuildException
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
  | TaskCycleBug PackageIdentifier
  | PackageIdMissingBug PackageIdentifier
  | AllInOneBuildBug
  | MulipleResultsBug PackageName [DumpPackage]
  | TemplateHaskellNotFoundBug
  | HaddockIndexNotFound
  | ShowBuildErrorBug
  deriving Typeable

instance Show BuildException where
    show (Couldn'tFindPkgId name) = bugReport "[S-7178]" $ concat
        [ "After installing "
        , packageNameString name
        ,", the package id couldn't be found (via ghc-pkg describe "
        , packageNameString name
        , ")."
        ]
    show (CompilerVersionMismatch mactual (expected, eArch) ghcVariant ghcBuild check mstack resolution) = concat
        [ "Error: [S-6362]\n"
        , case mactual of
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
        $ "Error: [S-3127]"
        : "The following targets could not be parsed as package names or \
          \directories:"
        : map T.unpack targets
    show (UnknownTargets noKnown notInSnapshot stackYaml) = unlines
        $ "Error: [S-2154]"
        : (noKnown' ++ notInSnapshot')
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
    show (TestSuiteFailure ident codes mlogFile bs) = unlines
        $ "Error: [S-1995]"
        : concat
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
        indent' = dropWhileEnd isSpace . unlines . fmap (\l -> "  " ++ l) . lines
        doubleIndent = indent' . indent'
    show (TestSuiteTypeUnsupported interface) = concat
        [ "Error: [S-3819]\n"
        , "Unsupported test suite type: "
        , show interface
        ]
     -- Suppressing duplicate output
    show (CabalExitedUnsuccessfully exitCode taskProvides' execName fullArgs logFiles bss) =
        showBuildError "[S-7011]"
            False exitCode (Just taskProvides') execName fullArgs logFiles bss
    show (SetupHsBuildFailure exitCode mtaskProvides execName fullArgs logFiles bss) =
        showBuildError "[S-6374]"
            True exitCode mtaskProvides execName fullArgs logFiles bss
    show (ExecutionFailure es) =
        "Error: [S-7282]\n"
        ++ intercalate "\n\n" (map show es)
    show (LocalPackageDoesn'tMatchTarget name localV requestedV) = concat
        [ "Error: [S-5797]\n"
        , "Version for local package "
        , packageNameString name
        , " is "
        , versionString localV
        , ", but you asked for "
        , versionString requestedV
        , " on the command line"
        ]
    show (NoSetupHsFound dir) = concat
        [ "Error: [S-3118]\n"
        , "No Setup.hs or Setup.lhs file found in "
        , toFilePath dir
        ]
    show (InvalidFlagSpecification unused) = unlines
        $ "Error: [S-8664]"
        : "Invalid flag specification:"
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
        $ "Error: [S-4925]"
        : "Invalid GHC options specification:"
        : map showGhcOptionSrc unused
      where
        showGhcOptionSrc name = concat
            [ "- Package '"
            , packageNameString name
            , "' not found"
            ]
    show (TargetParseException [err]) = concat
        [ "Error: [S-8506]\n"
        , "Error parsing targets: "
        , T.unpack err
        ]
    show (TargetParseException errs) = unlines
        $ "Error [S-8506]"
        : "The following errors occurred while parsing the build targets:"
        : map (("- " ++) . T.unpack) errs
    show (SomeTargetsNotBuildable xs) = unlines
        [ "Error: [S-7086]"
        , "The following components have 'buildable: False' set in the Cabal \
          \configuration, and so cannot be targets:"
        , "    " <> T.unpack (renderPkgComponents xs)
        , "To resolve this, either provide flags such that these components \
          \are buildable, or only specify buildable targets."
        ]
    show (TestSuiteExeMissing isSimpleBuildType exeName pkgName' testName) =
        missingExeError "[S-7987]"
          isSimpleBuildType $ concat
            [ "Test suite executable \""
            , exeName
            , " not found for "
            , pkgName'
            , ":test:"
            , testName
            ]
    show (CabalCopyFailed isSimpleBuildType innerMsg) =
        missingExeError "[S-8027]"
          isSimpleBuildType $ concat
            [ "'cabal copy' failed.  Error message:\n"
            , innerMsg
            , "\n"
            ]
    show (LocalPackagesPresent locals) = unlines
        $ "Error: [S-5510]"
        : "Local packages are not allowed when using the 'script' command. \
          \Packages found:"
        : map (\ident -> "- " ++ packageIdentifierString ident) locals
    show (CouldNotLockDistDir lockFile) = unlines
        [ "Error: [S-7168]"
        , "Locking the dist directory failed, try to lock file:"
        , "  " ++ toFilePath lockFile
        , "Maybe you're running another copy of Stack?"
        ]
    show (TaskCycleBug pid) = bugReport "[S-7868]" $
        "Error: The impossible happened! Unexpected task cycle for "
        ++ packageNameString (pkgName pid)
    show (PackageIdMissingBug ident) = bugReport "[S-8923]" $
        "The impossible happened! singleBuild: missing package ID missing: "
        ++ show ident
    show AllInOneBuildBug = bugReport "[S-7371]"
        "Cannot have an all-in-one build that also has a final build step."
    show (MulipleResultsBug name dps) = bugReport "[S-6739]"
        "singleBuild: multiple results when describing installed package "
        ++ show (name, dps)
    show TemplateHaskellNotFoundBug = bugReport "[S-3121]"
        "template-haskell is a wired-in GHC boot library but it wasn't found."
    show HaddockIndexNotFound =
        "Error: [S-6901]\n"
        ++ "No local or snapshot doc index found to open."
    show ShowBuildErrorBug = bugReport "[S-5452]"
        "Unexpected case in showBuildError."

instance Exception BuildException

data BuildPrettyException
    =  ConstructPlanFailed
           [ConstructPlanException]
           (Path Abs File)
           (Path Abs Dir)
           ParentMap
           (Set PackageName)
           (Map PackageName [PackageName])
    deriving Typeable

instance Show BuildPrettyException where
    show (ConstructPlanFailed {}) = "ConstructPlanFailed"

instance Pretty BuildPrettyException where
    pretty ( ConstructPlanFailed errs stackYaml stackRoot parents wanted prunedGlobalDeps ) =
           "Error:" <+> "[S-4804]"
        <> line
        <> flow "Stack failed to construct a build plan."
        <> line
        <> line
        <> pprintExceptions
               errs stackYaml stackRoot parents wanted prunedGlobalDeps

instance Exception BuildPrettyException

pprintExceptions
    :: [ConstructPlanException]
    -> Path Abs File
    -> Path Abs Dir
    -> ParentMap
    -> Set PackageName
    -> Map PackageName [PackageName]
    -> StyleDoc
pprintExceptions exceptions stackYaml stackRoot parentMap wanted' prunedGlobalDeps =
    mconcat $
      [ flow "While constructing the build plan, the following exceptions were \
             \encountered:"
      , blankLine
      , mconcat (L.intersperse blankLine (mapMaybe pprintException exceptions'))
      ] ++ if L.null recommendations
               then []
               else
                   [ blankLine
                   , flow "Some different approaches to resolving this:"
                   , blankLine
                   ] ++ recommendations

  where
    exceptions' = {- should we dedupe these somehow? nubOrd -} exceptions

    blankLine = line <> line

    recommendations =
        if not onlyHasDependencyMismatches
            then []
            else
                [ "  *" <+> align
                    (   flow "Set 'allow-newer: true' in "
                    <+> pretty (defaultUserConfigPath stackRoot)
                    <+> "to ignore all version constraints and build anyway."
                    )
                , blankLine
                ]
        ++ addExtraDepsRecommendations

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
            pretty stackYaml <> ":")
         , blankLine
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
       in fromString ("- " ++ T.unpack (utf8BuilderToText (display packageIdRev)))

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
                case getShortestDepsPath parentMap wanted' (packageName pkg) of
                    Nothing -> line <> flow "needed for unknown reason - stack invariant violated."
                    Just [] -> line <> flow "needed since" <+> pkgName' <+> flow "is a build target."
                    Just (target:path) -> line <> flow "needed due to" <+> encloseSep "" "" " -> " pathElems
                      where
                        pathElems =
                            [style Target . fromString . packageIdentifierString $ target] ++
                            map (fromString . packageIdentifierString) path ++
                            [pkgIdent]
              where
                pkgName' = style Current . fromString . packageNameString $ packageName pkg
                pkgIdent = style Current . fromString . packageIdentifierString $ packageIdentifier pkg
    -- Skip these when they are redundant with 'NotInBuildPlan' info.
    pprintException (UnknownPackage name)
        | name `Set.member` allNotInBuildPlan = Nothing
        | name `Set.member` wiredInPackages =
            Just $ flow "Can't build a package with same name as a wired-in-package:" <+> (style Current . fromString . packageNameString $ name)
        | Just pruned <- Map.lookup name prunedGlobalDeps =
            let prunedDeps = map (style Current . fromString . packageNameString) pruned
            in Just $ flow "Can't use GHC boot package" <+>
                      (style Current . fromString . packageNameString $ name) <+>
                      flow "when it has an overridden dependency (issue #4510);" <+>
                      flow "you need to add the following as explicit dependencies to the project:" <+>
                      line <+> encloseSep "" "" ", " prunedDeps
        | otherwise = Just $ flow "Unknown package:" <+> (style Current . fromString . packageNameString $ name)

    pprintFlags flags
        | Map.null flags = ""
        | otherwise = parens $ sep $ map pprintFlag $ Map.toList flags
    pprintFlag (name, True) = "+" <> fromString (flagNameString name)
    pprintFlag (name, False) = "-" <> fromString (flagNameString name)

    pprintDep (name, (range, mlatestApplicable, badDep)) = case badDep of
        NotInBuildPlan
          | name `elem` fold prunedGlobalDeps -> Just $
              style Error (fromString $ packageNameString name) <+>
              align ((if range == C.anyVersion
                        then flow "needed"
                        else flow "must match" <+> goodRange) <> "," <> softline <>
                     flow "but this GHC boot package has been pruned (issue #4510);" <+>
                     flow "you need to add the package explicitly to extra-deps" <+>
                     latestApplicable Nothing)
          | otherwise -> Just $
              style Error (fromString $ packageNameString name) <+>
              align ((if range == C.anyVersion
                        then flow "needed"
                        else flow "must match" <+> goodRange) <> "," <> softline <>
                     flow "but the Stack configuration has no specified version" <+>
                     latestApplicable Nothing)
        -- TODO: For local packages, suggest editing constraints
        DependencyMismatch version -> Just $
            (style Error . fromString . packageIdentifierString) (PackageIdentifier name version) <+>
            align (flow "from Stack configuration does not match" <+> goodRange <+>
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
            align (flow $ "dependency cycle detected: " ++ L.intercalate ", " (map packageNameString names))
      where
        goodRange = style Good (fromString (C.display range))
        latestApplicable mversion =
            case mlatestApplicable of
                Nothing
                    | isNothing mversion ->
                        flow "(no package with that name found, perhaps there \
                             \is a typo in a package's build-depends or an \
                             \omission from the stack.yaml packages list?)"
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
getShortestDepsPath (MonoidMap parentsMap) wanted' name =
    if Set.member name wanted'
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
        [PackageIdentifier (mkPackageName "stack-ran-out-of-jet-fuel") (C.mkVersion [0])]
    findShortest _ paths | M.null paths = []
    findShortest fuel paths =
        case targets of
            [] -> findShortest (fuel - 1) $ M.fromListWith chooseBest $ concatMap extendPath recurses
            _ -> let (DepsPath _ _ path) = L.minimum (map snd targets) in path
      where
        (targets, recurses) = L.partition (\(n, _) -> n `Set.member` wanted') (M.toList paths)
    chooseBest :: DepsPath -> DepsPath -> DepsPath
    chooseBest x y = max x y
    -- Extend a path to all its parents.
    extendPath :: (PackageName, DepsPath) -> [(PackageName, DepsPath)]
    extendPath (n, dp) =
        case M.lookup n parentsMap of
            Nothing -> []
            Just (_, parents) -> map (\(pkgId, _) -> (pkgName pkgId, extendDepsPath pkgId dp)) parents

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

data ConstructPlanException
    = DependencyCycleDetected [PackageName]
    | DependencyPlanFailures Package (Map PackageName (VersionRange, LatestApplicableVersion, BadDependency))
    | UnknownPackage PackageName -- TODO perhaps this constructor will be removed, and BadDependency will handle it all
    -- ^ Recommend adding to extra-deps, give a helpful version number?
    deriving (Typeable, Eq, Show)

-- | The latest applicable version and it's latest Cabal file revision.
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

type ParentMap = MonoidMap PackageName (First Version, [(PackageIdentifier, VersionRange)])

data DepsPath = DepsPath
    { dpLength :: Int -- ^ Length of dpPath
    , dpNameLength :: Int -- ^ Length of package names combined
    , dpPath :: [PackageIdentifier] -- ^ A path where the packages later
                                    -- in the list depend on those that
                                    -- come earlier
    }
    deriving (Eq, Ord, Show)

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

missingExeError :: String -> Bool -> String -> String
missingExeError errorCode isSimpleBuildType msg = unlines
    $ "Error: " <> errorCode
    : msg
    : "Possible causes of this issue:"
    : map ("* " <>) possibleCauses
  where
    possibleCauses
        = "No module named \"Main\". The 'main-is' source file should usually \
          \have a header indicating that it's a 'Main' module."
        : "A Cabal file that refers to nonexistent other files (e.g. a \
          \license-file that doesn't exist). Running 'cabal check' may point \
          \out these issues."
        : if isSimpleBuildType
            then []
            else ["The Setup.hs file is changing the installation target dir."]

showBuildError
  :: String
  -> Bool
  -> ExitCode
  -> Maybe PackageIdentifier
  -> Path Abs File
  -> [String]
  -> Maybe (Path Abs File)
  -> [Text]
  -> String
showBuildError errorCode isBuildingSetup exitCode mtaskProvides execName fullArgs logFiles bss =
  let fullCmd = unwords
              $ dropQuotes (toFilePath execName)
              : map (T.unpack . showProcessArgDebug) fullArgs
      logLocations = maybe "" (\fp -> "\n    Logs have been written to: " ++ toFilePath fp) logFiles
  in "Error: " ++ errorCode ++ "\n--  While building " ++
     (case (isBuildingSetup, mtaskProvides) of
       (False, Nothing) -> impureThrow ShowBuildErrorBug
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

    hideSourcePaths ghcVersion = ghcVersion >= C.mkVersion [8, 2] && configHideSourcePaths config

    config = view configL econfig
    bopts = bcoBuildOpts bco

    newerCabal = view cabalVersionL econfig >= C.mkVersion [1, 22]

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
