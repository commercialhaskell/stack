{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Stack.Types.Build.Exception
License     : BSD-3-Clause
-}

module Stack.Types.Build.Exception
  ( BuildException (..)
  , BuildPrettyException (..)
  , pprintTargetParseErrors
  , ConstructPlanException (..)
  , LatestApplicableVersion
  , BadDependency (..)
  ) where

import qualified Data.ByteString as S
import           Data.Char ( isSpace )
import           Data.List as L
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import           Data.Monoid.Map ( MonoidMap (..) )
import qualified Data.Set as Set
import qualified Data.Text as T
import           Distribution.System ( Arch )
import qualified Distribution.Text as C
import           Distribution.Types.PackageName ( mkPackageName )
import           Distribution.Types.TestSuiteInterface ( TestSuiteInterface )
import qualified Distribution.Version as C
import           RIO.NonEmpty ( nonEmpty )
import           RIO.Process ( showProcessArgDebug )
import           Stack.Constants ( defaultUserConfigPath, wiredInPackages )
import           Stack.Prelude
import           Stack.Types.Compiler ( ActualCompiler, compilerVersionString )
import           Stack.Types.CompilerBuild
                   ( CompilerBuild, compilerBuildSuffix )
import           Stack.Types.ComponentUtils
                   ( StackUnqualCompName, unqualCompToString )
import           Stack.Types.DumpPackage ( DumpPackage )
import           Stack.Types.UnusedFlags ( FlagSource (..), UnusedFlags (..) )
import           Stack.Types.GHCVariant ( GHCVariant, ghcVariantSuffix )
import           Stack.Types.NamedComponent
                   ( NamedComponent, renderPkgComponent )
import           Stack.Types.Package ( Package (..), packageIdentifier )
import           Stack.Types.ParentMap ( ParentMap )
import           Stack.Types.Version ( VersionCheck (..), VersionRange )
import           Stack.Types.WantedCompilerSetter ( WantedCompilerSetter (..) )

-- | Type representing exceptions thrown by functions exported by modules with
-- names beginning @Stack.Build@.
data BuildException
  = Couldn'tFindPkgId PackageName
  | Couldn'tParseTargets [Text]
  | UnknownTargets
      (Set PackageName) -- no known version
      (Map PackageName Version) -- not in snapshot, here's the most recent
                                -- version in the index
      (Path Abs File) -- stack.yaml
  | TestSuiteFailure
      PackageIdentifier
      (Map StackUnqualCompName (Maybe ExitCode))
      (Maybe (Path Abs File))
      S.ByteString
  | TestSuiteTypeUnsupported TestSuiteInterface
  | LocalPackageDoesn'tMatchTarget
      PackageName
      Version -- local version
      Version -- version specified on command line
  | NoSetupHsFound (Path Abs Dir)
  | InvalidGhcOptionsSpecification [PackageName]
  | TestSuiteExeMissing Bool String String String
  | CabalCopyFailed Bool String
  | LocalPackagesPresent [PackageIdentifier]
  | CouldNotLockDistDir !(Path Abs File)
  | TaskCycleBug PackageIdentifier
  | PackageIdMissingBug PackageIdentifier
  | AllInOneBuildBug
  | MultipleResultsBug PackageName [DumpPackage]
  | TemplateHaskellNotFoundBug
  | HaddockIndexNotFound
  | ShowBuildErrorBug
  | CallStackEmptyBug
  deriving Show

instance Exception BuildException where
  displayException (Couldn'tFindPkgId name) = bugReport "[S-7178]" $ concat
    [ "After installing "
    , packageNameString name
    ,", the package id couldn't be found (via ghc-pkg describe "
    , packageNameString name
    , ")."
    ]
  displayException (Couldn'tParseTargets targets) = unlines
    $ "Error: [S-3127]"
    : "The following targets could not be parsed as package names or \
      \directories:"
    : map T.unpack targets
  displayException (UnknownTargets noKnown notInSnapshot stackYaml) = unlines
    $ "Error: [S-2154]"
    : (noKnown' ++ notInSnapshot')
   where
    noKnown'
      | Set.null noKnown = []
      | otherwise = pure $
          "The following target packages were not found: " ++
          intercalate ", " (map packageNameString $ Set.toList noKnown) ++
          "\nSee https://docs.haskellstack.org/en/stable/commands/build_command/#target-syntax for details."
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
  displayException (TestSuiteFailure ident codes mlogFile bs) = unlines
    $ "Error: [S-1995]"
    : concat
        [ ["Test suite failure for package " ++ packageIdentifierString ident]
        , flip map (Map.toList codes) $ \(name, mcode) -> concat
            [ "    "
            , unqualCompToString name
            , ": "
            , case mcode of
                Nothing -> " executable not found"
                Just ec -> " exited with: " ++ displayException ec
            ]
        , pure $ case mlogFile of
            Nothing -> "Logs printed to console"
            -- TODO Should we load up the full error output and print it here?
            Just logFile -> "Full log available at " ++ toFilePath logFile
        , if S.null bs
            then []
            else
              [ ""
              , ""
              , doubleIndent $ T.unpack $ decodeUtf8With lenientDecode bs
              ]
        ]
   where
    indent' = dropWhileEnd isSpace . unlines . fmap ("  " ++) . lines
    doubleIndent = indent' . indent'
  displayException (TestSuiteTypeUnsupported interface) = concat
    [ "Error: [S-3819]\n"
    , "Unsupported test suite type: "
    , show interface
    ]
     -- Suppressing duplicate output
  displayException (LocalPackageDoesn'tMatchTarget name localV requestedV) = concat
    [ "Error: [S-5797]\n"
    , "Version for project package "
    , packageNameString name
    , " is "
    , versionString localV
    , ", but you asked for "
    , versionString requestedV
    , " on the command line"
    ]
  displayException (NoSetupHsFound dir) = concat
    [ "Error: [S-3118]\n"
    , "No Setup.hs or Setup.lhs file found in "
    , toFilePath dir
    ]
  displayException (InvalidGhcOptionsSpecification unused) = unlines
    $ "Error: [S-4925]"
    : "Invalid GHC options specification:"
    : map showGhcOptionSrc unused
   where
    showGhcOptionSrc name = concat
      [ "- Package '"
      , packageNameString name
      , "' not found"
      ]
  displayException (TestSuiteExeMissing isSimpleBuildType exeName pkgName' testName) =
    missingExeError "[S-7987]"
      isSimpleBuildType $ concat
        [ "Test suite executable \""
        , exeName
        , " not found for "
        , pkgName'
        , ":test:"
        , testName
        ]
  displayException (CabalCopyFailed isSimpleBuildType innerMsg) =
    missingExeError "[S-8027]"
      isSimpleBuildType $ concat
        [ "'cabal copy' failed.  Error message:\n"
        , innerMsg
        , "\n"
        ]
  displayException (LocalPackagesPresent locals) = unlines
    $ "Error: [S-5510]"
    : "Local packages are not allowed when using the 'script' command. \
      \Packages found:"
    : map (\ident -> "- " ++ packageIdentifierString ident) locals
  displayException (CouldNotLockDistDir lockFile) = unlines
    [ "Error: [S-7168]"
    , "Locking the dist directory failed, try to lock file:"
    , "  " ++ toFilePath lockFile
    , "Maybe you're running another copy of Stack?"
    ]
  displayException (TaskCycleBug pid) = bugReport "[S-7868]" $
    "Unexpected task cycle for "
    ++ packageNameString (pkgName pid)
  displayException (PackageIdMissingBug ident) = bugReport "[S-8923]" $
    "singleBuild: missing package ID missing: "
    ++ show ident
  displayException AllInOneBuildBug = bugReport "[S-7371]"
    "Cannot have an all-in-one build that also has a final build step."
  displayException (MultipleResultsBug name dps) = bugReport "[S-6739]" $
    "singleBuild: multiple results when describing installed package "
    ++ show (name, dps)
  displayException TemplateHaskellNotFoundBug = bugReport "[S-3121]"
    "template-haskell is a wired-in GHC boot library but it wasn't found."
  displayException HaddockIndexNotFound =
    "Error: [S-6901]\n"
    ++ "No local or snapshot doc index found to open."
  displayException ShowBuildErrorBug = bugReport "[S-5452]"
    "Unexpected case in showBuildError."
  displayException CallStackEmptyBug = bugReport "[S-2696]"
    "addDep: call stack is empty."

data BuildPrettyException
  = ConstructPlanFailed
      [ConstructPlanException]
      (Either (Path Abs File) (Path Abs File))
      (Path Abs Dir)
      Bool -- Is the project the implicit global project?
      ParentMap
      (Set PackageName)
      (Map PackageName [PackageName])
  | ExecutionFailure [SomeException]
  | CabalExitedUnsuccessfully
      ExitCode
      PackageIdentifier
      (Path Abs File)  -- cabal Executable
      [String]         -- cabal arguments
      (Maybe (Path Abs File)) -- logfiles location
      [Text]     -- log contents
  | SetupHsBuildFailure
      ExitCode
      (Maybe PackageIdentifier) -- which package's custom setup, is simple setup
                                -- if Nothing
      (Path Abs File)  -- ghc Executable
      [String]         -- ghc arguments
      (Maybe (Path Abs File)) -- logfiles location
      [Text]     -- log contents
  | TargetParseException [StyleDoc]
  | SomeTargetsNotBuildable [(PackageName, NamedComponent)]
  | InvalidFlagSpecification [UnusedFlags]
  | GHCProfOptionInvalid
  | NotOnlyLocal [PackageName] [StackUnqualCompName]
  | CompilerVersionMismatch
      (Maybe (ActualCompiler, Arch)) -- found
      (WantedCompiler, Arch) -- expected
      GHCVariant -- expected
      CompilerBuild -- expected
      VersionCheck
      WantedCompilerSetter -- Way that the wanted compiler is set
      StyleDoc -- recommended resolution
  | ActionNotFilteredBug StyleDoc
  deriving Show

instance Pretty BuildPrettyException where
  pretty ( ConstructPlanFailed errs configFile stackRoot isImplicitGlobal parents wanted prunedGlobalDeps ) =
    "[S-4804]"
    <> line
    <> flow "Stack failed to construct a build plan."
    <> blankLine
    <> pprintExceptions
         errs configFile stackRoot isImplicitGlobal parents wanted prunedGlobalDeps
  pretty (ExecutionFailure es) =
    "[S-7282]"
    <> line
    <> flow "Stack failed to execute the build plan."
    <> blankLine
    <> fillSep
         [ flow "While executing the build plan, Stack encountered the"
         , case es of
             [_] -> "error:"
             _ -> flow "following errors:"
         ]
    <> blankLine
    <> hcat (L.intersperse blankLine (map ppException es))
  pretty (CabalExitedUnsuccessfully exitCode taskProvides' execName fullArgs logFiles bss) =
    showBuildError "[S-7011]"
      False exitCode (Just taskProvides') execName fullArgs logFiles bss
  pretty (SetupHsBuildFailure exitCode mtaskProvides execName fullArgs logFiles bss) =
    showBuildError "[S-6374]"
      True exitCode mtaskProvides execName fullArgs logFiles bss
  pretty (TargetParseException errs) =
    "[S-8506]"
    <> pprintTargetParseErrors errs
  pretty (SomeTargetsNotBuildable xs) =
    "[S-7086]"
    <> line
    <> fillSep
         (  [ flow "The following components have"
            , style Shell (flow "buildable: False")
            , flow "set in the Cabal configuration, and so cannot be targets:"
            ]
         <> mkNarrativeList (Just Target) False
              (map (fromString . T.unpack . renderPkgComponent) xs :: [StyleDoc])
         )
    <> blankLine
    <> flow "To resolve this, either provide flags such that these components \
            \are buildable, or only specify buildable targets."
  pretty (InvalidFlagSpecification unused) =
    "[S-8664]"
    <> line
    <> flow "Invalid flag specification:"
    <> line
    <> bulletedList (map go (L.sort unused))
   where
    showFlagSrc :: FlagSource -> StyleDoc
    showFlagSrc FSCommandLine = flow "(specified on the command line)"
    showFlagSrc FSStackYaml =
      flow "(specified in the project-level configuration (e.g. stack.yaml))"

    go :: UnusedFlags -> StyleDoc
    go (UFNoPackage src name) = fillSep
      [ "Package"
      , style Error (fromPackageName name)
      , flow "not found"
      , showFlagSrc src
      ]
    go (UFFlagsNotDefined src pname pkgFlags flags) =
         fillSep
           ( "Package"
           : style Current (fromString name)
           : flow "does not define the following flags"
           : showFlagSrc src <> ":"
           : mkNarrativeList (Just Error) False
               (map (fromString . flagNameString) (Set.toList flags) :: [StyleDoc])
           )
      <> line
      <> if Set.null pkgFlags
           then fillSep
             [ flow "No flags are defined by package"
             , style Current (fromString name) <> "."
             ]
           else fillSep
           ( flow "Flags defined by package"
           : style Current (fromString name)
           : "are:"
           : mkNarrativeList (Just Good) False
               (map (fromString . flagNameString) (Set.toList pkgFlags) :: [StyleDoc])
           )
     where
      name = packageNameString pname
    go (UFSnapshot name) = fillSep
      [ flow "Attempted to set flag on snapshot package"
      , style Current (fromPackageName name) <> ","
      , flow "please add the package to"
      , style Shell "extra-deps" <> "."
      ]
  pretty GHCProfOptionInvalid =
    "[S-8100]"
    <> line
    <> fillSep
         [ flow "When building with Stack, you should not use GHC's"
         , style Shell "-prof"
         , flow "option. Instead, please use Stack's"
         , style Shell "--library-profiling"
         , "and"
         , style Shell "--executable-profiling"
         , flow "flags. See:"
         , style Url "https://github.com/commercialhaskell/stack/issues/1015" <> "."
         ]
  pretty (NotOnlyLocal packages exes) =
    "[S-1727]"
    <> line
    <> flow "Specified only-locals, but Stack needs to build snapshot contents:"
    <> line
    <> if null packages
         then mempty
         else
              fillSep
                ( "Packages:"
                : mkNarrativeList Nothing False
                    (map fromPackageName packages :: [StyleDoc])
                )
           <> line
    <> if null exes
         then mempty
         else
              fillSep
                ( "Executables:"
                : mkNarrativeList Nothing False
                    (map (fromString . unqualCompToString) exes :: [StyleDoc])
                )
           <> line
  pretty ( CompilerVersionMismatch
             mactual
             (expected, eArch)
             ghcVariant
             ghcBuild
             check
             wantedCompilerSetter
             resolution
         ) =
    "[S-6362]"
    <> line
    <> fillSep
         [ case mactual of
             Nothing -> flow "No compiler found, expected"
             Just (actual, arch) -> fillSep
               [ flow "Compiler version mismatched, found"
               , fromString $ compilerVersionString actual
               , parens (pretty arch) <> ","
               , flow "but expected"
               ]
         , case check of
             MatchMinor -> flow "minor version match with"
             MatchExact -> flow "exact version"
             NewerMinor -> flow "minor version match or newer with"
         , fromString $ T.unpack $ utf8BuilderToText $ display expected
         , parens $ mconcat
             [ pretty eArch
             , fromString $ ghcVariantSuffix ghcVariant
             , fromString $ compilerBuildSuffix ghcBuild
             ]
         ,    parens
                ( fillSep
                    [ flow "based on"
                    , case wantedCompilerSetter of
                        CompilerAtCommandLine -> fillSep
                          [ "the"
                          , style Shell "--compiler"
                          , "option"
                          ]
                        SnapshotAtCommandLine -> fillSep
                          [ "the"
                          , style Shell "--snapshot" <> ","
                          , "or (deprecated)"
                          , style Shell "--resolver" <> ","
                          , "option"
                          ]
                        YamlConfiguration mConfigFile -> case mConfigFile of
                          Nothing -> flow "command line arguments"
                          Just configFile -> fillSep
                            [ flow "the configuration in"
                            , pretty configFile
                            ]
                    ]
                )
          <> "."
         ]
    <> blankLine
    <> resolution
  pretty (ActionNotFilteredBug source) = bugPrettyReport "S-4660" $
    fillSep
      [ source
      , flow "is seeking to run an action that should have been filtered from \
             \the list of actions."
      ]

instance Exception BuildPrettyException

-- | Helper function to pretty print an error message for target parse errors.
pprintTargetParseErrors :: [StyleDoc] -> StyleDoc
pprintTargetParseErrors errs =
     line
  <> flow "Stack failed to parse the target(s)."
  <> blankLine
  <> fillSep
       [ flow "While parsing, Stack encountered the"
       , case errs of
           [err] ->
                  "error:"
               <> blankLine
               <> indent 4 err
           _ ->
                  flow "following errors:"
               <> blankLine
               <> bulletedList errs
       ]
  <> blankLine
  <> fillSep
       [ flow "Stack expects a target to be a package name (e.g."
       , style Shell "my-package" <> "),"
       , flow "a package identifier (e.g."
       , style Shell "my-package-0.1.2.3" <> "),"
       , flow "a package component (e.g."
       , style Shell "my-package:test:my-test-suite" <> "),"
       , flow "or, failing that, a relative path to a local directory for a \
              \package or a parent directory of one or more such directories."
       ]

pprintExceptions ::
     [ConstructPlanException]
  -> Either (Path Abs File) (Path Abs File)
     -- ^ The configuration file, which may be either (Left) a user-specific
     -- global one or (Right) a project-level one.
  -> Path Abs Dir
  -> Bool
  -> ParentMap
  -> Set PackageName
  -> Map PackageName [PackageName]
  -> StyleDoc
pprintExceptions exceptions configFile stackRoot isImplicitGlobal parentMap wanted' prunedGlobalDeps =
     fillSep
       [ flow
           (  "While constructing the build plan, Stack encountered the \
              \following errors"
           <> if hasConfigurationRefs then "." else ":"
           )
       , if hasConfigurationRefs
           then flow
             "The 'Stack configuration' refers to the set of package versions \
             \specified by the snapshot (after any dropped packages, or pruned \
             \GHC boot packages; if a boot package is replaced, Stack prunes \
             \all other such packages that depend on it) and any extra-deps:"
           else mempty
       ]
  <> blankLine
  <> mconcat (L.intersperse blankLine (mapMaybe pprintException exceptions'))
  <> if L.null recommendations
       then mempty
       else
            blankLine
         <> flow "Some different approaches to resolving some or all of this:"
         <> blankLine
         <> indent 2 (spacedBulletedList recommendations)
 where
  exceptions' = {- should we dedupe these somehow? nubOrd -} exceptions

  recommendations =
       [ allowNewerMsg True False | onlyHasDependencyMismatches ]
    <> [ fillSep
           $ allowNewerMsg False onlyHasDependencyMismatches
           : flow "add these package names under"
           : style Shell "allow-newer-deps" <> ":"
           : mkNarrativeList (Just Shell) False
               (map fromPackageName (Set.elems pkgsWithMismatches) :: [StyleDoc])
       | not $ Set.null pkgsWithMismatches
       ]
    <> addExtraDepsRecommendations
   where
    allowNewerMsg isAll isRepetitive = fillSep
      $ flow "To ignore"
      : (if isAll then "all" else "certain")
      : flow "version constraints and build anyway,"
      : if isRepetitive
          then ["also"]
          else
            [ fillSep
                $  [ "pass"
                   , style Shell "--allow-newer" <> ","
                   , flow "or, in"
                   , pretty (defaultUserConfigPath stackRoot)
                   , flow
                       (  "(global configuration)"
                       <> if isImplicitGlobal then "," else mempty
                       )
                   ]
                <> ( case configFile of
                      Left _ -> []
                      Right projectConfigFile -> if isImplicitGlobal
                        then []
                        else
                          [ "or"
                          , pretty projectConfigFile
                          , flow "(project-level configuration),"
                          ]
                   )
                <> [ "set"
                   ,    style Shell (flow "allow-newer: true")
                     <> if isAll then "." else mempty
                   ]
                <> [ "and" | not isAll ]
            ]

  addExtraDepsRecommendations
    | Map.null extras = []
    | (Just _) <- Map.lookup (mkPackageName "base") extras =
        [ fillSep
            [ flow "Build requires unattainable version of the"
            , style Current "base"
            , flow "package. Since"
            , style Current "base"
            , flow "is a part of GHC, you most likely need to use a \
                   \different GHC version with the matching"
            , style Current "base"<> "."
            ]
        ]
    | otherwise =
        [    fillSep
               [ style Recommendation (flow "Recommended action:")
               , flow "try adding the following to your"
               , case configFile of
                   Left _ -> fillSep
                     [ style Shell "--extra-dep"
                     , flow "options of the"
                     , style Shell (flow "stack script")
                     , "command:"
                     ]
                   Right projectConfigFile -> fillSep
                     [ style Shell "extra-deps"
                     , "in"
                     , pretty projectConfigFile
                     , "(project-level configuration):"
                     ]
               ]
          <> blankLine
          <> vsep (map pprintExtra (Map.toList extras))
        ]

  pprintExtra (name, (version, BlobKey cabalHash cabalSize)) =
    let cfInfo = CFIHash cabalHash (Just cabalSize)
        packageIdRev = PackageIdentifierRevision name version cfInfo
    in  fromString ("- " ++ T.unpack (utf8BuilderToText (display packageIdRev)))

  allNotInBuildPlan = Set.fromList $ concatMap toNotInBuildPlan exceptions'
  toNotInBuildPlan (DependencyPlanFailures _ pDeps) =
    map fst $
      filter
        (\(_, (_, _, badDep)) -> badDep == NotInBuildPlan)
        (Map.toList pDeps)
  toNotInBuildPlan _ = []

  (onlyHasDependencyMismatches, hasConfigurationRefs, extras, pkgsWithMismatches) =
    filterExceptions

  filterExceptions ::
    ( Bool
      -- ^ All the errors are DependencyMismatch. This checks if
      -- 'allow-newer: true' could resolve all reported issues.
    , Bool
      -- ^ One or more messages refer to 'the Stack configuration'. This
      -- triggers a message to explain what that phrase means.
    , Map PackageName (Version, BlobKey)
      -- ^ Recommended extras. TO DO: Likely a good idea to distinguish these to
      -- the user. In particular, those recommended for DependencyMismatch.
    , Set.Set PackageName
      -- ^ Set of names of packages with one or more DependencyMismatch errors.
    )
  filterExceptions = L.foldl' go acc0 exceptions'
   where
    acc0 = (True, False, Map.empty, Set.empty)
    go acc (DependencyPlanFailures pkg m) = Map.foldrWithKey go' acc m
     where
      pkgName = pkg.name
      go' name (_, Just extra, NotInBuildPlan) (_, _, m', s) =
        (False, True, Map.insert name extra m', s)
      go' _ (_, _, NotInBuildPlan) (_, _, m', s) = (False, True, m', s)
      go' name (_, Just extra, DependencyMismatch _) (p1, _, m', s) =
        (p1, True, Map.insert name extra m', Set.insert pkgName s)
      go' _ (_, _, DependencyMismatch _) (p1, _, m', s) =
        (p1, True, m', Set.insert pkgName s)
      go' _ (_, _, Couldn'tResolveItsDependencies _) acc' = acc'
      go' _ _ (_, p2, m', s) = (False, p2, m', s)
    go (_, p2, m, s) _ = (False, p2, m, s)

  pprintException (DependencyCycleDetected pNames) = Just $
       flow "Dependency cycle detected in packages:"
    <> line
    <> indent 4
         (encloseSep "[" "]" "," (map (style Error . fromPackageName) pNames))
  pprintException (DependencyPlanFailures pkg pDeps) =
    case mapMaybe pprintDep (Map.toList pDeps) of
      [] -> Nothing
      depErrors -> Just $
           fillSep
             [ flow "In the dependencies for"
             , pkgIdent <> pprintFlags pkg.flags <> ":"
             ]
        <> line
        <> indent 2 (bulletedList depErrors)
        <> line
        <> fillSep
             ( flow "The above is/are needed"
             : case getShortestDepsPath parentMap wanted' pkg.name of
                 Nothing ->
                   [flow "for unknown reason - Stack invariant violated."]
                 Just [] ->
                   [ "since"
                   , pkgName'
                   , flow "is a build target."
                   ]
                 Just (target:path) ->
                   [ flow "due to"
                   , encloseSep "" "" " -> " pathElems
                   ]
                  where
                   pathElems =
                        [style Target . fromPackageId $ target]
                     <> map fromPackageId path
                     <> [pkgIdent]
             )
       where
        pkgName' = style Current (fromPackageName pkg.name)
        pkgIdent = style Current (fromPackageId $ packageIdentifier pkg)
  -- Skip these when they are redundant with 'NotInBuildPlan' info.
  pprintException (UnknownPackage compiler name)
    | name `Set.member` allNotInBuildPlan = Nothing
    | name `Set.member` wiredInPackages compiler =
        Just $ fillSep
          [ flow "Can't build a package with same name as a wired-in-package:"
          , style Current . fromPackageName $ name
          ]
    | Just pruned <- Map.lookup name prunedGlobalDeps =
        let prunedDeps =
              map (style Current . fromPackageName) pruned
        in  Just $ fillSep
              [ flow "Can't use GHC boot package"
              , style Current . fromPackageName $ name
              , flow "when it depends on a replaced boot package. You need to \
                     \add the following as explicit dependencies to the \
                     \project:"
              , line
              , encloseSep "" "" ", " prunedDeps
              ]
    | otherwise = Just $ fillSep
        [ flow "Unknown package:"
        , style Current . fromPackageName $ name
        ]

  pprintFlags flags
    | Map.null flags = ""
    | otherwise = parens $ sep $ map pprintFlag $ Map.toList flags
  pprintFlag (name, True) = "+" <> fromString (flagNameString name)
  pprintFlag (name, False) = "-" <> fromString (flagNameString name)

  pprintDep (name, (range, mlatestApplicable, badDep)) = case badDep of
    NotInBuildPlan
      | name `elem` fold prunedGlobalDeps -> butMsg $ fillSep
          [ flow "this GHC boot package has been pruned from the Stack \
                 \configuration. You need to add the package explicitly to"
          , style Shell "extra-deps" <> "."
          ]
      | otherwise -> butMsg $ inconsistentMsg Nothing
    -- TODO: For local packages, suggest editing constraints
    DependencyMismatch version -> butMsg $ inconsistentMsg $ Just version
    -- I think the main useful info is these explain why missing packages are
    -- needed. Instead lets give the user the shortest path from a target to the
    -- package.
    Couldn'tResolveItsDependencies _version -> Nothing
    HasNoLibrary -> Just $ fillSep
      [ errorName
      , flow "is a library dependency, but the package provides no library."
      ]
    BDDependencyCycleDetected names -> Just $ fillSep
      [ errorName
      , flow $ "dependency cycle detected: "
             ++ L.intercalate ", " (map packageNameString names)
      ]
   where
    errorName = style Error . fromPackageName $ name
    goodRange = style Good (fromString (C.display range))
    rangeMsg = if range == C.anyVersion
      then "needed,"
      else fillSep
        [ flow "must match"
        , goodRange <> ","
        ]
    butMsg msg = Just $ fillSep
      [ errorName
      , rangeMsg
      , "but"
      , msg
      , latestApplicable Nothing
      ]
    inconsistentMsg mVersion = fillSep
      [ style Error $ maybe
          ( flow "no version" )
          ( fromPackageId . PackageIdentifier name )
          mVersion
      , flow "is in the Stack configuration"
      ]
    latestApplicable mversion =
      case mlatestApplicable of
        Nothing
          | isNothing mversion -> fillSep
              [ flow "(no matching package and version found. Perhaps there is \
                     \an error in the specification of a package's"
              , style Shell "dependencies"
              , "or"
              , style Shell "build-tools"
              , flow "(Hpack) or"
              , style Shell "build-depends" <> ","
              , style Shell "build-tools"
              , "or"
              , style Shell "build-tool-depends"
              , case configFile of
                  Left _ -> flow "(Cabal file)."
                  Right projectConfigFile -> fillSep
                    [ flow "(Cabal file) or an omission from the"
                    , style Shell "packages"
                    , flow "list in"
                    , pretty projectConfigFile
                    , flow "(project-level configuration).)"
                    ]
              ]
          | otherwise -> ""
        Just (laVer, _)
          | Just laVer == mversion ->
              flow "(latest matching version is specified)."
          | otherwise ->
              fillSep
                [ flow "(latest matching version is"
                , style Good (fromString $ versionString laVer) <> ")."
                ]

data ConstructPlanException
  = DependencyCycleDetected [PackageName]
  | DependencyPlanFailures
      Package
      (Map PackageName (VersionRange, LatestApplicableVersion, BadDependency))
  | UnknownPackage ActualCompiler PackageName
    -- TODO perhaps this constructor will be removed, and BadDependency will
    -- handle it all
  -- ^ Recommend adding to extra-deps, give a helpful version number?
  deriving (Eq, Show)

-- | The latest applicable version and it's latest Cabal file revision.
-- For display purposes only, Nothing if package not found
type LatestApplicableVersion = Maybe (Version, BlobKey)

-- | Reason why a dependency was not used
data BadDependency
  = NotInBuildPlan
  | Couldn'tResolveItsDependencies Version
  | DependencyMismatch Version
  | HasNoLibrary
  -- ^ See description of 'Stack.Types.Dependency.DepType'
  | BDDependencyCycleDetected ![PackageName]
  deriving (Eq, Ord, Show)

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
    : [ "The Setup.hs file is changing the installation target dir."
      | not isSimpleBuildType
      ]

showBuildError ::
     String
  -> Bool
  -> ExitCode
  -> Maybe PackageIdentifier
  -> Path Abs File
  -> [String]
  -> Maybe (Path Abs File)
  -> [Text]
  -> StyleDoc
showBuildError errorCode isBuildingSetup exitCode mtaskProvides execName fullArgs logFiles bss =
  let fullCmd = unwords
        $ dropQuotes (toFilePath execName)
        : map (T.unpack . showProcessArgDebug) fullArgs
      logLocations =
        maybe
          mempty
          (\fp -> line <> flow "Logs have been written to:" <+>
                    pretty fp)
          logFiles
  in     fromString errorCode
      <> line
      <> flow "While building" <+>
         ( case (isBuildingSetup, mtaskProvides) of
             (False, Nothing) -> impureThrow ShowBuildErrorBug
             (False, Just taskProvides') ->
                "package" <+>
                  style
                    Target
                    (fromString $ dropQuotes (packageIdentifierString taskProvides'))
             (True, Nothing) -> "simple" <+> style File "Setup.hs"
             (True, Just taskProvides') ->
                "custom" <+>
                  style File "Setup.hs" <+>
                  flow "for package" <+>
                  style
                    Target
                    (fromString $ dropQuotes (packageIdentifierString taskProvides'))
         ) <+>
         flow "(scroll up to its section to see the error) using:"
      <> line
      <> style Shell (fromString fullCmd)
      <> line
      <> flow "Process exited with code:" <+> (fromString . show) exitCode <+>
         ( if exitCode == ExitFailure (-9)
             then flow "(THIS MAY INDICATE OUT OF MEMORY)"
             else mempty
         )
      <> logLocations
      <> if null bss
           then mempty
           else blankLine <> string (removeTrailingSpaces (map T.unpack bss))
   where
    removeTrailingSpaces = dropWhileEnd isSpace . unlines
    dropQuotes = filter ('\"' /=)

-- | Get the shortest reason for the package to be in the build plan. In other
-- words, trace the parent dependencies back to a \'wanted\' package.
getShortestDepsPath ::
     ParentMap
  -> Set PackageName
  -> PackageName
  -> Maybe [PackageIdentifier]
getShortestDepsPath (MonoidMap parentsMap) wanted' name =
  if Set.member name wanted'
    then Just []
    else case M.lookup name parentsMap of
      Nothing -> Nothing
      Just parents -> Just $ findShortest 256 paths0
       where
        paths0 = M.fromList $
          map (\(ident, _) -> (pkgName ident, startDepsPath ident)) parents
 where
  -- The 'paths' map is a map from PackageName to the shortest path
  -- found to get there. It is the frontier of our breadth-first
  -- search of dependencies.
  findShortest :: Int -> Map PackageName DepsPath -> [PackageIdentifier]
  findShortest fuel _ | fuel <= 0 =
    [ PackageIdentifier
        (mkPackageName "stack-ran-out-of-jet-fuel")
        (C.mkVersion [0])
    ]
  findShortest _ paths | M.null paths = []
  findShortest fuel paths =
    case nonEmpty targets of
      Nothing -> findShortest (fuel - 1) $ M.fromListWith chooseBest $
              concatMap extendPath recurses
      Just targets' ->
        let (DepsPath _ _ path) = minimum (snd <$> targets')
        in  path
   where
    (targets, recurses) =
      L.partition (\(n, _) -> n `Set.member` wanted') (M.toList paths)
  chooseBest :: DepsPath -> DepsPath -> DepsPath
  chooseBest = max
  -- Extend a path to all its parents.
  extendPath :: (PackageName, DepsPath) -> [(PackageName, DepsPath)]
  extendPath (n, dp) =
    case M.lookup n parentsMap of
      Nothing -> []
      Just parents ->
        map (\(pkgId, _) -> (pkgName pkgId, extendDepsPath pkgId dp)) parents

startDepsPath :: PackageIdentifier -> DepsPath
startDepsPath ident = DepsPath
  { dpLength = 1
  , dpNameLength = length (packageNameString (pkgName ident))
  , dpPath = [ident]
  }

extendDepsPath :: PackageIdentifier -> DepsPath -> DepsPath
extendDepsPath ident dp = DepsPath
  { dpLength = dp.dpLength + 1
  , dpNameLength = dp.dpNameLength + length (packageNameString (pkgName ident))
  , dpPath = [ident]
  }

data DepsPath = DepsPath
  { dpLength :: Int
    -- ^ Length of dpPath
  , dpNameLength :: Int
    -- ^ Length of package names combined
  , dpPath :: [PackageIdentifier]
    -- ^ A path where the packages later in the list depend on those that come
    -- earlier
  }
  deriving (Eq, Ord, Show)
