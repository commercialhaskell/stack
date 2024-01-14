{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Resolving a build plan for a set of packages in a given Stackage snapshot.

module Stack.BuildPlan
  ( BuildPlanException (..)
  , BuildPlanCheck (..)
  , checkSnapBuildPlan
  , DepError (..)
  , DepErrors
  , removeSrcPkgDefaultFlags
  , selectBestSnapshot
  , showItems
  ) where

import qualified Data.Foldable as F
import           Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Distribution.Package as C
import           Distribution.PackageDescription
                   ( GenericPackageDescription, flagDefault, flagName
                   , flagManual, genPackageFlags
                   )
import qualified Distribution.PackageDescription as C
import           Distribution.System ( Platform )
import           Distribution.Text ( display )
import           Distribution.Types.UnqualComponentName
                   ( unUnqualComponentName )
import qualified Distribution.Version as C
import qualified RIO.NonEmpty as NE
import           Stack.Constants ( wiredInPackages )
import           Stack.Package
                   ( PackageConfig (..), packageDependencies
                   , resolvePackageDescription
                   )
import           Stack.Prelude hiding ( Display (..) )
import           Stack.SourceMap
                   ( SnapshotCandidate, loadProjectSnapshotCandidate )
import           Stack.Types.Compiler
                   ( ActualCompiler, WhichCompiler (..), compilerVersionText
                   , whichCompiler
                   )
import           Stack.Types.Config ( HasConfig )
import           Stack.Types.GHCVariant ( HasGHCVariant )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.SourceMap
                   ( CommonPackage (..), DepPackage (..)
                   , GlobalPackageVersion (..), ProjectPackage (..)
                   , SMActual (..)
                   )
import           Stack.Types.Version ( VersionRange, withinRange )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.BuildPlan" module.
data BuildPlanException
  = UnknownPackages
      (Path Abs File) -- stack.yaml file
      (Map PackageName (Maybe Version, Set PackageName)) -- truly unknown
      (Map PackageName (Set PackageIdentifier)) -- shadowed
  | SnapshotNotFound SnapName
  | NeitherCompilerOrResolverSpecified T.Text
  | DuplicatePackagesBug
  deriving (Show, Typeable)

instance Exception BuildPlanException where
  displayException (SnapshotNotFound snapName) = unlines
    [ "Error: [S-2045]"
    , "SnapshotNotFound " ++ snapName'
    , "Non existing resolver: " ++ snapName' ++ "."
    , "For a complete list of available snapshots see https://www.stackage.org/snapshots"
    ]
   where
    snapName' = show snapName
  displayException (UnknownPackages stackYaml unknown shadowed) =
    "Error: [S-7571]\n"
    ++ unlines (unknown' ++ shadowed')
   where
    unknown' :: [String]
    unknown'
      | Map.null unknown = []
      | otherwise = concat
          [ ["The following packages do not exist in the build plan:"]
          , map go (Map.toList unknown)
          , case mapMaybe goRecommend $ Map.toList unknown of
              [] -> []
              rec ->
                  ("Recommended action: modify the extra-deps field of " ++
                  toFilePath stackYaml ++
                  " to include the following:")
                  : (rec
                  ++ ["Note: further dependencies may need to be added"])
          , case mapMaybe getNoKnown $ Map.toList unknown of
              [] -> []
              noKnown ->
                  [ "There are no known versions of the following packages:"
                  , intercalate ", " $ map packageNameString noKnown
                  ]
          ]
     where
      go (dep, (_, users)) | Set.null users = packageNameString dep
      go (dep, (_, users)) = concat
        [ packageNameString dep
        , " (used by "
        , intercalate ", " $ map packageNameString $ Set.toList users
        , ")"
        ]

      goRecommend (name, (Just version, _)) =
        Just $ "- " ++ packageIdentifierString (PackageIdentifier name version)
      goRecommend (_, (Nothing, _)) = Nothing

      getNoKnown (name, (Nothing, _)) = Just name
      getNoKnown (_, (Just _, _)) = Nothing

    shadowed' :: [String]
    shadowed'
      | Map.null shadowed = []
      | otherwise = concat
          [ ["The following packages are shadowed by local packages:"]
          , map go (Map.toList shadowed)
          , ["Recommended action: modify the extra-deps field of " ++
             toFilePath stackYaml ++
             " to include the following:"]
          , extraDeps
          , ["Note: further dependencies may need to be added"]
          ]
     where
      go (dep, users) | Set.null users = packageNameString dep ++ " (internal Stack error: this should never be null)"
      go (dep, users) = concat
        [ packageNameString dep
        , " (used by "
        , intercalate ", "
            $ map (packageNameString . pkgName)
            $ Set.toList users
        , ")"
        ]

      extraDeps = map (\ident -> "- " ++ packageIdentifierString ident)
                $ Set.toList
                $ Set.unions
                $ Map.elems shadowed
  displayException (NeitherCompilerOrResolverSpecified url) = concat
    [ "Error: [S-8559]\n"
    , "Failed to load custom snapshot at "
    , T.unpack url
    , ", because no 'compiler' or 'resolver' is specified."
    ]
  displayException DuplicatePackagesBug = bugReport "[S-5743]"
    "Duplicate packages are not expected here."

gpdPackages :: [GenericPackageDescription] -> Map PackageName Version
gpdPackages = Map.fromList . map (toPair . C.package . C.packageDescription)
 where
  toPair (C.PackageIdentifier name version) = (name, version)

gpdPackageDeps ::
     GenericPackageDescription
  -> ActualCompiler
  -> Platform
  -> Map FlagName Bool
  -> Map PackageName VersionRange
gpdPackageDeps gpd ac platform flags =
  Map.filterWithKey (const . not . isLocalLibrary) (packageDependencies pkgDesc)
 where
  isLocalLibrary name' = name' == name || name' `Set.member` subs

  name = gpdPackageName gpd
  subs = Set.fromList
       $ map (C.mkPackageName . unUnqualComponentName . fst)
       $ C.condSubLibraries gpd

  -- Since tests and benchmarks are both enabled, doesn't matter if we choose
  -- modified or unmodified
  pkgDesc = resolvePackageDescription pkgConfig gpd
  pkgConfig = PackageConfig
    { enableTests = True
    , enableBenchmarks = True
    , flags = flags
    , ghcOptions = []
    , cabalConfigOpts = []
    , compilerVersion = ac
    , platform = platform
    }

-- Remove any src package flags having default values
-- Remove any package entries with no flags set
removeSrcPkgDefaultFlags ::
     [C.GenericPackageDescription]
  -> Map PackageName (Map FlagName Bool)
  -> Map PackageName (Map FlagName Bool)
removeSrcPkgDefaultFlags gpds flags =
  let defaults = Map.unions (map gpdDefaultFlags gpds)
      flags'   = Map.differenceWith removeSame flags defaults
  in  Map.filter (not . Map.null) flags'
 where
  removeSame f1 f2 =
    let diff v v' = if v == v' then Nothing else Just v
    in  Just $ Map.differenceWith diff f1 f2

  gpdDefaultFlags gpd =
    let tuples = map getDefault (C.genPackageFlags gpd)
    in  Map.singleton (gpdPackageName gpd) (Map.fromList tuples)

  getDefault f
    | C.flagDefault f = (C.flagName f, True)
    | otherwise       = (C.flagName f, False)

-- | Find the set of @FlagName@s necessary to get the given
-- @GenericPackageDescription@ to compile against the given @BuildPlan@. Will
-- only modify non-manual flags, and will prefer default values for flags.
-- Returns the plan which produces least number of dep errors
selectPackageBuildPlan ::
     Platform
  -> ActualCompiler
  -> Map PackageName Version
  -> GenericPackageDescription
  -> (Map PackageName (Map FlagName Bool), DepErrors)
selectPackageBuildPlan platform compiler pool gpd =
  (selectPlan . limitSearchSpace . NE.map makePlan) flagCombinations
 where
  selectPlan :: NonEmpty (a, DepErrors) -> (a, DepErrors)
  selectPlan = F.foldr1 fewerErrors
   where
    fewerErrors p1 p2
        | nErrors p1 == 0 = p1
        | nErrors p1 <= nErrors p2 = p1
        | otherwise = p2
     where
      nErrors = Map.size . snd

  -- Avoid exponential complexity in flag combinations making us sad pandas.
  -- See: https://github.com/commercialhaskell/stack/issues/543
  limitSearchSpace :: NonEmpty a -> NonEmpty a
  limitSearchSpace (x :| xs) = x :| take (maxFlagCombinations - 1) xs
   where
    maxFlagCombinations = 128

  makePlan ::
       [(FlagName, Bool)]
    -> (Map PackageName (Map FlagName Bool), DepErrors)
  makePlan flags =
    checkPackageBuildPlan platform compiler pool (Map.fromList flags) gpd

  flagCombinations :: NonEmpty [(FlagName, Bool)]
  flagCombinations = mapM getOptions (genPackageFlags gpd)
   where
    getOptions :: C.PackageFlag -> NonEmpty (FlagName, Bool)
    getOptions f
      | flagManual f = (fname, flagDefault f) :| []
      | flagDefault f = (fname, True) :| [(fname, False)]
      | otherwise = (fname, False) :| [(fname, True)]
     where
      fname = flagName f

-- | Check whether with the given set of flags a package's dependency
-- constraints can be satisfied against a given build plan or pool of packages.
checkPackageBuildPlan ::
     Platform
  -> ActualCompiler
  -> Map PackageName Version
  -> Map FlagName Bool
  -> GenericPackageDescription
  -> (Map PackageName (Map FlagName Bool), DepErrors)
checkPackageBuildPlan platform compiler pool flags gpd =
  (Map.singleton pkg flags, errs)
 where
  pkg = gpdPackageName gpd
  errs = checkPackageDeps pkg constraints pool
  constraints = gpdPackageDeps gpd compiler platform flags

-- | Checks if the given package dependencies can be satisfied by the given set
-- of packages. Will fail if a package is either missing or has a version
-- outside of the version range.
checkPackageDeps ::
     PackageName -- ^ package using dependencies, for constructing DepErrors
  -> Map PackageName VersionRange -- ^ dependency constraints
  -> Map PackageName Version -- ^ Available package pool or index
  -> DepErrors
checkPackageDeps myName deps packages =
  Map.unionsWith combineDepError $ map go $ Map.toList deps
 where
  go :: (PackageName, VersionRange) -> DepErrors
  go (name, range) =
    case Map.lookup name packages of
      Nothing -> Map.singleton name DepError
        { deVersion = Nothing
        , deNeededBy = Map.singleton myName range
        }
      Just v
        | withinRange v range -> Map.empty
        | otherwise -> Map.singleton name DepError
            { deVersion = Just v
            , deNeededBy = Map.singleton myName range
            }

type DepErrors = Map PackageName DepError

data DepError = DepError
  { deVersion :: !(Maybe Version)
  , deNeededBy :: !(Map PackageName VersionRange)
  }
  deriving Show

-- | Combine two 'DepError's for the same 'Version'.
combineDepError :: DepError -> DepError -> DepError
combineDepError (DepError a x) (DepError b y) =
  assert (a == b) $ DepError a (Map.unionWith C.intersectVersionRanges x y)

-- | Given a bundle of packages (a list of @GenericPackageDescriptions@'s) to
-- build and an available package pool (snapshot) check whether the bundle's
-- dependencies can be satisfied. If flags is passed as Nothing flag settings
-- will be chosen automatically.
checkBundleBuildPlan ::
     Platform
  -> ActualCompiler
  -> Map PackageName Version
  -> Maybe (Map PackageName (Map FlagName Bool))
  -> [GenericPackageDescription]
  -> (Map PackageName (Map FlagName Bool), DepErrors)
checkBundleBuildPlan platform compiler pool flags gpds =
  ( Map.unionsWith dupError (map fst plans)
  , Map.unionsWith combineDepError (map snd plans)
  )
 where
  plans = map (pkgPlan flags) gpds
  pkgPlan Nothing gpd =
    selectPackageBuildPlan platform compiler pool' gpd
  pkgPlan (Just f) gpd =
    checkPackageBuildPlan platform compiler pool' (flags' f gpd) gpd
  flags' f gpd = fromMaybe Map.empty (Map.lookup (gpdPackageName gpd) f)
  pool' = Map.union (gpdPackages gpds) pool

  dupError _ _ = impureThrow DuplicatePackagesBug

data BuildPlanCheck
  = BuildPlanCheckOk (Map PackageName (Map FlagName Bool))
  | BuildPlanCheckPartial (Map PackageName (Map FlagName Bool)) DepErrors
  | BuildPlanCheckFail
      (Map PackageName (Map FlagName Bool))
      DepErrors
      ActualCompiler

-- | Compare 'BuildPlanCheck', where GT means a better plan.
compareBuildPlanCheck :: BuildPlanCheck -> BuildPlanCheck -> Ordering
compareBuildPlanCheck
    (BuildPlanCheckPartial _ e1)
    (BuildPlanCheckPartial _ e2)
  =
    -- Note: order of comparison flipped, since it's better to have fewer errors.
    compare (Map.size e2) (Map.size e1)
compareBuildPlanCheck (BuildPlanCheckFail _ e1 _) (BuildPlanCheckFail _ e2 _) =
  let numUserPkgs e = Map.size $ Map.unions (Map.elems (fmap (.deNeededBy) e))
  in  compare (numUserPkgs e2) (numUserPkgs e1)
compareBuildPlanCheck BuildPlanCheckOk{}      BuildPlanCheckOk{}      = EQ
compareBuildPlanCheck BuildPlanCheckOk{}      BuildPlanCheckPartial{} = GT
compareBuildPlanCheck BuildPlanCheckOk{}      BuildPlanCheckFail{}    = GT
compareBuildPlanCheck BuildPlanCheckPartial{} BuildPlanCheckFail{}    = GT
compareBuildPlanCheck _                       _                       = LT

instance Show BuildPlanCheck where
  show BuildPlanCheckOk {} = ""
  show (BuildPlanCheckPartial f e)  = T.unpack $ showDepErrors f e
  show (BuildPlanCheckFail f e c) = T.unpack $ showCompilerErrors f e c

-- | Check a set of 'GenericPackageDescription's and a set of flags against a
-- given snapshot. Returns how well the snapshot satisfies the dependencies of
-- the packages.
checkSnapBuildPlan ::
     (HasConfig env, HasGHCVariant env)
  => [ResolvedPath Dir]
  -> Maybe (Map PackageName (Map FlagName Bool))
  -> SnapshotCandidate env
  -> RIO env BuildPlanCheck
checkSnapBuildPlan pkgDirs flags snapCandidate = do
  platform <- view platformL
  sma <- snapCandidate pkgDirs
  gpds <- liftIO $ forM (Map.elems sma.project) (.common.gpd)

  let compiler = sma.compiler
      globalVersion (GlobalPackageVersion v) = v
      depVersion dep
        | PLImmutable loc <- dep.location = Just $ packageLocationVersion loc
        | otherwise = Nothing
      snapPkgs = Map.union
        (Map.mapMaybe depVersion sma.deps)
        (Map.map globalVersion sma.global)
      (f, errs) = checkBundleBuildPlan platform compiler snapPkgs flags gpds
      cerrs = compilerErrors compiler errs

  if Map.null errs
    then pure $ BuildPlanCheckOk f
    else if Map.null cerrs
      then pure $ BuildPlanCheckPartial f errs
      else pure $ BuildPlanCheckFail f cerrs compiler
 where
  compilerErrors compiler errs
    | whichCompiler compiler == Ghc = ghcErrors errs
    | otherwise = Map.empty

  isGhcWiredIn p _ = p `Set.member` wiredInPackages
  ghcErrors = Map.filterWithKey isGhcWiredIn

-- | Find a snapshot and set of flags that is compatible with and matches as
-- best as possible with the given 'GenericPackageDescription's.
selectBestSnapshot ::
     (HasConfig env, HasGHCVariant env)
  => [ResolvedPath Dir]
  -> NonEmpty SnapName
  -> RIO env (SnapshotCandidate env, RawSnapshotLocation, BuildPlanCheck)
selectBestSnapshot pkgDirs snaps = do
  prettyInfo $
       fillSep
         [ flow "Selecting the best among"
         , fromString $ show (NE.length snaps)
         , "snapshots..."
         ]
    <> line
  F.foldr1 go (NE.map (getResult <=< snapshotLocation) snaps)
 where
  go mold mnew = do
    old@(_snap, _loc, bpc) <- mold
    case bpc of
      BuildPlanCheckOk {} -> pure old
      _ -> fmap (betterSnap old) mnew

  getResult loc = do
    candidate <- loadProjectSnapshotCandidate loc NoPrintWarnings False
    result <- checkSnapBuildPlan pkgDirs Nothing candidate
    reportResult result loc
    pure (candidate, loc, result)

  betterSnap (s1, l1, r1) (s2, l2, r2)
    | compareBuildPlanCheck r1 r2 /= LT = (s1, l1, r1)
    | otherwise = (s2, l2, r2)

  reportResult BuildPlanCheckOk {} loc =
    prettyNote $
         fillSep
           [ flow "Matches"
           , pretty $ PrettyRawSnapshotLocation loc
           ]
      <> line

  reportResult r@BuildPlanCheckPartial {} loc =
    prettyWarn $
         fillSep
           [ flow "Partially matches"
           , pretty $ PrettyRawSnapshotLocation loc
           ]
      <> blankLine
      <> indent 4 (string (show r))

  reportResult r@BuildPlanCheckFail {} loc =
    prettyWarn $
         fillSep
           [ flow "Rejected"
           , pretty $ PrettyRawSnapshotLocation loc
           ]
      <> blankLine
      <> indent 4 (string (show r))

showItems :: [String] -> Text
showItems items = T.concat (map formatItem items)
 where
  formatItem item = T.concat
    [ "    - "
    , T.pack item
    , "\n"
    ]

showPackageFlags :: PackageName -> Map FlagName Bool -> Text
showPackageFlags pkg fl =
  if not $ Map.null fl
    then
      T.concat
        [ "    - "
        , T.pack $ packageNameString pkg
        , ": "
        , T.pack $ intercalate ", "
                 $ map formatFlags (Map.toList fl)
        , "\n"
        ]
    else ""
 where
  formatFlags (f, v) = show f ++ " = " ++ show v

showMapPackages :: Map PackageName a -> Text
showMapPackages mp = showItems $ map packageNameString $ Map.keys mp

showCompilerErrors ::
     Map PackageName (Map FlagName Bool)
  -> DepErrors
  -> ActualCompiler
  -> Text
showCompilerErrors flags errs compiler =
  T.concat
    [ compilerVersionText compiler
    , " cannot be used for these packages:\n"
    , showMapPackages $ Map.unions (Map.elems (fmap (.deNeededBy) errs))
    , showDepErrors flags errs -- TODO only in debug mode
    ]

showDepErrors :: Map PackageName (Map FlagName Bool) -> DepErrors -> Text
showDepErrors flags errs =
  T.concat
    [ T.concat $ map formatError (Map.toList errs)
    , if T.null flagVals then ""
      else "Using package flags:\n" <> flagVals
    ]
 where
  formatError (depName, DepError mversion neededBy) = T.concat
    [ showDepVersion depName mversion
    , T.concat (map showRequirement (Map.toList neededBy))
    ]

  showDepVersion depName mversion = T.concat
    [ T.pack $ packageNameString depName
    , case mversion of
        Nothing -> " not found"
        Just version -> T.concat
            [ " version "
            , T.pack $ versionString version
            , " found"
            ]
    , "\n"
    ]

  showRequirement (user, range) = T.concat
    [ "    - "
    , T.pack $ packageNameString user
    , " requires "
    , T.pack $ display range
    , "\n"
    ]

  flagVals = T.concat (map showFlags userPkgs)
  userPkgs = Map.keys $ Map.unions (Map.elems (fmap (.deNeededBy) errs))
  showFlags pkg = maybe "" (showPackageFlags pkg) (Map.lookup pkg flags)
