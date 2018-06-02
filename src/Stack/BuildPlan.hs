{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Resolving a build plan for a set of packages in a given Stackage
-- snapshot.

module Stack.BuildPlan
    ( BuildPlanException (..)
    , BuildPlanCheck (..)
    , checkSnapBuildPlan
    , DepError(..)
    , DepErrors
    , gpdPackageDeps
    , gpdPackages
    , removeSrcPkgDefaultFlags
    , selectBestSnapshot
    , getToolMap
    , showItems
    ) where

import           Stack.Prelude hiding (Display (..))
import qualified Data.Foldable as F
import qualified Data.HashSet as HashSet
import           Data.List (intercalate)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Distribution.Package as C
import           Distribution.PackageDescription (GenericPackageDescription,
                                                  flagDefault, flagManual,
                                                  flagName, genPackageFlags,
                                                  condExecutables)
import qualified Distribution.PackageDescription as C
import qualified Distribution.Types.UnqualComponentName as C
import           Distribution.System (Platform)
import           Distribution.Text (display)
import qualified Distribution.Version as C
import qualified RIO
import           Stack.Constants
import           Stack.Package
import           Stack.Snapshot
import           Stack.Types.BuildPlan
import           Stack.Types.FlagName
import           Stack.Types.NamedComponent
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version
import           Stack.Types.Config
import           Stack.Types.Compiler
import           Stack.Types.Resolver

data BuildPlanException
    = UnknownPackages
        (Path Abs File) -- stack.yaml file
        (Map PackageName (Maybe Version, Set PackageName)) -- truly unknown
        (Map PackageName (Set PackageIdentifier)) -- shadowed
    | SnapshotNotFound SnapName
    | NeitherCompilerOrResolverSpecified T.Text
    deriving (Typeable)
instance Exception BuildPlanException
instance Show BuildPlanException where
    show (SnapshotNotFound snapName) = unlines
        [ "SnapshotNotFound " ++ snapName'
        , "Non existing resolver: " ++ snapName' ++ "."
        , "For a complete list of available snapshots see https://www.stackage.org/snapshots"
        ]
        where snapName' = show $ renderSnapName snapName
    show (UnknownPackages stackYaml unknown shadowed) =
        unlines $ unknown' ++ shadowed'
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
            go (dep, users) | Set.null users = packageNameString dep ++ " (internal stack error: this should never be null)"
            go (dep, users) = concat
                [ packageNameString dep
                , " (used by "
                , intercalate ", "
                    $ map (packageNameString . packageIdentifierName)
                    $ Set.toList users
                , ")"
                ]

            extraDeps = map (\ident -> "- " ++ packageIdentifierString ident)
                      $ Set.toList
                      $ Set.unions
                      $ Map.elems shadowed
    show (NeitherCompilerOrResolverSpecified url) =
        "Failed to load custom snapshot at " ++
        T.unpack url ++
        ", because no 'compiler' or 'resolver' is specified."

-- | Map from tool name to package providing it. This accounts for
-- both snapshot and local packages (deps and project packages).
getToolMap :: LoadedSnapshot
           -> LocalPackages
           -> Map ExeName (Set PackageName)
getToolMap ls locals =

    {- We no longer do this, following discussion at:

        https://github.com/commercialhaskell/stack/issues/308#issuecomment-112076704

    -- First grab all of the package names, for times where a build tool is
    -- identified by package name
    $ Map.fromList (map (packageNameByteString &&& Set.singleton) (Map.keys ps))
    -}

    Map.unionsWith Set.union $ concat
        [ concatMap goSnap      $ Map.toList $ lsPackages ls
        , concatMap goLocalProj $ Map.toList $ lpProject locals
        , concatMap goLocalDep  $ Map.toList $ lpDependencies locals
        ]
  where
    goSnap (pname, lpi) =
        map (flip Map.singleton (Set.singleton pname))
      $ Set.toList
      $ lpiProvidedExes lpi

    goLocalProj (pname, lpv) =
        map (flip Map.singleton (Set.singleton pname))
        [ExeName t | CExe t <- Set.toList (lpvComponents lpv)]

    goLocalDep (pname, (gpd, _loc)) =
        map (flip Map.singleton (Set.singleton pname))
      $ gpdExes gpd

    -- TODO consider doing buildable checking. Not a big deal though:
    -- worse case scenario is we build an extra package that wasn't
    -- strictly needed.
    gpdExes :: GenericPackageDescription -> [ExeName]
    gpdExes = map (ExeName . T.pack . C.unUnqualComponentName . fst) . condExecutables

gpdPackages :: [GenericPackageDescription] -> Map PackageName Version
gpdPackages gpds = Map.fromList $
            map (fromCabalIdent . C.package . C.packageDescription) gpds
    where
        fromCabalIdent (C.PackageIdentifier name version) =
            (fromCabalPackageName name, fromCabalVersion version)

gpdPackageDeps
    :: GenericPackageDescription
    -> CompilerVersion 'CVActual
    -> Platform
    -> Map FlagName Bool
    -> Map PackageName VersionRange
gpdPackageDeps gpd cv platform flags =
    Map.filterWithKey (const . (/= name)) (packageDependencies pkgConfig pkgDesc)
    where
        name = gpdPackageName gpd
        -- Since tests and benchmarks are both enabled, doesn't matter
        -- if we choose modified or unmodified
        pkgDesc = pdpModifiedBuildable $ resolvePackageDescription pkgConfig gpd
        pkgConfig = PackageConfig
            { packageConfigEnableTests = True
            , packageConfigEnableBenchmarks = True
            , packageConfigFlags = flags
            , packageConfigGhcOptions = []
            , packageConfigCompilerVersion = cv
            , packageConfigPlatform = platform
            }

-- Remove any src package flags having default values
-- Remove any package entries with no flags set
removeSrcPkgDefaultFlags :: [C.GenericPackageDescription]
                         -> Map PackageName (Map FlagName Bool)
                         -> Map PackageName (Map FlagName Bool)
removeSrcPkgDefaultFlags gpds flags =
    let defaults = Map.unions (map gpdDefaultFlags gpds)
        flags'   = Map.differenceWith removeSame flags defaults
    in  Map.filter (not . Map.null) flags'
    where
        removeSame f1 f2 =
            let diff v v' = if v == v' then Nothing else Just v
            in Just $ Map.differenceWith diff f1 f2

        gpdDefaultFlags gpd =
            let tuples = map getDefault (C.genPackageFlags gpd)
            in Map.singleton (gpdPackageName gpd) (Map.fromList tuples)

        flagName' = fromCabalFlagName . C.flagName
        getDefault f
            | C.flagDefault f = (flagName' f, True)
            | otherwise       = (flagName' f, False)

-- | Find the set of @FlagName@s necessary to get the given
-- @GenericPackageDescription@ to compile against the given @BuildPlan@. Will
-- only modify non-manual flags, and will prefer default values for flags.
-- Returns the plan which produces least number of dep errors
selectPackageBuildPlan
    :: Platform
    -> CompilerVersion 'CVActual
    -> Map PackageName Version
    -> GenericPackageDescription
    -> (Map PackageName (Map FlagName Bool), DepErrors)
selectPackageBuildPlan platform compiler pool gpd =
    (selectPlan . limitSearchSpace . NonEmpty.map makePlan) flagCombinations
  where
    selectPlan :: NonEmpty (a, DepErrors) -> (a, DepErrors)
    selectPlan = F.foldr1 fewerErrors
      where
        fewerErrors p1 p2
            | nErrors p1 == 0 = p1
            | nErrors p1 <= nErrors p2 = p1
            | otherwise = p2
          where nErrors = Map.size . snd

    -- Avoid exponential complexity in flag combinations making us sad pandas.
    -- See: https://github.com/commercialhaskell/stack/issues/543
    limitSearchSpace :: NonEmpty a -> NonEmpty a
    limitSearchSpace (x :| xs) = x :| take (maxFlagCombinations - 1) xs
      where maxFlagCombinations = 128

    makePlan :: [(FlagName, Bool)] -> (Map PackageName (Map FlagName Bool), DepErrors)
    makePlan flags = checkPackageBuildPlan platform compiler pool (Map.fromList flags) gpd

    flagCombinations :: NonEmpty [(FlagName, Bool)]
    flagCombinations = mapM getOptions (genPackageFlags gpd)
      where
        getOptions :: C.Flag -> NonEmpty (FlagName, Bool)
        getOptions f
            | flagManual f = (fname, flagDefault f) :| []
            | flagDefault f = (fname, True) :| [(fname, False)]
            | otherwise = (fname, False) :| [(fname, True)]
          where fname = (fromCabalFlagName . flagName) f

-- | Check whether with the given set of flags a package's dependency
-- constraints can be satisfied against a given build plan or pool of packages.
checkPackageBuildPlan
    :: Platform
    -> CompilerVersion 'CVActual
    -> Map PackageName Version
    -> Map FlagName Bool
    -> GenericPackageDescription
    -> (Map PackageName (Map FlagName Bool), DepErrors)
checkPackageBuildPlan platform compiler pool flags gpd =
    (Map.singleton pkg flags, errs)
    where
        pkg         = gpdPackageName gpd
        errs        = checkPackageDeps pkg constraints pool
        constraints = gpdPackageDeps gpd compiler platform flags

-- | Checks if the given package dependencies can be satisfied by the given set
-- of packages. Will fail if a package is either missing or has a version
-- outside of the version range.
checkPackageDeps
    :: PackageName -- ^ package using dependencies, for constructing DepErrors
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
    } deriving Show

-- | Combine two 'DepError's for the same 'Version'.
combineDepError :: DepError -> DepError -> DepError
combineDepError (DepError a x) (DepError b y) =
    assert (a == b) $ DepError a (Map.unionWith C.intersectVersionRanges x y)

-- | Given a bundle of packages (a list of @GenericPackageDescriptions@'s) to
-- build and an available package pool (snapshot) check whether the bundle's
-- dependencies can be satisfied. If flags is passed as Nothing flag settings
-- will be chosen automatically.
checkBundleBuildPlan
    :: Platform
    -> CompilerVersion 'CVActual
    -> Map PackageName Version
    -> Maybe (Map PackageName (Map FlagName Bool))
    -> [GenericPackageDescription]
    -> (Map PackageName (Map FlagName Bool), DepErrors)
checkBundleBuildPlan platform compiler pool flags gpds =
    (Map.unionsWith dupError (map fst plans)
    , Map.unionsWith combineDepError (map snd plans))

    where
        plans = map (pkgPlan flags) gpds
        pkgPlan Nothing gpd =
            selectPackageBuildPlan platform compiler pool' gpd
        pkgPlan (Just f) gpd =
            checkPackageBuildPlan platform compiler pool' (flags' f gpd) gpd
        flags' f gpd = fromMaybe Map.empty (Map.lookup (gpdPackageName gpd) f)
        pool' = Map.union (gpdPackages gpds) pool

        dupError _ _ = error "Bug: Duplicate packages are not expected here"

data BuildPlanCheck =
      BuildPlanCheckOk      (Map PackageName (Map FlagName Bool))
    | BuildPlanCheckPartial (Map PackageName (Map FlagName Bool)) DepErrors
    | BuildPlanCheckFail    (Map PackageName (Map FlagName Bool)) DepErrors
                            (CompilerVersion 'CVActual)

-- | Compare 'BuildPlanCheck', where GT means a better plan.
compareBuildPlanCheck :: BuildPlanCheck -> BuildPlanCheck -> Ordering
compareBuildPlanCheck (BuildPlanCheckPartial _ e1) (BuildPlanCheckPartial _ e2) =
    -- Note: order of comparison flipped, since it's better to have fewer errors.
    compare (Map.size e2) (Map.size e1)
compareBuildPlanCheck (BuildPlanCheckFail _ e1 _) (BuildPlanCheckFail _ e2 _) =
    let numUserPkgs e = Map.size $ Map.unions (Map.elems (fmap deNeededBy e))
    in compare (numUserPkgs e2) (numUserPkgs e1)
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
checkSnapBuildPlan
    :: (HasConfig env, HasGHCVariant env)
    => Path Abs Dir -- ^ project root, used for checking out necessary files
    -> [GenericPackageDescription]
    -> Maybe (Map PackageName (Map FlagName Bool))
    -> SnapshotDef
    -> Maybe (CompilerVersion 'CVActual)
    -> RIO env BuildPlanCheck
checkSnapBuildPlan root gpds flags snapshotDef mactualCompiler = do
    platform <- view platformL
    rs <- loadSnapshot mactualCompiler root snapshotDef

    let
        compiler = lsCompilerVersion rs
        snapPkgs = Map.union
          (lpiVersion <$> lsGlobals rs)
          (lpiVersion <$> lsPackages rs)
        (f, errs) = checkBundleBuildPlan platform compiler snapPkgs flags gpds
        cerrs = compilerErrors compiler errs

    if Map.null errs then
        return $ BuildPlanCheckOk f
    else if Map.null cerrs then do
            return $ BuildPlanCheckPartial f errs
        else
            return $ BuildPlanCheckFail f cerrs compiler
    where
        compilerErrors compiler errs
            | whichCompiler compiler == Ghc = ghcErrors errs
            -- FIXME not sure how to handle ghcjs boot packages
            | otherwise = Map.empty

        isGhcWiredIn p _ = p `HashSet.member` wiredInPackages
        ghcErrors = Map.filterWithKey isGhcWiredIn

-- | Find a snapshot and set of flags that is compatible with and matches as
-- best as possible with the given 'GenericPackageDescription's.
selectBestSnapshot
    :: (HasConfig env, HasGHCVariant env)
    => Path Abs Dir -- ^ project root, used for checking out necessary files
    -> [GenericPackageDescription]
    -> NonEmpty SnapName
    -> RIO env (SnapshotDef, BuildPlanCheck)
selectBestSnapshot root gpds snaps = do
    logInfo $ "Selecting the best among "
               <> displayShow (NonEmpty.length snaps)
               <> " snapshots...\n"
    F.foldr1 go (NonEmpty.map (getResult <=< loadResolver . ResolverStackage) snaps)
    where
        go mold mnew = do
            old@(_snap, bpc) <- mold
            case bpc of
                BuildPlanCheckOk {} -> return old
                _ -> fmap (betterSnap old) mnew

        getResult snap = do
            result <- checkSnapBuildPlan root gpds Nothing snap
              -- We know that we're only dealing with ResolverStackage
              -- here, where we can rely on the global package hints.
              -- Therefore, we don't use an actual compiler. For more
              -- info, see comments on
              -- Stack.Solver.checkSnapBuildPlanActual.
              Nothing
            reportResult result snap
            return (snap, result)

        betterSnap (s1, r1) (s2, r2)
          | compareBuildPlanCheck r1 r2 /= LT = (s1, r1)
          | otherwise = (s2, r2)

        reportResult BuildPlanCheckOk {} snap = do
            logInfo $ "* Matches " <> RIO.display (sdResolverName snap)
            logInfo ""

        reportResult r@BuildPlanCheckPartial {} snap = do
            logWarn $ "* Partially matches " <> RIO.display (sdResolverName snap)
            logWarn $ RIO.display $ indent $ T.pack $ show r

        reportResult r@BuildPlanCheckFail {} snap = do
            logWarn $ "* Rejected " <> RIO.display (sdResolverName snap)
            logWarn $ RIO.display $ indent $ T.pack $ show r

        indent t = T.unlines $ fmap ("    " <>) (T.lines t)

showItems :: Show a => [a] -> Text
showItems items = T.concat (map formatItem items)
    where
        formatItem item = T.concat
            [ "    - "
            , T.pack $ show item
            , "\n"
            ]

showPackageFlags :: PackageName -> Map FlagName Bool -> Text
showPackageFlags pkg fl =
    if not $ Map.null fl then
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
showMapPackages mp = showItems $ Map.keys mp

showCompilerErrors
    :: Map PackageName (Map FlagName Bool)
    -> DepErrors
    -> CompilerVersion 'CVActual
    -> Text
showCompilerErrors flags errs compiler =
    T.concat
        [ compilerVersionText compiler
        , " cannot be used for these packages:\n"
        , showMapPackages $ Map.unions (Map.elems (fmap deNeededBy errs))
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
        userPkgs = Map.keys $ Map.unions (Map.elems (fmap deNeededBy errs))
        showFlags pkg = maybe "" (showPackageFlags pkg) (Map.lookup pkg flags)
