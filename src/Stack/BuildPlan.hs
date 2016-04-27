{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}

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
    , gpdPackageName
    , MiniBuildPlan(..)
    , MiniPackageInfo(..)
    , loadMiniBuildPlan
    , removeSrcPkgDefaultFlags
    , resolveBuildPlan
    , selectBestSnapshot
    , getToolMap
    , shadowMiniBuildPlan
    , showItems
    , showPackageFlags
    , parseCustomMiniBuildPlan
    ) where

import           Control.Applicative
import           Control.Exception (assert)
import           Control.Monad (liftM, forM)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (asks)
import           Control.Monad.State.Strict      (State, execState, get, modify,
                                                  put)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.Aeson.Extended (FromJSON (..), withObject, (.:), (.:?), (.!=))
import           Data.Binary.VersionTagged (taggedDecodeOrLoad)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as S8
import           Data.Either (partitionEithers)
import qualified Data.Foldable as F
import qualified Data.HashSet as HashSet
import           Data.List (intercalate)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Traversable as Tr
import           Data.Typeable (Typeable)
import           Data.Yaml (decodeEither', decodeFileEither)
import qualified Distribution.Package as C
import           Distribution.PackageDescription (GenericPackageDescription,
                                                  flagDefault, flagManual,
                                                  flagName, genPackageFlags,
                                                  executables, exeName, library, libBuildInfo, buildable)
import qualified Distribution.PackageDescription as C
import           Distribution.System (Platform)
import           Distribution.Text (display)
import qualified Distribution.Version as C
import           Network.HTTP.Client (checkStatus)
import           Network.HTTP.Download
import           Network.HTTP.Types (Status(..))
import           Path
import           Path.IO
import           Prelude -- Fix AMP warning
import           Stack.Constants
import           Stack.Fetch
import           Stack.Package
import           Stack.PackageIndex
import           Stack.Types
import           Stack.Types.StackT
import qualified System.Directory as D
import qualified System.FilePath as FP

data BuildPlanException
    = UnknownPackages
        (Path Abs File) -- stack.yaml file
        (Map PackageName (Maybe Version, Set PackageName)) -- truly unknown
        (Map PackageName (Set PackageIdentifier)) -- shadowed
    | SnapshotNotFound SnapName
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
            go (dep, users) | Set.null users = concat
                [ packageNameString dep
                , " (internal stack error: this should never be null)"
                ]
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

-- | Determine the necessary packages to install to have the given set of
-- packages available.
--
-- This function will not provide test suite and benchmark dependencies.
--
-- This may fail if a target package is not present in the @BuildPlan@.
resolveBuildPlan :: (MonadThrow m, MonadIO m, MonadReader env m, HasBuildConfig env, MonadLogger m, HasHttpManager env, MonadBaseControl IO m,MonadCatch m)
                 => MiniBuildPlan
                 -> (PackageName -> Bool) -- ^ is it shadowed by a local package?
                 -> Map PackageName (Set PackageName) -- ^ required packages, and users of it
                 -> m ( Map PackageName (Version, Map FlagName Bool)
                      , Map PackageName (Set PackageName)
                      )
resolveBuildPlan mbp isShadowed packages
    | Map.null (rsUnknown rs) && Map.null (rsShadowed rs) = return (rsToInstall rs, rsUsedBy rs)
    | otherwise = do
        bconfig <- asks getBuildConfig
        caches <- getPackageCaches
        let maxVer =
                Map.fromListWith max $
                map toTuple $
                Map.keys caches
            unknown = flip Map.mapWithKey (rsUnknown rs) $ \ident x ->
                (Map.lookup ident maxVer, x)
        throwM $ UnknownPackages
            (bcStackYaml bconfig)
            unknown
            (rsShadowed rs)
  where
    rs = getDeps mbp isShadowed packages

data ResolveState = ResolveState
    { rsVisited   :: Map PackageName (Set PackageName) -- ^ set of shadowed dependencies
    , rsUnknown   :: Map PackageName (Set PackageName)
    , rsShadowed  :: Map PackageName (Set PackageIdentifier)
    , rsToInstall :: Map PackageName (Version, Map FlagName Bool)
    , rsUsedBy    :: Map PackageName (Set PackageName)
    }

toMiniBuildPlan :: (MonadIO m, MonadLogger m, MonadReader env m, HasHttpManager env, MonadThrow m, HasConfig env, MonadBaseControl IO m, MonadCatch m)
                => CompilerVersion -- ^ Compiler version
                -> Map PackageName Version -- ^ cores
                -> Map PackageName (Version, Map FlagName Bool) -- ^ non-core packages
                -> m MiniBuildPlan
toMiniBuildPlan compilerVersion corePackages packages = do
    $logInfo "Caching build plan"

    -- Determine the dependencies of all of the packages in the build plan. We
    -- handle core packages specially, because some of them will not be in the
    -- package index. For those, we allow missing packages to exist, and then
    -- remove those from the list of dependencies, since there's no way we'll
    -- ever reinstall them anyway.
    (cores, missingCores) <- addDeps True compilerVersion
        $ fmap (, Map.empty) corePackages

    (extras, missing) <- addDeps False compilerVersion packages

    assert (Set.null missing) $ return MiniBuildPlan
        { mbpCompilerVersion = compilerVersion
        , mbpPackages = Map.unions
            [ fmap (removeMissingDeps (Map.keysSet cores)) cores
            , extras
            , Map.fromList $ map goCore $ Set.toList missingCores
            ]
        }
  where
    goCore (PackageIdentifier name version) = (name, MiniPackageInfo
                { mpiVersion = version
                , mpiFlags = Map.empty
                , mpiPackageDeps = Set.empty
                , mpiToolDeps = Set.empty
                , mpiExes = Set.empty
                , mpiHasLibrary = True
                })

    removeMissingDeps cores mpi = mpi
        { mpiPackageDeps = Set.intersection cores (mpiPackageDeps mpi)
        }

-- | Add in the resolved dependencies from the package index
addDeps :: (MonadIO m, MonadLogger m, MonadReader env m, HasHttpManager env, MonadThrow m, HasConfig env, MonadBaseControl IO m, MonadCatch m)
        => Bool -- ^ allow missing
        -> CompilerVersion -- ^ Compiler version
        -> Map PackageName (Version, Map FlagName Bool)
        -> m (Map PackageName MiniPackageInfo, Set PackageIdentifier)
addDeps allowMissing compilerVersion toCalc = do
    menv <- getMinimalEnvOverride
    platform <- asks $ configPlatform . getConfig
    (resolvedMap, missingIdents) <-
        if allowMissing
            then do
                (missingNames, missingIdents, m) <-
                    resolvePackagesAllowMissing (Map.keysSet idents0) Set.empty
                assert (Set.null missingNames)
                    $ return (m, missingIdents)
            else do
                m <- resolvePackages menv (Map.keysSet idents0) Set.empty
                return (m, Set.empty)
    let byIndex = Map.fromListWith (++) $ flip map (Map.toList resolvedMap)
            $ \(ident, rp) ->
                (indexName $ rpIndex rp,
                    [( ident
                    , rpCache rp
                    , maybe Map.empty snd $ Map.lookup (packageIdentifierName ident) toCalc
                    )])
    res <- forM (Map.toList byIndex) $ \(indexName', pkgs) -> withCabalFiles indexName' pkgs
        $ \ident flags cabalBS -> do
            (_warnings,gpd) <- readPackageUnresolvedBS Nothing cabalBS
            let packageConfig = PackageConfig
                    { packageConfigEnableTests = False
                    , packageConfigEnableBenchmarks = False
                    , packageConfigFlags = flags
                    , packageConfigCompilerVersion = compilerVersion
                    , packageConfigPlatform = platform
                    }
                name = packageIdentifierName ident
                pd = resolvePackageDescription packageConfig gpd
                exes = Set.fromList $ map (ExeName . T.pack . exeName) $ executables pd
                notMe = Set.filter (/= name) . Map.keysSet
            return (name, MiniPackageInfo
                { mpiVersion = packageIdentifierVersion ident
                , mpiFlags = flags
                , mpiPackageDeps = notMe $ packageDependencies pd
                , mpiToolDeps = Map.keysSet $ packageToolDependencies pd
                , mpiExes = exes
                , mpiHasLibrary = maybe
                    False
                    (buildable . libBuildInfo)
                    (library pd)
                })
    return (Map.fromList $ concat res, missingIdents)
  where
    idents0 = Map.fromList
        $ map (\(n, (v, f)) -> (PackageIdentifier n v, Left f))
        $ Map.toList toCalc

-- | Resolve all packages necessary to install for the needed packages.
getDeps :: MiniBuildPlan
        -> (PackageName -> Bool) -- ^ is it shadowed by a local package?
        -> Map PackageName (Set PackageName)
        -> ResolveState
getDeps mbp isShadowed packages =
    execState (mapM_ (uncurry goName) $ Map.toList packages) ResolveState
        { rsVisited = Map.empty
        , rsUnknown = Map.empty
        , rsShadowed = Map.empty
        , rsToInstall = Map.empty
        , rsUsedBy = Map.empty
        }
  where
    toolMap = getToolMap mbp

    -- | Returns a set of shadowed packages we depend on.
    goName :: PackageName -> Set PackageName -> State ResolveState (Set PackageName)
    goName name users = do
        -- Even though we could check rsVisited first and short-circuit things
        -- earlier, lookup in mbpPackages first so that we can produce more
        -- usable error information on missing dependencies
        rs <- get
        put rs
            { rsUsedBy = Map.insertWith Set.union name users $ rsUsedBy rs
            }
        case Map.lookup name $ mbpPackages mbp of
            Nothing -> do
                modify $ \rs' -> rs'
                    { rsUnknown = Map.insertWith Set.union name users $ rsUnknown rs'
                    }
                return Set.empty
            Just mpi -> case Map.lookup name (rsVisited rs) of
              Just shadowed -> return shadowed
              Nothing -> do
                put rs { rsVisited = Map.insert name Set.empty $ rsVisited rs }
                let depsForTools = Set.unions $ mapMaybe (flip Map.lookup toolMap) (Set.toList $ mpiToolDeps mpi)
                let deps = Set.filter (/= name) (mpiPackageDeps mpi <> depsForTools)
                shadowed <- fmap F.fold $ Tr.forM (Set.toList deps) $ \dep ->
                    if isShadowed dep
                        then do
                            modify $ \rs' -> rs'
                                { rsShadowed = Map.insertWith
                                    Set.union
                                    dep
                                    (Set.singleton $ PackageIdentifier name (mpiVersion mpi))
                                    (rsShadowed rs')
                                }
                            return $ Set.singleton dep
                        else do
                            shadowed <- goName dep (Set.singleton name)
                            let m = Map.fromSet (\_ -> Set.singleton $ PackageIdentifier name (mpiVersion mpi)) shadowed
                            modify $ \rs' -> rs'
                                { rsShadowed = Map.unionWith Set.union m $ rsShadowed rs'
                                }
                            return shadowed
                modify $ \rs' -> rs'
                    { rsToInstall = Map.insert name (mpiVersion mpi, mpiFlags mpi) $ rsToInstall rs'
                    , rsVisited = Map.insert name shadowed $ rsVisited rs'
                    }
                return shadowed

-- | Map from tool name to package providing it
getToolMap :: MiniBuildPlan -> Map Text (Set PackageName)
getToolMap mbp =
      Map.unionsWith Set.union

    {- We no longer do this, following discussion at:

        https://github.com/commercialhaskell/stack/issues/308#issuecomment-112076704

    -- First grab all of the package names, for times where a build tool is
    -- identified by package name
    $ Map.fromList (map (packageNameByteString &&& Set.singleton) (Map.keys ps))
    -}

    -- And then get all of the explicit executable names
    $ concatMap goPair (Map.toList ps)
  where
    ps = mbpPackages mbp

    goPair (pname, mpi) =
        map (flip Map.singleton (Set.singleton pname) . unExeName)
      $ Set.toList
      $ mpiExes mpi

-- | Load up a 'MiniBuildPlan', preferably from cache
loadMiniBuildPlan
    :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader env m, HasHttpManager env, HasConfig env, HasGHCVariant env, MonadBaseControl IO m, MonadCatch m)
    => SnapName
    -> m MiniBuildPlan
loadMiniBuildPlan name = do
    path <- configMiniBuildPlanCache name
    taggedDecodeOrLoad path $ liftM buildPlanFixes $ do
        bp <- loadBuildPlan name
        toMiniBuildPlan
            (siCompilerVersion $ bpSystemInfo bp)
            (siCorePackages $ bpSystemInfo bp)
            (fmap goPP $ bpPackages bp)
  where
    goPP pp =
        ( ppVersion pp
        , pcFlagOverrides $ ppConstraints pp
        )

-- | Some hard-coded fixes for build plans, hopefully to be irrelevant over
-- time.
buildPlanFixes :: MiniBuildPlan -> MiniBuildPlan
buildPlanFixes mbp = mbp
    { mbpPackages = Map.fromList $ map go $ Map.toList $ mbpPackages mbp
    }
  where
    go (name, mpi) =
        (name, mpi
            { mpiFlags = goF (packageNameString name) (mpiFlags mpi)
            })

    goF "persistent-sqlite" = Map.insert $(mkFlagName "systemlib") False
    goF "yaml" = Map.insert $(mkFlagName "system-libyaml") False
    goF _ = id

-- | Load the 'BuildPlan' for the given snapshot. Will load from a local copy
-- if available, otherwise downloading from Github.
loadBuildPlan :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader env m, HasHttpManager env, HasStackRoot env, HasConfig env)
              => SnapName
              -> m BuildPlan
loadBuildPlan name = do
    env <- ask
    let stackage = getStackRoot env
    file' <- parseRelFile $ T.unpack file
    let fp = buildPlanDir stackage </> file'
    $logDebug $ "Decoding build plan from: " <> T.pack (toFilePath fp)
    eres <- liftIO $ decodeFileEither $ toFilePath fp
    case eres of
        Right bp -> return bp
        Left e -> do
            $logDebug $ "Decoding build plan from file failed: " <> T.pack (show e)
            ensureDir (parent fp)
            url <- buildBuildPlanUrl name file
            req <- parseUrl $ T.unpack url
            $logSticky $ "Downloading " <> renderSnapName name <> " build plan ..."
            $logDebug $ "Downloading build plan from: " <> url
            _ <- redownload req { checkStatus = handle404 } fp
            $logStickyDone $ "Downloaded " <> renderSnapName name <> " build plan."
            liftIO (decodeFileEither $ toFilePath fp) >>= either throwM return

  where
    file = renderSnapName name <> ".yaml"
    handle404 (Status 404 _) _ _ = Just $ SomeException $ SnapshotNotFound name
    handle404 _ _ _              = Nothing

buildBuildPlanUrl :: (MonadReader env m, HasConfig env) => SnapName -> Text -> m Text
buildBuildPlanUrl name file = do
    urls <- asks (configUrls . getConfig)
    return $
        case name of
             LTS _ _ -> urlsLtsBuildPlans urls <> "/" <> file
             Nightly _ -> urlsNightlyBuildPlans urls <> "/" <> file

gpdPackages :: [GenericPackageDescription] -> Map PackageName Version
gpdPackages gpds = Map.fromList $
            map (fromCabalIdent . C.package . C.packageDescription) gpds
    where
        fromCabalIdent (C.PackageIdentifier name version) =
            (fromCabalPackageName name, fromCabalVersion version)

gpdPackageName :: GenericPackageDescription -> PackageName
gpdPackageName = fromCabalPackageName
    . C.pkgName
    . C.package
    . C.packageDescription

gpdPackageDeps
    :: GenericPackageDescription
    -> CompilerVersion
    -> Platform
    -> Map FlagName Bool
    -> Map PackageName VersionRange
gpdPackageDeps gpd cv platform flags =
    Map.filterWithKey (const . (/= name)) (packageDependencies pkgDesc)
    where
        name = gpdPackageName gpd
        pkgDesc = resolvePackageDescription pkgConfig gpd
        pkgConfig = PackageConfig
            { packageConfigEnableTests = True
            , packageConfigEnableBenchmarks = True
            , packageConfigFlags = flags
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
    -> CompilerVersion
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
    -> CompilerVersion
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
checkPackageDeps :: PackageName -- ^ package using dependencies, for constructing DepErrors
          -> Map PackageName VersionRange -- ^ dependency constraints
          -> Map PackageName Version -- ^ Available package pool or index
          -> DepErrors
checkPackageDeps myName deps packages =
    Map.unionsWith mappend $ map go $ Map.toList deps
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
instance Monoid DepError where
    mempty = DepError Nothing Map.empty
    mappend (DepError a x) (DepError b y) = DepError
        (maybe a Just b)
        (Map.unionWith C.intersectVersionRanges x y)

-- | Given a bundle of packages (a list of @GenericPackageDescriptions@'s) to
-- build and an available package pool (snapshot) check whether the bundle's
-- dependencies can be satisfied. If flags is passed as Nothing flag settings
-- will be chosen automatically.
checkBundleBuildPlan
    :: Platform
    -> CompilerVersion
    -> Map PackageName Version
    -> Maybe (Map PackageName (Map FlagName Bool))
    -> [GenericPackageDescription]
    -> (Map PackageName (Map FlagName Bool), DepErrors)
checkBundleBuildPlan platform compiler pool flags gpds =
    (Map.unionsWith dupError (map fst plans)
    , Map.unionsWith mappend (map snd plans))

    where
        plans = map (pkgPlan flags) gpds
        pkgPlan Nothing gpd =
            selectPackageBuildPlan platform compiler pool' gpd
        pkgPlan (Just f) gpd =
            checkPackageBuildPlan platform compiler pool' (flags' f gpd) gpd
        flags' f gpd = maybe Map.empty id (Map.lookup (gpdPackageName gpd) f)
        pool' = Map.union (gpdPackages gpds) pool

        dupError _ _ = error "Bug: Duplicate packages are not expected here"

data BuildPlanCheck =
      BuildPlanCheckOk      (Map PackageName (Map FlagName Bool))
    | BuildPlanCheckPartial (Map PackageName (Map FlagName Bool)) DepErrors
    | BuildPlanCheckFail    (Map PackageName (Map FlagName Bool)) DepErrors
                            CompilerVersion

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
    :: ( MonadIO m, MonadCatch m, MonadLogger m, MonadReader env m
       , HasHttpManager env, HasConfig env, HasGHCVariant env
       , MonadBaseControl IO m)
    => [GenericPackageDescription]
    -> Maybe (Map PackageName (Map FlagName Bool))
    -> SnapName
    -> m BuildPlanCheck
checkSnapBuildPlan gpds flags snap = do
    platform <- asks (configPlatform . getConfig)
    mbp <- loadMiniBuildPlan snap

    let
        compiler = mbpCompilerVersion mbp
        snapPkgs = fmap mpiVersion $ mbpPackages mbp
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
    :: ( MonadIO m, MonadCatch m, MonadLogger m, MonadReader env m
       , HasHttpManager env, HasConfig env, HasGHCVariant env
       , MonadBaseControl IO m)
    => [GenericPackageDescription]
    -> NonEmpty SnapName
    -> m (SnapName, BuildPlanCheck)
selectBestSnapshot gpds snaps = do
    $logInfo $ "Selecting the best among "
               <> T.pack (show (NonEmpty.length snaps))
               <> " snapshots...\n"
    F.foldr1 go (NonEmpty.map getResult snaps)
    where
        go mold mnew = do
            old@(_snap, bpc) <- mold
            case bpc of
                BuildPlanCheckOk {} -> return old
                _ -> fmap (betterSnap old) mnew

        getResult snap = do
            result <- checkSnapBuildPlan gpds Nothing snap
            reportResult result snap
            return (snap, result)

        betterSnap (s1, r1) (s2, r2)
          | compareBuildPlanCheck r1 r2 /= LT = (s1, r1)
          | otherwise = (s2, r2)

        reportResult BuildPlanCheckOk {} snap = do
            $logInfo $ "* Matches " <> renderSnapName snap
            $logInfo ""

        reportResult r@BuildPlanCheckPartial {} snap = do
            $logWarn $ "* Partially matches " <> renderSnapName snap
            $logWarn $ indent $ T.pack $ show r

        reportResult r@BuildPlanCheckFail {} snap = do
            $logWarn $ "* Rejected " <> renderSnapName snap
            $logWarn $ indent $ T.pack $ show r

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
    if (not $ Map.null fl) then
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
        formatFlags (f, v) = (show f) ++ " = " ++ (show v)

showMapPackages :: Map PackageName a -> Text
showMapPackages mp = showItems $ Map.keys mp

showCompilerErrors
    :: Map PackageName (Map FlagName Bool)
    -> DepErrors
    -> CompilerVersion
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
          else ("Using package flags:\n" <> flagVals)
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

shadowMiniBuildPlan :: MiniBuildPlan
                    -> Set PackageName
                    -> (MiniBuildPlan, Map PackageName MiniPackageInfo)
shadowMiniBuildPlan (MiniBuildPlan cv pkgs0) shadowed =
    (MiniBuildPlan cv $ Map.fromList met, Map.fromList unmet)
  where
    pkgs1 = Map.difference pkgs0 $ Map.fromSet (\_ -> ()) shadowed

    depsMet = flip execState Map.empty $ mapM_ (check Set.empty) (Map.keys pkgs1)

    check visited name
        | name `Set.member` visited =
            error $ "shadowMiniBuildPlan: cycle detected, your MiniBuildPlan is broken: " ++ show (visited, name)
        | otherwise = do
            m <- get
            case Map.lookup name m of
                Just x -> return x
                Nothing ->
                    case Map.lookup name pkgs1 of
                        Nothing
                            | name `Set.member` shadowed -> return False

                            -- In this case, we have to assume that we're
                            -- constructing a build plan on a different OS or
                            -- architecture, and therefore different packages
                            -- are being chosen. The common example of this is
                            -- the Win32 package.
                            | otherwise -> return True
                        Just mpi -> do
                            let visited' = Set.insert name visited
                            ress <- mapM (check visited') (Set.toList $ mpiPackageDeps mpi)
                            let res = and ress
                            modify $ \m' -> Map.insert name res m'
                            return res

    (met, unmet) = partitionEithers $ map toEither $ Map.toList pkgs1

    toEither pair@(name, _) =
        wrapper pair
      where
        wrapper =
            case Map.lookup name depsMet of
                Just True -> Left
                Just False -> Right
                Nothing -> assert False Right

parseCustomMiniBuildPlan :: (MonadIO m, MonadCatch m, MonadLogger m, MonadReader env m, HasHttpManager env, HasConfig env, MonadBaseControl IO m)
                         => Path Abs File -- ^ stack.yaml file location
                         -> T.Text -> m MiniBuildPlan
parseCustomMiniBuildPlan stackYamlFP url0 = do
    yamlFP <- getYamlFP url0

    yamlBS <- liftIO $ S.readFile $ toFilePath yamlFP
    let yamlHash = S8.unpack $ B16.encode $ SHA256.hash yamlBS
    binaryFilename <- parseRelFile $ yamlHash ++ ".bin"
    customPlanDir <- getCustomPlanDir
    let binaryFP = customPlanDir </> $(mkRelDir "bin") </> binaryFilename

    taggedDecodeOrLoad binaryFP $ do
        cs <- either throwM return $ decodeEither' yamlBS
        let addFlags :: PackageIdentifier -> (PackageName, (Version, Map FlagName Bool))
            addFlags (PackageIdentifier name ver) =
                (name, (ver, fromMaybe Map.empty $ Map.lookup name $ csFlags cs))
        toMiniBuildPlan
            (csCompilerVersion cs)
            Map.empty
            (Map.fromList $ map addFlags $ Set.toList $ csPackages cs)
  where
    getCustomPlanDir = do
        root <- asks $ configStackRoot . getConfig
        return $ root </> $(mkRelDir "custom-plan")

    -- Get the path to the YAML file
    getYamlFP url =
        case parseUrl $ T.unpack url of
            Just req -> getYamlFPFromReq url req
            Nothing -> getYamlFPFromFile url

    getYamlFPFromReq url req = do
        let hashStr = S8.unpack $ B16.encode $ SHA256.hash $ encodeUtf8 url
        hashFP <- parseRelFile $ hashStr ++ ".yaml"
        customPlanDir <- getCustomPlanDir

        let cacheFP = customPlanDir </> $(mkRelDir "yaml") </> hashFP
        _ <- download req cacheFP
        return cacheFP

    getYamlFPFromFile url = do
        fp <- liftIO $ D.canonicalizePath $ toFilePath (parent stackYamlFP) FP.</> T.unpack (fromMaybe url $
            T.stripPrefix "file://" url <|> T.stripPrefix "file:" url)
        parseAbsFile fp

data CustomSnapshot = CustomSnapshot
    { csCompilerVersion :: !CompilerVersion
    , csPackages :: !(Set PackageIdentifier)
    , csFlags :: !(Map PackageName (Map FlagName Bool))
    }
instance FromJSON CustomSnapshot where
    parseJSON = withObject "CustomSnapshot" $ \o -> CustomSnapshot
        <$> ((o .: "compiler") >>= \t ->
                case parseCompilerVersion t of
                    Nothing -> fail $ "Invalid compiler: " ++ T.unpack t
                    Just compilerVersion -> return compilerVersion)
        <*> o .: "packages"
        <*> o .:? "flags" .!= Map.empty
