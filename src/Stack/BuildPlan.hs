{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
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
    , loadResolver
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
import           Control.Monad (liftM, forM, unless)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.State.Strict      (State, execState, get, modify,
                                                  put)
import qualified Crypto.Hash.SHA256 as SHA256
import           Data.Aeson.Extended (WithJSONWarnings(..), logJSONWarnings)
import           Data.Store.VersionTagged
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as S8
import           Data.Either (partitionEithers)
import qualified Data.Foldable as F
import qualified Data.HashSet as HashSet
import           Data.List (intercalate)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe, isNothing)
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
import           Network.HTTP.Download
import           Path
import           Path.IO
import           Prelude -- Fix AMP warning
import           Stack.Constants
import           Stack.Fetch
import           Stack.Package
import           Stack.PackageIndex
import           Stack.Types.BuildPlan
import           Stack.Types.FlagName
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageIndex
import           Stack.Types.PackageName
import           Stack.Types.Version
import           Stack.Types.Config
import           Stack.Types.Urls
import           Stack.Types.Compiler
import           Stack.Types.Resolver
import           Stack.Types.StackT

data BuildPlanException
    = UnknownPackages
        (Path Abs File) -- stack.yaml file
        (Map PackageName (Maybe Version, Set PackageName)) -- truly unknown
        (Map PackageName (Set PackageIdentifier)) -- shadowed
    | SnapshotNotFound SnapName
    | FilepathInDownloadedSnapshot T.Text
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
    show (FilepathInDownloadedSnapshot url) = unlines
        [ "Downloaded snapshot specified a 'resolver: { location: filepath }' "
        , "field, but filepaths are not allowed in downloaded snapshots.\n"
        , "Filepath specified: " ++ T.unpack url
        ]
    show (NeitherCompilerOrResolverSpecified url) =
        "Failed to load custom snapshot at " ++
        T.unpack url ++
        ", because no 'compiler' or 'resolver' is specified."

-- | Determine the necessary packages to install to have the given set of
-- packages available.
--
-- This function will not provide test suite and benchmark dependencies.
--
-- This may fail if a target package is not present in the @BuildPlan@.
resolveBuildPlan
    :: (StackMiniM env m, HasBuildConfig env)
    => MiniBuildPlan
    -> (PackageName -> Bool) -- ^ is it shadowed by a local package?
    -> Map PackageName (Set PackageName) -- ^ required packages, and users of it
    -> m ( Map PackageName (Version, Map FlagName Bool)
         , Map PackageName (Set PackageName)
         )
resolveBuildPlan mbp isShadowed packages
    | Map.null (rsUnknown rs) && Map.null (rsShadowed rs) = return (rsToInstall rs, rsUsedBy rs)
    | otherwise = do
        bconfig <- view buildConfigLocalL
        (caches, _gitShaCaches) <- getPackageCaches
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

toMiniBuildPlan
    :: (StackMiniM env m, HasConfig env)
    => CompilerVersion -- ^ Compiler version
    -> Map PackageName Version -- ^ cores
    -> Map PackageName (Version, Map FlagName Bool, [Text], Maybe GitSHA1) -- ^ non-core packages
    -> m MiniBuildPlan
toMiniBuildPlan compilerVersion corePackages packages = do
    -- Determine the dependencies of all of the packages in the build plan. We
    -- handle core packages specially, because some of them will not be in the
    -- package index. For those, we allow missing packages to exist, and then
    -- remove those from the list of dependencies, since there's no way we'll
    -- ever reinstall them anyway.
    (cores, missingCores) <- addDeps True compilerVersion
        $ fmap (, Map.empty, [], Nothing) corePackages

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
                , mpiGhcOptions = []
                , mpiPackageDeps = Set.empty
                , mpiToolDeps = Set.empty
                , mpiExes = Set.empty
                , mpiHasLibrary = True
                , mpiGitSHA1 = Nothing
                })

    removeMissingDeps cores mpi = mpi
        { mpiPackageDeps = Set.intersection cores (mpiPackageDeps mpi)
        }

-- | Add in the resolved dependencies from the package index
addDeps
    :: (StackMiniM env m, HasConfig env)
    => Bool -- ^ allow missing
    -> CompilerVersion -- ^ Compiler version
    -> Map PackageName (Version, Map FlagName Bool, [Text], Maybe GitSHA1)
    -> m (Map PackageName MiniPackageInfo, Set PackageIdentifier)
addDeps allowMissing compilerVersion toCalc = do
    menv <- getMinimalEnvOverride
    platform <- view platformL
    (resolvedMap, missingIdents) <-
        if allowMissing
            then do
                (missingNames, missingIdents, m) <-
                    resolvePackagesAllowMissing shaMap Set.empty
                assert (Set.null missingNames)
                    $ return (m, missingIdents)
            else do
                m <- resolvePackages menv shaMap Set.empty
                return (m, Set.empty)
    let byIndex = Map.fromListWith (++) $ flip map (Map.toList resolvedMap)
            $ \(ident, rp) ->
                let (cache, ghcOptions, sha) =
                        case Map.lookup (packageIdentifierName ident) toCalc of
                            Nothing -> (Map.empty, [], Nothing)
                            Just (_, x, y, z) -> (x, y, z)
                 in (indexName $ rpIndex rp,
                    [( ident
                    , rpCache rp
                    , sha
                    , (cache, ghcOptions, sha)
                    )])
    res <- forM (Map.toList byIndex) $ \(indexName', pkgs) -> withCabalFiles indexName' pkgs
        $ \ident (flags, ghcOptions, mgitSha) cabalBS -> do
            (_warnings,gpd) <- readPackageUnresolvedBS Nothing cabalBS
            let packageConfig = PackageConfig
                    { packageConfigEnableTests = False
                    , packageConfigEnableBenchmarks = False
                    , packageConfigFlags = flags
                    , packageConfigGhcOptions = ghcOptions
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
                , mpiGhcOptions = ghcOptions
                , mpiPackageDeps = notMe $ packageDependencies pd
                , mpiToolDeps = Map.keysSet $ packageToolDependencies pd
                , mpiExes = exes
                , mpiHasLibrary = maybe
                    False
                    (buildable . libBuildInfo)
                    (library pd)
                , mpiGitSHA1 = mgitSha
                })
    return (Map.fromList $ concat res, missingIdents)
  where
    shaMap = Map.fromList
        $ map (\(n, (v, _f, _ghcOptions, gitsha)) -> (PackageIdentifier n v, gitsha))
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

loadResolver
    :: (StackMiniM env m, HasConfig env, HasGHCVariant env)
    => Maybe (Path Abs File)
    -> Resolver
    -> m (MiniBuildPlan, LoadedResolver)
loadResolver mconfigPath resolver =
    case resolver of
        ResolverSnapshot snap ->
            liftM (, ResolverSnapshot snap) $ loadMiniBuildPlan snap
        -- TODO(mgsloan): Not sure what this FIXME means
        -- FIXME instead of passing the stackYaml dir we should maintain
        -- the file URL in the custom resolver always relative to stackYaml.
        ResolverCustom name url -> do
            (mbp, hash) <- parseCustomMiniBuildPlan mconfigPath url
            return (mbp, ResolverCustomLoaded name url hash)
        ResolverCompiler compiler -> return
            ( MiniBuildPlan
                { mbpCompilerVersion = compiler
                , mbpPackages = mempty
                }
            , ResolverCompiler compiler
            )

-- | Load up a 'MiniBuildPlan', preferably from cache
loadMiniBuildPlan
    :: (StackMiniM env m, HasConfig env, HasGHCVariant env)
    => SnapName -> m MiniBuildPlan
loadMiniBuildPlan name = do
    path <- configMiniBuildPlanCache name
    $(versionedDecodeOrLoad miniBuildPlanVC) path $ liftM buildPlanFixes $ do
        bp <- loadBuildPlan name
        toMiniBuildPlan
            (siCompilerVersion $ bpSystemInfo bp)
            (siCorePackages $ bpSystemInfo bp)
            (goPP <$> bpPackages bp)
  where
    goPP pp =
        ( ppVersion pp
        , pcFlagOverrides $ ppConstraints pp
         -- TODO: store ghc options in BuildPlan?
        , []
        , ppCabalFileInfo pp
            >>= fmap (GitSHA1 . encodeUtf8)
              . Map.lookup "GitSHA1"
              . cfiHashes
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
loadBuildPlan :: (StackMiniM env m, HasConfig env) => SnapName -> m BuildPlan
loadBuildPlan name = do
    stackage <- view stackRootL
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
            req <- parseRequest $ T.unpack url
            $logSticky $ "Downloading " <> renderSnapName name <> " build plan ..."
            $logDebug $ "Downloading build plan from: " <> url
            _ <- redownload req fp
            $logStickyDone $ "Downloaded " <> renderSnapName name <> " build plan."
            liftIO (decodeFileEither $ toFilePath fp) >>= either throwM return

  where
    file = renderSnapName name <> ".yaml"

buildBuildPlanUrl :: (MonadReader env m, HasConfig env) => SnapName -> Text -> m Text
buildBuildPlanUrl name file = do
    urls <- view $ configL.to configUrls
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
    -> CompilerVersion
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
    :: (StackM env m, HasConfig env, HasGHCVariant env)
    => [GenericPackageDescription]
    -> Maybe (Map PackageName (Map FlagName Bool))
    -> SnapName
    -> m BuildPlanCheck
checkSnapBuildPlan gpds flags snap = do
    platform <- view platformL
    mbp <- loadMiniBuildPlan snap

    let
        compiler = mbpCompilerVersion mbp
        snapPkgs = mpiVersion <$> mbpPackages mbp
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
    :: (StackM env m, HasConfig env, HasGHCVariant env)
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

shadowMiniBuildPlan :: MiniBuildPlan
                    -> Set PackageName
                    -> (MiniBuildPlan, Map PackageName MiniPackageInfo)
shadowMiniBuildPlan (MiniBuildPlan cv pkgs0) shadowed =
    (MiniBuildPlan cv (Map.fromList met), Map.fromList unmet)
  where
    pkgs1 = Map.difference pkgs0 $ Map.fromSet (const ()) shadowed

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

-- This works differently for snapshots fetched from URL and those
-- fetched from file:
--
-- 1) If downloading the snapshot from a URL, assume the fetched data is
-- immutable. Hash the URL in order to determine the location of the
-- cached download. The file contents of the snapshot determines the
-- hash for looking up cached MBP.
--
-- 2) If loading the snapshot from a file, load all of the involved
-- snapshot files. The hash used to determine the cached MBP is the hash
-- of the concatenation of the parent's hash with the snapshot contents.
--
-- Why this difference? We want to make it easy to simply edit snapshots
-- in the filesystem, but we want caching for remote snapshots. In order
-- to avoid reparsing / reloading all the yaml for remote snapshots, we
-- need a different hash system.

-- TODO: This could probably be more efficient if it first merged the
-- custom snapshots, and then applied them to the MBP. It is nice to
-- apply directly, because then we have the guarantee that it's
-- semantically identical to snapshot extension. If this optimization is
-- implemented, note that the direct Monoid for CustomSnapshot is not
-- correct. Crucially, if a package is present in the snapshot, its
-- flags and ghc-options are not based on settings from prior snapshots.
-- TODO: This semantics should be discussed / documented more.

-- TODO: allow a hash check in the resolver. This adds safety /
-- correctness, allowing you to ensure that you are indeed getting the
-- right custom snapshot.

-- TODO: Allow custom plan to specify a name.

parseCustomMiniBuildPlan
    :: (StackMiniM env m, HasConfig env, HasGHCVariant env)
    => Maybe (Path Abs File) -- ^ Root directory for when url is a filepath
    -> T.Text
    -> m (MiniBuildPlan, SnapshotHash)
parseCustomMiniBuildPlan mconfigPath0 url0 = do
    $logDebug $ "Loading " <> url0 <> " build plan"
    case parseUrlThrow $ T.unpack url0 of
        Just req -> downloadCustom url0 req
        Nothing ->
           case mconfigPath0 of
               Nothing -> throwM $ FilepathInDownloadedSnapshot url0
               Just configPath -> do
                   (getMbp, hash) <- readCustom configPath url0
                   mbp <- getMbp
                   -- NOTE: We make the choice of only writing a cache
                   -- file for the full MBP, not the intermediate ones.
                   -- This isn't necessarily the best choice if we want
                   -- to share work extended snapshots. I think only
                   -- writing this one is more efficient for common
                   -- cases.
                   binaryPath <- getBinaryPath hash
                   alreadyCached <- doesFileExist binaryPath
                   unless alreadyCached $ $(versionedEncodeFile miniBuildPlanVC) binaryPath mbp
                   return (mbp, hash)
  where
    downloadCustom url req = do
        let urlHash = S8.unpack $ trimmedSnapshotHash $ doHash $ encodeUtf8 url
        hashFP <- parseRelFile $ urlHash ++ ".yaml"
        customPlanDir <- getCustomPlanDir
        let cacheFP = customPlanDir </> $(mkRelDir "yaml") </> hashFP
        _ <- download req cacheFP
        yamlBS <- liftIO $ S.readFile $ toFilePath cacheFP
        let yamlHash = doHash yamlBS
        binaryPath <- getBinaryPath yamlHash
        liftM (, yamlHash) $ $(versionedDecodeOrLoad miniBuildPlanVC) binaryPath $ do
            (cs, mresolver) <- decodeYaml yamlBS
            parentMbp <- case (csCompilerVersion cs, mresolver) of
                (Nothing, Nothing) -> throwM (NeitherCompilerOrResolverSpecified url)
                (Just cv, Nothing) -> return (compilerBuildPlan cv)
                -- NOTE: ignoring the parent's hash, even though
                -- there could be one. URL snapshot's hash are
                -- determined just from their contents.
                (_, Just resolver) -> liftM fst (loadResolver Nothing resolver)
            applyCustomSnapshot cs parentMbp
    readCustom configPath path = do
        yamlFP <- resolveFile (parent configPath) (T.unpack $ fromMaybe path $
            T.stripPrefix "file://" path <|> T.stripPrefix "file:" path)
        yamlBS <- liftIO $ S.readFile $ toFilePath yamlFP
        (cs, mresolver) <- decodeYaml yamlBS
        (getMbp, hash) <- case mresolver of
            Just (ResolverCustom _ url ) ->
                case parseUrlThrow $ T.unpack url of
                    Just req -> do
                        let getMbp = do
                                -- Ignore custom hash, under the
                                -- assumption that the URL is sufficient
                                -- for identity.
                                (mbp, _) <- downloadCustom url req
                                return mbp
                        return (getMbp, doHash yamlBS)
                    Nothing -> do
                        (getMbp0, SnapshotHash hash0) <- readCustom yamlFP url
                        let hash = doHash (hash0 <> yamlBS)
                            getMbp = do
                                binaryPath <- getBinaryPath hash
                                -- Idea here is to not waste time
                                -- writing out intermediate cache files,
                                -- but check for them.
                                exists <- doesFileExist binaryPath
                                if exists
                                    then do
                                        eres <- $(versionedDecodeFile miniBuildPlanVC) binaryPath
                                        case eres of
                                            Just mbp -> return mbp
                                            -- Invalid format cache file, remove.
                                            Nothing -> do
                                                removeFile binaryPath
                                                getMbp0
                                    else getMbp0
                        return (getMbp, hash)
            Just resolver -> do
                -- NOTE: in the cases where we don't have a hash, the
                -- normal resolver name is enough. Since this name is
                -- part of the yaml file, it ends up in our hash.
                let hash = doHash yamlBS
                    getMbp = do
                        (mbp, resolver') <- loadResolver (Just configPath) resolver
                        let mhash = customResolverHash resolver'
                        assert (isNothing mhash) (return mbp)
                return (getMbp, hash)
            Nothing -> do
                case csCompilerVersion cs of
                    Nothing -> throwM (NeitherCompilerOrResolverSpecified path)
                    Just cv -> do
                        let hash = doHash yamlBS
                            getMbp = return (compilerBuildPlan cv)
                        return (getMbp, hash)
        return (applyCustomSnapshot cs =<< getMbp, hash)
    getBinaryPath hash = do
        binaryFilename <- parseRelFile $ S8.unpack (trimmedSnapshotHash hash) ++ ".bin"
        customPlanDir <- getCustomPlanDir
        return $ customPlanDir </> $(mkRelDir "bin") </> binaryFilename
    decodeYaml yamlBS = do
        WithJSONWarnings res warnings <-
             either (throwM . ParseCustomSnapshotException url0) return $
             decodeEither' yamlBS
        logJSONWarnings (T.unpack url0) warnings
        return res
    compilerBuildPlan cv = MiniBuildPlan
         { mbpCompilerVersion = cv
         , mbpPackages = mempty
         }
    getCustomPlanDir = do
        root <- view stackRootL
        return $ root </> $(mkRelDir "custom-plan")
    doHash = SnapshotHash . B64URL.encode . SHA256.hash

applyCustomSnapshot
    :: (StackMiniM env m, HasConfig env)
    => CustomSnapshot
    -> MiniBuildPlan
    -> m MiniBuildPlan
applyCustomSnapshot cs mbp0 = do
    let CustomSnapshot mcompilerVersion
                       packages
                       dropPackages
                       (PackageFlags flags)
                       ghcOptions
            = cs
        addFlagsAndOpts :: PackageIdentifier -> (PackageName, (Version, Map FlagName Bool, [Text], Maybe GitSHA1))
        addFlagsAndOpts (PackageIdentifier name ver) =
            ( name
            , ( ver
              , Map.findWithDefault Map.empty name flags
              -- NOTE: similar to 'allGhcOptions' in Stack.Types.Build
              , ghcOptionsFor name ghcOptions
              -- we add a Nothing since we don't yet collect Git SHAs for custom snapshots
              , Nothing
              )
            )
        packageMap = Map.fromList $ map addFlagsAndOpts $ Set.toList packages
        cv = fromMaybe (mbpCompilerVersion mbp0) mcompilerVersion
        packages0 =
             mbpPackages mbp0 `Map.difference` Map.fromSet (const ()) dropPackages
    mbp1 <- toMiniBuildPlan cv mempty packageMap
    return MiniBuildPlan
        { mbpCompilerVersion = cv
        , mbpPackages = Map.union (mbpPackages mbp1) packages0
        }
