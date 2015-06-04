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
    , MiniBuildPlan(..)
    , MiniPackageInfo(..)
    , Snapshots (..)
    , getSnapshots
    , loadMiniBuildPlan
    , resolveBuildPlan
    , findBuildPlan
    , ToolMap
    , getToolMap
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Control.Arrow                   ((&&&))
import           Control.Exception.Enclosed      (tryIO, handleIO)
import           Control.Monad                   (liftM, forM)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Control.Monad.Reader            (asks)
import           Control.Monad.State.Strict      (State, execState, get, modify,
                                                  put)
import           Data.Aeson                      (FromJSON (..))
import           Data.Aeson                      (withObject, withText, (.:))
import qualified Data.Binary                     as Binary
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as S8
import qualified Data.Foldable                   as F
import qualified Data.HashMap.Strict             as HM
import           Data.IntMap                     (IntMap)
import qualified Data.IntMap                     as IntMap
import           Data.List                       (intercalate, sort)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (mapMaybe)
import           Data.Monoid                     ((<>))
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Time                       (Day)
import qualified Data.Traversable                as Tr
import           Data.Typeable                   (Typeable)
import           Data.Yaml                       (decodeFileEither)
import           Distribution.PackageDescription (GenericPackageDescription,
                                                  flagDefault, flagManual,
                                                  flagName, genPackageFlags,
                                                  executables, exeName, library, libBuildInfo, buildable)
import           Network.HTTP.Download
import           Path
import           Stack.Fetch
import           Stack.GhcPkg
import           Stack.Types
import           Stack.Constants
import           Stack.Package
import           Stack.PackageIndex
import           System.Directory                (createDirectoryIfMissing, getDirectoryContents)
import           System.FilePath                 (takeDirectory)

data BuildPlanException
    = UnknownPackages
        (Path Abs File) -- ^ stack.yaml file
        (Map PackageName (Maybe Version, (Set PackageName))) -- truly unknown
        (Map PackageName (Set PackageIdentifier)) -- shadowed
    deriving (Typeable)
instance Exception BuildPlanException
instance Show BuildPlanException where
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
resolveBuildPlan :: (MonadThrow m, MonadIO m, MonadReader env m, HasBuildConfig env, MonadLogger m, HasHttpManager env)
                 => EnvOverride
                 -> MiniBuildPlan
                 -> (PackageName -> Bool) -- ^ is it shadowed by a local package?
                 -> Map PackageName (Set PackageName) -- ^ required packages, and users of it
                 -> m ( Map PackageName (Version, Map FlagName Bool)
                      , Map PackageName (Set PackageName)
                      )
resolveBuildPlan menv mbp isShadowed packages
    | Map.null (rsUnknown rs) && Map.null (rsShadowed rs) = return (rsToInstall rs, rsUsedBy rs)
    | otherwise = do
        cache <- getPackageCaches menv
        let maxVer = Map.fromListWith max $ map toTuple $ Map.keys cache
            unknown = flip Map.mapWithKey (rsUnknown rs) $ \ident x ->
                (Map.lookup ident maxVer, x)
        bconfig <- asks getBuildConfig
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

toMiniBuildPlan :: (MonadIO m, MonadLogger m, MonadReader env m, HasHttpManager env, MonadThrow m, HasConfig env, MonadBaseControl IO m)
                => BuildPlan -> m MiniBuildPlan
toMiniBuildPlan bp = do
    extras <- addDeps ghcVersion $ fmap goPP $ bpPackages bp
    return MiniBuildPlan
        { mbpGhcVersion = ghcVersion
        , mbpPackages = Map.union cores extras
        }
  where
    ghcVersion = siGhcVersion $ bpSystemInfo bp
    cores = fmap (\v -> MiniPackageInfo
                { mpiVersion = v
                , mpiFlags = Map.empty
                , mpiPackageDeps = Set.empty
                , mpiToolDeps = Set.empty
                , mpiExes = Set.empty
                , mpiHasLibrary = True
                }) $ siCorePackages $ bpSystemInfo bp

    goPP pp =
        ( ppVersion pp
        , pcFlagOverrides $ ppConstraints pp
        )

-- | Add in the resolved dependencies from the package index
addDeps :: (MonadIO m, MonadLogger m, MonadReader env m, HasHttpManager env, MonadThrow m, HasConfig env, MonadBaseControl IO m)
        => Version -- ^ GHC version
        -> Map PackageName (Version, Map FlagName Bool)
        -> m (Map PackageName MiniPackageInfo)
addDeps ghcVersion toCalc = do
    menv <- getMinimalEnvOverride
    platform <- asks $ configPlatform . getConfig
    resolvedMap <- resolvePackages menv (Map.keysSet idents0) Set.empty
    let byIndex = Map.fromListWith (++) $ flip map (Map.toList resolvedMap)
            $ \(ident, rp) ->
                (indexName $ rpIndex rp,
                    [( ident
                    , rpCache rp
                    , maybe Map.empty snd $ Map.lookup (packageIdentifierName ident) toCalc
                    )])
    res <- forM (Map.toList byIndex) $ \(indexName', pkgs) -> withCabalFiles indexName' pkgs
        $ \ident flags cabalBS -> do
            gpd <- readPackageUnresolvedBS Nothing cabalBS
            let packageConfig = PackageConfig
                    { packageConfigEnableTests = False
                    , packageConfigEnableBenchmarks = False
                    , packageConfigFlags = flags
                    , packageConfigGhcVersion = ghcVersion
                    , packageConfigPlatform = platform
                    }
                name = packageIdentifierName ident
                pd = resolvePackageDescription packageConfig gpd
                exes = Set.fromList $ map (ExeName . S8.pack . exeName) $ executables pd
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
    return $ Map.fromList $ concat res
  where
    idents0 = Map.fromList
        $ map (\(n, (v, f)) -> (PackageIdentifier n v, Left f))
        $ Map.toList toCalc

-- | Resolve all packages necessary to install for
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
                            let m = Map.fromList $ map (\x -> (x, Set.singleton $ PackageIdentifier name (mpiVersion mpi)))
                                        $ Set.toList shadowed
                            modify $ \rs' -> rs'
                                { rsShadowed = Map.unionWith Set.union m $ rsShadowed rs'
                                }
                            return shadowed
                modify $ \rs' -> rs'
                    { rsToInstall = Map.insert name (mpiVersion mpi, mpiFlags mpi) $ rsToInstall rs'
                    , rsVisited = Map.insert name shadowed $ rsVisited rs'
                    }
                return shadowed

-- | Look up with packages provide which tools.
type ToolMap = Map ByteString (Set PackageName)

-- | Map from tool name to package providing it
getToolMap :: MiniBuildPlan -> Map ByteString (Set PackageName)
getToolMap mbp = Map.unionsWith Set.union
    -- First grab all of the package names, for times where a build tool is
    -- identified by package name
    $ Map.fromList (map (packageNameByteString &&& Set.singleton) (Map.keys ps))
    -- And then get all of the explicit executable names
    : concatMap goPair (Map.toList ps)
  where
    ps = mbpPackages mbp

    goPair (pname, mpi) =
        map (flip Map.singleton (Set.singleton pname) . unExeName)
      $ Set.toList
      $ mpiExes mpi

-- | Download the 'Snapshots' value from stackage.org.
getSnapshots :: (MonadThrow m, MonadIO m, MonadReader env m, HasHttpManager env, HasStackRoot env, HasConfig env)
             => m Snapshots
getSnapshots = askLatestSnapshotUrl >>= parseUrl . T.unpack >>= downloadJSON

-- | Most recent Nightly and newest LTS version per major release.
data Snapshots = Snapshots
    { snapshotsNightly :: !Day
    , snapshotsLts     :: !(IntMap Int)
    }
    deriving Show
instance FromJSON Snapshots where
    parseJSON = withObject "Snapshots" $ \o -> Snapshots
        <$> (o .: "nightly" >>= parseNightly)
        <*> (fmap IntMap.unions
                $ mapM parseLTS
                $ map snd
                $ filter (isLTS . fst)
                $ HM.toList o)
      where
        parseNightly t =
            case parseSnapName t of
                Left e -> fail $ show e
                Right (LTS _ _) -> fail "Unexpected LTS value"
                Right (Nightly d) -> return d

        isLTS = ("lts-" `T.isPrefixOf`)

        parseLTS = withText "LTS" $ \t ->
            case parseSnapName t of
                Left e -> fail $ show e
                Right (LTS x y) -> return $ IntMap.singleton x y
                Right (Nightly _) -> fail "Unexpected nightly value"

-- | Load up a 'MiniBuildPlan', preferably from cache
loadMiniBuildPlan
    :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader env m, HasHttpManager env, HasConfig env, MonadBaseControl IO m)
    => SnapName
    -> m MiniBuildPlan
loadMiniBuildPlan name = do
    path <- configMiniBuildPlanCache name
    let fp = toFilePath path
        dir = toFilePath $ parent path

    eres <- liftIO $ tryIO $ Binary.decodeFileOrFail fp
    mbp <- case eres of
        Right (Right mbp) -> return mbp
        _ -> do
            $logDebug $ "loadMiniBuildPlan from cache failed: " <> T.pack (show (name, eres))
            bp <- loadBuildPlan name
            mbp <- liftM buildPlanFixes $ toMiniBuildPlan bp
            liftIO $ do
                createDirectoryIfMissing True dir
                Binary.encodeFile fp mbp
            return mbp
    return mbp

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
loadBuildPlan :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader env m, HasHttpManager env, HasStackRoot env)
              => SnapName
              -> m BuildPlan
loadBuildPlan name = do
    env <- ask
    let stackage = getStackRoot env
    file' <- parseRelFile $ T.unpack file
    let fp = stackage </> $(mkRelDir "build-plan") </> file'
    $logDebug $ "Decoding build plan from: " <> T.pack (toFilePath fp)
    eres <- liftIO $ decodeFileEither $ toFilePath fp
    case eres of
        Right bp -> return bp
        Left e -> do
            $logDebug $ "Decoding build plan from file failed: " <> T.pack (show e)
            liftIO $ createDirectoryIfMissing True $ takeDirectory $ toFilePath fp
            req <- parseUrl $ T.unpack url
            $logInfo $ "Downloading build plan from: " <> url
            download req fp
            liftIO (decodeFileEither $ toFilePath fp) >>= either throwM return
  where
    file = renderSnapName name <> ".yaml"
    reponame =
        case name of
            LTS _ _ -> "lts-haskell"
            Nightly _ -> "stackage-nightly"
    url = rawGithubUrl "fpco" reponame "master" file

-- | Find the set of @FlagName@s necessary to get the given
-- @GenericPackageDescription@ to compile against the given @BuildPlan@. Will
-- only modify non-manual flags, and will prefer default values for flags.
-- Returns @Nothing@ if no combination exists.
checkBuildPlan :: (MonadLogger m, MonadThrow m, MonadIO m, MonadReader env m, HasConfig env)
               => SnapName -- ^ used only for debugging purposes
               -> MiniBuildPlan
               -> Path Abs File -- ^ cabal file path, used only for debugging purposes
               -> GenericPackageDescription
               -> m (Maybe (Map FlagName Bool))
checkBuildPlan name mbp cabalfp gpd = do
    $logInfo $ "Checking against build plan " <> renderSnapName name
    platform <- asks (configPlatform . getConfig)
    loop platform flagOptions
  where
    loop _ [] = return Nothing
    loop platform (flags:rest) = do
        pkg <- resolvePackage pkgConfig cabalfp gpd
        passes <- checkDeps flags (packageDeps pkg) (mbpPackages mbp)
        if passes
            then return $ Just flags
            else loop platform rest
      where
        pkgConfig = PackageConfig
            { packageConfigEnableTests = True
            , packageConfigEnableBenchmarks = True
            , packageConfigFlags = flags
            , packageConfigGhcVersion = ghcVersion
            , packageConfigPlatform = platform
            }

    ghcVersion = mbpGhcVersion mbp

    flagName' = fromCabalFlagName . flagName

    flagOptions = map Map.fromList $ mapM getOptions $ genPackageFlags gpd
    getOptions f
        | flagManual f = [(flagName' f, flagDefault f)]
        | flagDefault f =
            [ (flagName' f, True)
            , (flagName' f, False)
            ]
        | otherwise =
            [ (flagName' f, False)
            , (flagName' f, True)
            ]

-- | Checks if the given package dependencies can be satisfied by the given set
-- of packages. Will fail if a package is either missing or has a version
-- outside of the version range.
checkDeps :: MonadLogger m
          => Map FlagName Bool -- ^ used only for debugging purposes
          -> Map PackageName VersionRange
          -> Map PackageName MiniPackageInfo
          -> m Bool
checkDeps flags deps packages = do
    let errs = mapMaybe go $ Map.toList deps
    if null errs
        then return True
        else do
            $logDebug $ "Checked against following flags: " <> T.pack (show flags)
            mapM_ $logDebug errs
            return False
  where
    go :: (PackageName, VersionRange) -> Maybe Text
    go (name, range) =
        case fmap mpiVersion $ Map.lookup name packages of
            Nothing -> Just $ "Package not present: " <> packageNameText name
            Just v
                | withinRange v range -> Nothing
                | otherwise -> Just $ T.concat
                    [ packageNameText name
                    , " version available: "
                    , versionText v
                    , " does not match "
                    , versionRangeText range
                    ]

-- | Find a snapshot and set of flags that is compatible with the given
-- 'GenericPackageDescription'. Returns 'Nothing' if no such snapshot is found.
findBuildPlan :: (MonadIO m, MonadCatch m, MonadLogger m, MonadReader env m, HasHttpManager env, HasConfig env, MonadBaseControl IO m)
              => Path Abs File
              -> GenericPackageDescription
              -> m (Maybe (SnapName, Map FlagName Bool))
findBuildPlan cabalfp gpd = do
    -- Get the most recent LTS and Nightly in the snapshots directory and
    -- prefer them over anything else, since odds are high that something
    -- already exists for them.
    existing <-
        liftM (reverse . sort . mapMaybe (parseSnapName . T.pack)) $
        snapshotsDir >>=
        liftIO . handleIO (const $ return [])
               . getDirectoryContents . toFilePath
    let isLTS LTS{} = True
        isLTS Nightly{} = False
        isNightly Nightly{} = True
        isNightly LTS{} = False

    snapshots <- getSnapshots
    let names = nubOrd $ concat
            [ take 2 $ filter isLTS existing
            , take 2 $ filter isNightly existing
            , map (uncurry LTS)
                (take 2 $ reverse $ IntMap.toList $ snapshotsLts snapshots)
            , [Nightly $ snapshotsNightly snapshots]
            ]
        loop [] = return Nothing
        loop (name:names') = do
            mbp <- loadMiniBuildPlan name
            mflags <- checkBuildPlan name mbp cabalfp gpd
            case mflags of
                Nothing -> loop names'
                Just flags -> return $ Just (name, flags)
    loop names

-- | Same semantics as @nub@, but more efficient by using the @Ord@ constraint.
nubOrd :: Ord a => [a] -> [a]
nubOrd =
    go Set.empty
  where
    go _ [] = []
    go s (x:xs)
        | x `Set.member` s = go s xs
        | otherwise = x : go (Set.insert x s) xs
