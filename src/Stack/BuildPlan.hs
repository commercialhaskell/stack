{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}


-- | Resolving a build plan for a set of packages in a given Stackage
-- snapshot.

module Stack.BuildPlan
    ( BuildPlanException (..)
    , MiniBuildPlan
    , mbpGhcVersion
    , Snapshots (..)
    , getSnapshots
    , loadMiniBuildPlan
    , resolveBuildPlan
    , findBuildPlan
    , removeReverseDeps
    , ToolMap
    , getToolMap
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Control.Arrow                   ((&&&))
import           Control.Exception.Enclosed      (tryIO, handleIO)
import           Control.Monad                   (when, liftM)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State.Strict      (State, execState, get, modify,
                                                  put)
import           Data.Aeson                      (FromJSON (..))
import           Data.Aeson                      (withObject, withText, (.:))
import qualified Data.Binary                     as Binary
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as S8
import           Data.Conduit
import qualified Data.Conduit.List               as CL
import           Data.Either                     (partitionEithers)
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
import           Data.Typeable                   (Typeable)
import           Data.Yaml                       (decodeFileEither)
import           Distribution.PackageDescription (GenericPackageDescription,
                                                  flagDefault, flagManual,
                                                  flagName, genPackageFlags,
                                                  executables, exeName)
import           GHC.Generics                    (Generic)
import           Network.HTTP.Download
import           Path
import           Stack.GhcPkg
import           Stack.Types
import           Stack.Constants
import           Stack.Package
import           Stack.PackageIndex
import           System.Directory                (createDirectoryIfMissing, getDirectoryContents)
import           System.FilePath                 (takeDirectory)

data BuildPlanException
    = UnknownPackages (Map PackageName (Set PackageName))
    | Couldn'tFindInIndex (Set PackageIdentifier)
    deriving (Typeable)
instance Exception BuildPlanException
instance Show BuildPlanException where
    show (UnknownPackages m) = unlines
        $ "The following packages do not exist in the build plan:"
        : map go (Map.toList m)
      where
        go (dep, users) | Set.null users = packageNameString dep
        go (dep, users) = concat
            [ packageNameString dep
            , " (used by "
            , intercalate ", " $ map packageNameString $ Set.toList users
            , ")"
            ]
    show (Couldn'tFindInIndex idents) =
        "Couldn't find the following packages in the index: " ++
        intercalate ", " (map packageIdentifierString $ Set.toList idents)

-- | Determine the necessary packages to install to have the given set of
-- packages available.
--
-- This function will not provide test suite and benchmark dependencies.
--
-- This may fail if a target package is not present in the @BuildPlan@.
resolveBuildPlan :: MonadThrow m
                 => MiniBuildPlan
                 -> Map PackageName (Set PackageName) -- ^ required packages, and users of it
                 -> m ( Map PackageName (Version, Map FlagName Bool)
                      , Map PackageName (Set PackageName)
                      )
resolveBuildPlan mbp packages
    | Map.null (rsUnknown rs) = return (rsToInstall rs, rsUsedBy rs)
    | otherwise = throwM $ UnknownPackages $ rsUnknown rs
  where
    rs = getDeps mbp packages

data ResolveState = ResolveState
    { rsVisited   :: Set PackageName
    , rsUnknown   :: Map PackageName (Set PackageName)
    , rsToInstall :: Map PackageName (Version, Map FlagName Bool)
    , rsUsedBy    :: Map PackageName (Set PackageName)
    }

-- | Information on a single package for the 'MiniBuildPlan'.
data MiniPackageInfo = MiniPackageInfo
    { mpiVersion :: !Version
    , mpiFlags :: !(Map FlagName Bool)
    , mpiPackageDeps :: !(Set PackageName)
    , mpiToolDeps :: !(Set ByteString)
    -- ^ Due to ambiguity in Cabal, it is unclear whether this refers to the
    -- executable name, the package name, or something else. We have to guess
    -- based on what's available, which is why we store this is an unwrapped
    -- 'ByteString'.
    , mpiExes :: !(Set ExeName)
    -- ^ Executables provided by this package
    }
    deriving (Generic, Show)
instance Binary.Binary MiniPackageInfo

-- | A simplified version of the 'BuildPlan' + cabal file.
data MiniBuildPlan = MiniBuildPlan
    { mbpGhcVersion :: !Version
    , mbpPackages :: !(Map PackageName MiniPackageInfo)
    }
    deriving (Generic, Show)
instance Binary.Binary MiniBuildPlan

-- | When a set of targets includes one of the packages in a build plan, we
-- need to remove that package from the build plan. See:
--
-- https://github.com/fpco/stack/issues/42
--
-- In the future, we may want to make the behavior actually match what's stated
-- (removing all the reverse deps). For now, we'll leave it to 'getDeps' to
-- discover if there's a dependency on a removed package.
removeReverseDeps :: F.Foldable f => f PackageName -> MiniBuildPlan -> MiniBuildPlan
removeReverseDeps toRemove mbp = mbp
    { mbpPackages = Map.difference (mbpPackages mbp) toRemoveMap
    }
  where
    toRemoveMap = Map.unions $ map (\x -> Map.singleton x ()) $ F.toList toRemove

toMiniBuildPlan :: (MonadIO m, MonadLogger m, MonadReader env m, HasHttpManager env, MonadThrow m, HasConfig env)
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
                }) $ siCorePackages $ bpSystemInfo bp

    goPP pp =
        ( ppVersion pp
        , pcFlagOverrides $ ppConstraints pp
        )

-- | Add in the resolved dependencies from the package index
addDeps :: (MonadIO m, MonadLogger m, MonadReader env m, HasHttpManager env, MonadThrow m, HasConfig env)
        => Version -- ^ GHC version
        -> Map PackageName (Version, Map FlagName Bool)
        -> m (Map PackageName MiniPackageInfo)
addDeps ghcVersion toCalc = do
    menv <- getMinimalEnvOverride
    eres <- tryAddDeps menv
    case eres of
        Left _ -> do
            $logInfo "Missing packages in index, updating and trying again"
            updateIndex menv
            tryAddDeps menv >>= either throwM return
        Right res -> return res
  where
    tryAddDeps menv = do
        idents <- sourcePackageIndex menv $$ CL.foldM go idents0
        return $ case partitionEithers $ map hoistEither $ Map.toList idents of
            ([], pairs) -> Right $ Map.fromList pairs
            (missing, _) -> Left $ Couldn'tFindInIndex $ Set.fromList missing

    idents0 = Map.fromList
        $ map (\(n, (v, f)) -> (PackageIdentifier n v, Left f))
        $ Map.toList toCalc

    hoistEither (ident, Left _) = Left ident
    hoistEither (PackageIdentifier name version, Right (flags, pdeps, tdeps, exes)) =
        Right (name, MiniPackageInfo
            { mpiVersion = version
            , mpiFlags = flags
            , mpiPackageDeps = pdeps
            , mpiToolDeps = tdeps
            , mpiExes = exes
            })

    go m ucf =
        case Map.lookup ident m of
            Just (Left flags) -> do
                gpd <- ucfParse ucf
                let packageConfig = PackageConfig
                        { packageConfigEnableTests = False
                        , packageConfigEnableBenchmarks = False
                        , packageConfigFlags = flags
                        , packageConfigGhcVersion = ghcVersion
                        }
                    pd = resolvePackageDescription packageConfig gpd
                    pdeps = Map.filterWithKey
                        (const . (/= ucfName ucf))
                        (packageDependencies pd)
                    tdeps = Map.keysSet (packageToolDependencies pd)
                    exes = Set.fromList $ map (ExeName . S8.pack . exeName) $ executables pd
                return $ Map.insert
                    ident
                    (Right
                        ( flags
                        , Map.keysSet pdeps
                        , tdeps
                        , exes
                        ))
                    m
            _ -> return m
      where
        ident = PackageIdentifier (ucfName ucf) (ucfVersion ucf)

-- | Resolve all packages necessary to install for
getDeps :: MiniBuildPlan -> Map PackageName (Set PackageName) -> ResolveState
getDeps mbp packages =
    execState (mapM_ (uncurry goName) $ Map.toList packages) ResolveState
        { rsVisited = Set.empty
        , rsUnknown = Map.empty
        , rsToInstall = Map.empty
        , rsUsedBy = Map.empty
        }
  where
    toolMap = getToolMap mbp

    goName :: PackageName -> Set PackageName -> State ResolveState ()
    goName name users = do
        -- Even though we could check rsVisited first and short-circuit things
        -- earlier, lookup in mbpPackages first so that we can produce more
        -- usable error information on missing dependencies
        rs <- get
        put rs
            { rsUsedBy = Map.insertWith Set.union name users $ rsUsedBy rs
            }
        case Map.lookup name $ mbpPackages mbp of
            Nothing -> modify $ \rs' -> rs'
                { rsUnknown = Map.insertWith Set.union name users $ rsUnknown rs'
                }
            Just mpi -> when (name `Set.notMember` rsVisited rs) $ do
                put rs { rsVisited = Set.insert name $ rsVisited rs }
                let depsForTools = Set.unions $ mapMaybe (flip Map.lookup toolMap) (Set.toList $ mpiToolDeps mpi)
                let deps = Set.filter (/= name) (mpiPackageDeps mpi <> depsForTools)
                F.mapM_ (flip goName $ Set.singleton name) deps
                modify $ \rs' -> rs'
                    { rsToInstall = Map.insert name (mpiVersion mpi, mpiFlags mpi) $ rsToInstall rs'
                    }

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
getSnapshots :: (MonadThrow m, MonadIO m, MonadReader env m, HasHttpManager env, HasStackRoot env, HasUrls env)
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
    :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader env m, HasHttpManager env, HasConfig env)
    => SnapName
    -> Map PackageName Version -- ^ packages in global database
    -> m MiniBuildPlan
loadMiniBuildPlan name globals = do
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
        { mbpPackages = mbpPackages mbp `Map.union`
            fmap (\v -> MiniPackageInfo
                { mpiVersion = v
                , mpiFlags = Map.empty
                , mpiPackageDeps = Set.empty
                , mpiToolDeps = Set.empty
                , mpiExes = Set.empty
                }) globals
        }

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
            $logWarn $ "Decoding build plan from file failed: " <> T.pack (show e)
            liftIO $ createDirectoryIfMissing True $ takeDirectory $ toFilePath fp
            req <- parseUrl $ T.unpack url
            $logWarn $ "Instead, downloading build plan from: " <> url
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
checkBuildPlan :: (MonadLogger m, MonadThrow m, MonadIO m)
               => SnapName -- ^ used only for debugging purposes
               -> MiniBuildPlan
               -> Path Abs File -- ^ cabal file path, used only for debugging purposes
               -> GenericPackageDescription
               -> m (Maybe (Map FlagName Bool))
checkBuildPlan name mbp cabalfp gpd = do
    $logInfo $ "Checking against build plan " <> renderSnapName name
    loop flagOptions
  where
    loop [] = return Nothing
    loop (flags:rest) = do
        pkg <- resolvePackage pkgConfig cabalfp PTUser gpd
        passes <- checkDeps flags (packageDeps pkg) (mbpPackages mbp)
        if passes
            then return $ Just flags
            else loop rest
      where
        pkgConfig = PackageConfig
            { packageConfigEnableTests = True
            , packageConfigEnableBenchmarks = True
            , packageConfigFlags = flags
            , packageConfigGhcVersion = ghcVersion
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
findBuildPlan :: (MonadIO m, MonadCatch m, MonadLogger m, MonadReader env m, HasHttpManager env, HasConfig env)
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
    let names = concat
            [ take 2 $ filter isLTS existing
            , take 2 $ filter isNightly existing
            , map (uncurry LTS)
                (take 2 $ reverse $ IntMap.toList $ snapshotsLts snapshots)
            , [Nightly $ snapshotsNightly snapshots]
            ]
        loop [] = return Nothing
        loop (name:names') = do
            menv <- getMinimalEnvOverride
            globalDB <- getGlobalDB menv
            globals <- getPackageVersionMap menv [globalDB]

            mbp <- loadMiniBuildPlan name globals
            mflags <- checkBuildPlan name mbp cabalfp gpd
            case mflags of
                Nothing -> loop names'
                Just flags -> return $ Just (name, flags)
    loop names
