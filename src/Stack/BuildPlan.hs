{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


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
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Control.Exception.Enclosed      (tryIO)
import           Control.Monad                   (when)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State.Strict      (State, execState, get, modify,
                                                  put)
import           Data.Aeson                      (FromJSON (..))
import           Data.Aeson                      (withObject, withText, (.:))
import qualified Data.Binary                     as Binary
import qualified Data.Foldable                   as F
import qualified Data.HashMap.Strict             as HM
import           Data.IntMap                     (IntMap)
import qualified Data.IntMap                     as IntMap
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
                                                  flagName, genPackageFlags)
import           GHC.Generics                    (Generic)
import           Network.HTTP.Download
import           Path
import           Stack.Types
import           Stack.Constants
import           Stack.Package
import           System.Directory                (createDirectoryIfMissing)
import           System.FilePath                 (takeDirectory)

data BuildPlanException = UnknownPackages (Set PackageName)
    deriving (Show, Typeable)
instance Exception BuildPlanException

-- | Determine the necessary packages to install to have the given set of
-- packages available.
--
-- This function will not provide test suite and benchmark dependencies.
--
-- This may fail if a target package is not present in the @BuildPlan@.
resolveBuildPlan :: MonadThrow m
                 => MiniBuildPlan
                 -> Set PackageName
                 -> m (Map PackageName (Version, Map FlagName Bool))
resolveBuildPlan mbp packages
    | Set.null (rsUnknown rs) = return (rsToInstall rs)
    | otherwise = throwM $ UnknownPackages $ rsUnknown rs
  where
    rs = getDeps mbp packages

data ResolveState = ResolveState
    { rsVisited   :: Set PackageName
    , rsUnknown   :: Set PackageName
    , rsToInstall :: Map PackageName (Version, Map FlagName Bool)
    }

data MiniBuildPlan = MiniBuildPlan
    { mbpGhcVersion :: !Version
    , mbpPackages :: !(Map PackageName (Version, Map FlagName Bool, Set PackageName))
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

toMiniBuildPlan :: BuildPlan -> MiniBuildPlan
toMiniBuildPlan bp = MiniBuildPlan
    { mbpGhcVersion = siGhcVersion $ bpSystemInfo bp
    , mbpPackages = Map.union cores extras
    }
  where
    -- We never build the test suites or benchmarks of dependencies
    includeDep di = CompLibrary `Set.member` diComponents di
                 || CompExecutable `Set.member` diComponents di

    cores = fmap (\v -> (v, Map.empty, Set.empty)) $ siCorePackages $ bpSystemInfo bp
    extras = fmap goPP $ bpPackages bp

    goPP pp =
        ( ppVersion pp
        , pcFlagOverrides $ ppConstraints pp
        , Set.unions $ map goDI $ Map.toList $ sdPackages $ ppDesc pp
        -- FIXME add tool info
        )

    goDI (name, di)
        | includeDep di = Set.singleton name
        | otherwise = Set.empty

-- | Resolve all packages necessary to install for
getDeps :: F.Foldable f => MiniBuildPlan -> f PackageName -> ResolveState
getDeps mbp packages =
    execState (F.mapM_ goName packages) ResolveState
        { rsVisited = Set.empty
        , rsUnknown = Set.empty
        , rsToInstall = Map.empty
        }
  where
    goName :: PackageName -> State ResolveState ()
    goName name = do
        rs <- get
        when (name `Set.notMember` rsVisited rs) $ do
            put rs { rsVisited = Set.insert name $ rsVisited rs }
            case Map.lookup name $ mbpPackages mbp of
                Just (version, flags, deps) -> do
                    F.mapM_ goName deps
                    modify $ \rs' -> rs'
                        { rsToInstall = Map.insert name (version, flags) $ rsToInstall rs'
                        }
                Nothing -> modify $ \rs' -> rs'
                    { rsUnknown = Set.insert name $ rsUnknown rs'
                    }

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
    :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader env m, HasHttpManager env, HasStackRoot env)
    => SnapName
    -> m MiniBuildPlan
loadMiniBuildPlan name = do
    path <- configMiniBuildPlanCache name
    let fp = toFilePath path
        dir = toFilePath $ parent path

    eres <- liftIO $ tryIO $ Binary.decodeFileOrFail fp
    case eres of
        Right (Right mbp) -> return mbp
        _ -> do
            $logDebug $ "loadMiniBuildPlan from cache failed: " <> T.pack (show (name, eres))
            bp <- loadBuildPlan name
            let mbp = toMiniBuildPlan bp
            liftIO $ do
                createDirectoryIfMissing True dir
                Binary.encodeFile fp mbp
            return mbp

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
        pkg <- resolvePackage pkgConfig cabalfp gpd
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
          -> Map PackageName (Version, ignored1, ignored2)
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
        case Map.lookup name packages of
            Nothing -> Just $ "Package not present: " <> packageNameText name
            Just (v, _, _)
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
findBuildPlan :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader env m, HasHttpManager env, HasStackRoot env, HasUrls env)
              => Path Abs File
              -> GenericPackageDescription
              -> m (Maybe (SnapName, Map FlagName Bool))
findBuildPlan cabalfp gpd = do
    snapshots <- getSnapshots
    let names =
            map (uncurry LTS)
                (take 2 $ reverse $ IntMap.toList $ snapshotsLts snapshots)
            ++ [Nightly $ snapshotsNightly snapshots]
        loop [] = return Nothing
        loop (name:names') = do
            mbp <- loadMiniBuildPlan name
            mflags <- checkBuildPlan name mbp cabalfp gpd
            case mflags of
                Nothing -> loop names'
                Just flags -> return $ Just (name, flags)
    loop names
