{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
-- Determine which packages are already installed
module Stack.Build.Installed
    ( InstalledMap
    , Installed (..)
    , GetInstalledOpts (..)
    , getInstalled
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch          (MonadCatch, MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader         (MonadReader, asks)
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.List            as CL
import           Data.Function
import qualified Data.HashSet                 as HashSet
import           Data.List
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Network.HTTP.Client.Conduit  (HasHttpManager)
import           Path
import           Prelude                      hiding (FilePath, writeFile)
import           Stack.Build.Cache
import           Stack.Build.Types
import           Stack.Constants
import           Stack.GhcPkg
import           Stack.PackageDump
import           Stack.Types
import           Stack.Types.Internal

type M env m = (MonadIO m,MonadReader env m,HasHttpManager env,HasEnvConfig env,MonadLogger m,MonadBaseControl IO m,MonadCatch m,MonadMask m,HasLogLevel env)

data LoadHelper = LoadHelper
    { lhId   :: !GhcPkgId
    , lhDeps :: ![GhcPkgId]
    , lhPair :: !(PackageName, (Version, Location, Installed)) -- TODO Version is now redundant and can be gleaned from Installed
    }
    deriving Show

type InstalledMap = Map PackageName (Version, Location, Installed) -- TODO Version is now redundant and can be gleaned from Installed

-- | Options for 'getInstalled'.
data GetInstalledOpts = GetInstalledOpts
    { getInstalledProfiling :: !Bool
      -- ^ Require profiling libraries?
    , getInstalledHaddock   :: !Bool
      -- ^ Require haddocks?
    }

-- | Returns the new InstalledMap and all of the locally registered packages.
getInstalled :: (M env m, PackageInstallInfo pii)
             => EnvOverride
             -> GetInstalledOpts
             -> Map PackageName pii -- ^ does not contain any installed information
             -> m (InstalledMap, Set GhcPkgId)
getInstalled menv opts sourceMap = do
    snapDBPath <- packageDatabaseDeps
    localDBPath <- packageDatabaseLocal

    bconfig <- asks getBuildConfig

    mcache <-
        if getInstalledProfiling opts || getInstalledHaddock opts
            then liftM Just $ loadInstalledCache $ configInstalledCache bconfig
            else return Nothing

    let loadDatabase' = loadDatabase menv opts mcache sourceMap
    (installedLibs', localInstalled) <-
        loadDatabase' Nothing [] >>=
        loadDatabase' (Just (Snap, snapDBPath)) . fst >>=
        loadDatabase' (Just (Local, localDBPath)) . fst
    let installedLibs = M.fromList $ map lhPair installedLibs'

    case mcache of
        Nothing -> return ()
        Just pcache -> saveInstalledCache (configInstalledCache bconfig) pcache

    -- Add in the executables that are installed, making sure to only trust a
    -- listed installation under the right circumstances (see below)
    let exesToSM loc = Map.unions . map (exeToSM loc)
        exeToSM loc (PackageIdentifier name version) =
            case Map.lookup name sourceMap of
                -- Doesn't conflict with anything, so that's OK
                Nothing -> m
                Just pii
                    -- Not the version we want, ignore it
                    | version /= piiVersion pii || loc /= piiLocation pii -> Map.empty

                    | otherwise -> m
          where
            m = Map.singleton name (version, loc, Executable $ PackageIdentifier name version)
    exesSnap <- getInstalledExes Snap
    exesLocal <- getInstalledExes Local
    let installedMap = Map.unions
            [ exesToSM Local exesLocal
            , exesToSM Snap exesSnap
            , installedLibs
            ]

    return (installedMap, localInstalled)

-- | Outputs both the modified InstalledMap and the Set of all installed packages in this database
--
-- The goal is to ascertain that the dependencies for a package are present,
-- that it has profiling if necessary, and that it matches the version and
-- location needed by the SourceMap
loadDatabase :: (M env m, PackageInstallInfo pii)
             => EnvOverride
             -> GetInstalledOpts
             -> Maybe InstalledCache -- ^ if Just, profiling or haddock is required
             -> Map PackageName pii -- ^ to determine which installed things we should include
             -> Maybe (Location, Path Abs Dir) -- ^ package database, Nothing for global
             -> [LoadHelper] -- ^ from parent databases
             -> m ([LoadHelper], Set GhcPkgId)
loadDatabase menv opts mcache sourceMap mdb lhs0 = do
    (lhs1, gids) <- ghcPkgDump menv (fmap snd mdb)
                  $ conduitDumpPackage =$ sink
    let lhs = pruneDeps
            (packageIdentifierName . ghcPkgIdPackageIdentifier)
            lhId
            lhDeps
            const
            (lhs0 ++ lhs1)
    return (map (\lh -> lh { lhDeps = [] }) $ Map.elems lhs, Set.fromList gids)
  where
    conduitProfilingCache =
        case mcache of
            Just cache | getInstalledProfiling opts -> addProfiling cache
            -- Just an optimization to avoid calculating the profiling
            -- values when they aren't necessary
            _ -> CL.map (\dp -> dp { dpProfiling = False })
    conduitHaddockCache =
        case mcache of
            Just cache | getInstalledHaddock opts -> addHaddock cache
            -- Just an optimization to avoid calculating the haddock
            -- values when they aren't necessary
            _ -> CL.map (\dp -> dp { dpHaddock = False })
    sinkDP = conduitProfilingCache
          =$ conduitHaddockCache
          =$ CL.mapMaybe (isAllowed opts mcache sourceMap (fmap fst mdb))
          =$ CL.consume
    sinkGIDs = CL.map dpGhcPkgId =$ CL.consume
    sink = getZipSink $ (,)
        <$> ZipSink sinkDP
        <*> ZipSink sinkGIDs

-- | Check if a can be included in the set of installed packages or not, based
-- on the package selections made by the user. This does not perform any
-- dirtiness or flag change checks.
isAllowed :: PackageInstallInfo pii
          => GetInstalledOpts
          -> Maybe InstalledCache
          -> Map PackageName pii
          -> Maybe Location
          -> DumpPackage Bool Bool
          -> Maybe LoadHelper
isAllowed opts mcache sourceMap mloc dp
    -- Check that it can do profiling if necessary
    | getInstalledProfiling opts && isJust mcache && not (dpProfiling dp) = Nothing
    -- Check that it has haddocks if necessary
    | getInstalledHaddock opts && isJust mcache && not (dpHaddock dp) = Nothing
    | toInclude = Just LoadHelper
        { lhId = gid
        , lhDeps =
            -- We always want to consider the wired in packages as having all
            -- of their dependencies installed, since we have no ability to
            -- reinstall them. This is especially important for using different
            -- minor versions of GHC, where the dependencies of wired-in
            -- packages may change slightly and therefore not match the
            -- snapshot.
            if name `HashSet.member` wiredInPackages
                then []
                else dpDepends dp
        , lhPair = (name, (version, fromMaybe Snap mloc, Library gid))
        }
    | otherwise = Nothing
  where
    toInclude =
        case Map.lookup name sourceMap of
            Nothing ->
                case mloc of
                    -- The sourceMap has nothing to say about this global
                    -- package, so we can use it
                    Nothing -> True
                    -- For non-global packages, don't include unknown packages.
                    -- See:
                    -- https://github.com/commercialhaskell/stack/issues/292
                    Just _ -> False

            Just pii ->
                version == piiVersion pii -- only accept the desired version
                && checkLocation (piiLocation pii)

    -- Ensure that the installed location matches where the sourceMap says it
    -- should be installed
    checkLocation Snap = mloc /= Just Local -- we can allow either global or snap
    checkLocation Local = mloc == Just Local

    gid = dpGhcPkgId dp
    PackageIdentifier name version = ghcPkgIdPackageIdentifier gid
