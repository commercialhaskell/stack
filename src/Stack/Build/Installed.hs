{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Control.Monad.Catch          (MonadMask)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader         (MonadReader, asks)
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.List            as CL
import           Data.Function
import qualified Data.Foldable                as F
import qualified Data.HashSet                 as HashSet
import           Data.List
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import           Data.Maybe.Extra             (mapMaybeM)
import           Data.Monoid
import qualified Data.Text                    as T
import           Network.HTTP.Client.Conduit  (HasHttpManager)
import           Path
import           Prelude                      hiding (FilePath, writeFile)
import           Stack.Build.Cache
import           Stack.Types.Build
import           Stack.Types.Version
import           Stack.Constants
import           Stack.GhcPkg
import           Stack.PackageDump
import           Stack.Types
import           Stack.Types.Internal

type M env m = (MonadIO m,MonadReader env m,HasHttpManager env,HasEnvConfig env,MonadLogger m,MonadBaseControl IO m,MonadMask m,HasLogLevel env)

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
             -> m ( InstalledMap
                  , [DumpPackage () ()] -- globally installed
                  , [DumpPackage () ()] -- snapshot installed
                  , [DumpPackage () ()] -- locally installed
                  )
getInstalled menv opts sourceMap = do
    snapDBPath <- packageDatabaseDeps
    localDBPath <- packageDatabaseLocal
    extraDBPaths <- packageDatabaseExtra

    bconfig <- asks getBuildConfig

    mcache <-
        if getInstalledProfiling opts || getInstalledHaddock opts
            then liftM Just $ loadInstalledCache $ configInstalledCache bconfig
            else return Nothing

    let loadDatabase' = loadDatabase menv opts mcache sourceMap

    (installedLibs0, globalDumpPkgs) <- loadDatabase' Nothing []
    (installedLibs1, _extraInstalled) <-
      (foldM (\lhs' pkgdb ->
        loadDatabase' (Just (ExtraGlobal, pkgdb)) (fst lhs')
        ) (installedLibs0, globalDumpPkgs) extraDBPaths)
    (installedLibs2, snapshotDumpPkgs) <-
        loadDatabase' (Just (InstalledTo Snap, snapDBPath)) installedLibs1
    (installedLibs3, localDumpPkgs) <-
        loadDatabase' (Just (InstalledTo Local, localDBPath)) installedLibs2
    let installedLibs = M.fromList $ map lhPair installedLibs3

    F.forM_ mcache (saveInstalledCache (configInstalledCache bconfig))

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
            m = Map.singleton name (loc, Executable $ PackageIdentifier name version)
    exesSnap <- getInstalledExes Snap
    exesLocal <- getInstalledExes Local
    let installedMap = Map.unions
            [ exesToSM Local exesLocal
            , exesToSM Snap exesSnap
            , installedLibs
            ]

    return ( installedMap
           , globalDumpPkgs
           , snapshotDumpPkgs
           , localDumpPkgs
           )

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
             -> Maybe (InstalledPackageLocation, Path Abs Dir) -- ^ package database, Nothing for global
             -> [LoadHelper] -- ^ from parent databases
             -> m ([LoadHelper], [DumpPackage () ()])
loadDatabase menv opts mcache sourceMap mdb lhs0 = do
    wc <- getWhichCompiler
    (lhs1', dps) <- ghcPkgDump menv wc (fmap snd (maybeToList mdb))
                $ conduitDumpPackage =$ sink
    let ghcjsHack = wc == Ghcjs && isNothing mdb
    lhs1 <- mapMaybeM (processLoadResult mdb ghcjsHack) lhs1'
    let lhs = pruneDeps
            id
            lhId
            lhDeps
            const
            (lhs0 ++ lhs1)
    return (map (\lh -> lh { lhDeps = [] }) $ Map.elems lhs, dps)
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
    mloc = fmap fst mdb
    sinkDP = conduitProfilingCache
          =$ conduitHaddockCache
          =$ CL.map (\dp -> (isAllowed opts mcache sourceMap mloc dp, toLoadHelper mloc dp))
          =$ CL.consume
    sink = getZipSink $ (,)
        <$> ZipSink sinkDP
        <*> ZipSink CL.consume

processLoadResult :: MonadLogger m
                  => Maybe (InstalledPackageLocation, Path Abs Dir)
                  -> Bool
                  -> (Allowed, LoadHelper)
                  -> m (Maybe LoadHelper)
processLoadResult _ _ (Allowed, lh) = return (Just lh)
processLoadResult _ True (WrongVersion actual wanted, lh)
    -- Allow some packages in the ghcjs global DB to have the wrong
    -- versions.  Treat them as wired-ins by setting deps to [].
    | fst (lhPair lh) `HashSet.member` ghcjsBootPackages = do
        $logWarn $ T.concat
            [ "Ignoring that the GHCJS boot package \""
            , packageNameText (fst (lhPair lh))
            , "\" has a different version, "
            , versionText actual
            , ", than the resolver's wanted version, "
            , versionText wanted
            ]
        return (Just lh)
processLoadResult mdb _ (reason, lh) = do
    $logDebug $ T.concat $
        [ "Ignoring package "
        , packageNameText (fst (lhPair lh))
        ] ++
        maybe [] (\db -> [", from ", T.pack (show db), ","]) mdb ++
        [ " due to"
        , case reason of
            Allowed -> " the impossible?!?!"
            NeedsProfiling -> " it needing profiling."
            NeedsHaddock -> " it needing haddocks."
            UnknownPkg -> " it being unknown to the resolver / extra-deps."
            WrongLocation mloc loc -> " wrong location: " <> T.pack (show (mloc, loc))
            WrongVersion actual wanted -> T.concat
                [ " wanting version "
                , versionText wanted
                , " instead of "
                , versionText actual
                ]
        ]
    return Nothing

data Allowed
    = Allowed
    | NeedsProfiling
    | NeedsHaddock
    | UnknownPkg
    | WrongLocation (Maybe InstalledPackageLocation) InstallLocation
    | WrongVersion Version Version
    deriving (Eq, Show)

-- | Check if a can be included in the set of installed packages or not, based
-- on the package selections made by the user. This does not perform any
-- dirtiness or flag change checks.
isAllowed :: PackageInstallInfo pii
          => GetInstalledOpts
          -> Maybe InstalledCache
          -> Map PackageName pii
          -> Maybe InstalledPackageLocation
          -> DumpPackage Bool Bool
          -> Allowed
isAllowed opts mcache sourceMap mloc dp
    -- Check that it can do profiling if necessary
    | getInstalledProfiling opts && isJust mcache && not (dpProfiling dp) = NeedsProfiling
    -- Check that it has haddocks if necessary
    | getInstalledHaddock opts && isJust mcache && not (dpHaddock dp) = NeedsHaddock
    | otherwise =
        case Map.lookup name sourceMap of
            Nothing ->
                case mloc of
                    -- The sourceMap has nothing to say about this global
                    -- package, so we can use it
                    Nothing -> Allowed
                    Just ExtraGlobal -> Allowed
                    -- For non-global packages, don't include unknown packages.
                    -- See:
                    -- https://github.com/commercialhaskell/stack/issues/292
                    Just _ -> UnknownPkg
            Just pii
                | not (checkLocation (piiLocation pii)) -> WrongLocation mloc (piiLocation pii)
                | version /= piiVersion pii -> WrongVersion version (piiVersion pii)
                | otherwise -> Allowed
  where
    PackageIdentifier name version = dpPackageIdent dp
    -- Ensure that the installed location matches where the sourceMap says it
    -- should be installed
    checkLocation Snap = mloc /= Just (InstalledTo Local) -- we can allow either global or snap
    checkLocation Local = mloc == Just (InstalledTo Local) || mloc == Just ExtraGlobal -- 'locally' installed snapshot packages can come from extra dbs

data LoadHelper = LoadHelper
    { lhId   :: !GhcPkgId
    , lhDeps :: ![GhcPkgId]
    , lhPair :: !(PackageName, (InstallLocation, Installed))
    }
    deriving Show

toLoadHelper :: Maybe InstalledPackageLocation -> DumpPackage Bool Bool -> LoadHelper
toLoadHelper mloc dp = LoadHelper
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
    , lhPair = (name, (toPackageLocation mloc, Library ident gid))
    }
  where
    gid = dpGhcPkgId dp
    ident@(PackageIdentifier name _) = dpPackageIdent dp

toPackageLocation :: Maybe InstalledPackageLocation -> InstallLocation
toPackageLocation Nothing = Snap
toPackageLocation (Just ExtraGlobal) = Snap
toPackageLocation (Just (InstalledTo loc)) = loc
