{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- Determine which packages are already installed
module Stack.Build.Installed
    ( InstalledMap
    , Installed (..)
    , GetInstalledOpts (..)
    , getInstalled
    ) where

import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Foldable as F
import qualified Data.HashSet as HashSet
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict as Map
import           Path
import           Stack.Build.Cache
import           Stack.Constants
import           Stack.PackageDump
import           Stack.Prelude
import           Stack.Types.Build
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.GhcPkgId
import           Stack.Types.Package
import           Stack.Types.PackageDump
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version

-- | Options for 'getInstalled'.
data GetInstalledOpts = GetInstalledOpts
    { getInstalledProfiling :: !Bool
      -- ^ Require profiling libraries?
    , getInstalledHaddock   :: !Bool
      -- ^ Require haddocks?
    , getInstalledSymbols   :: !Bool
      -- ^ Require debugging symbols?
    }

-- | Returns the new InstalledMap and all of the locally registered packages.
getInstalled :: HasEnvConfig env
             => GetInstalledOpts
             -> Map PackageName PackageSource -- ^ does not contain any installed information
             -> RIO env
                  ( InstalledMap
                  , [DumpPackage () () ()] -- globally installed
                  , [DumpPackage () () ()] -- snapshot installed
                  , [DumpPackage () () ()] -- locally installed
                  )
getInstalled opts sourceMap = do
    logDebug "Finding out which packages are already installed"
    snapDBPath <- packageDatabaseDeps
    localDBPath <- packageDatabaseLocal
    extraDBPaths <- packageDatabaseExtra

    mcache <-
        if getInstalledProfiling opts || getInstalledHaddock opts
            then configInstalledCache >>= liftM Just . loadInstalledCache
            else return Nothing

    let loadDatabase' = loadDatabase opts mcache sourceMap

    (installedLibs0, globalDumpPkgs) <- loadDatabase' Nothing []
    (installedLibs1, _extraInstalled) <-
      foldM (\lhs' pkgdb ->
        loadDatabase' (Just (ExtraGlobal, pkgdb)) (fst lhs')
        ) (installedLibs0, globalDumpPkgs) extraDBPaths
    (installedLibs2, snapshotDumpPkgs) <-
        loadDatabase' (Just (InstalledTo Snap, snapDBPath)) installedLibs1
    (installedLibs3, localDumpPkgs) <-
        loadDatabase' (Just (InstalledTo Local, localDBPath)) installedLibs2
    let installedLibs = M.fromList $ map lhPair installedLibs3

    F.forM_ mcache $ \cache -> do
        icache <- configInstalledCache
        saveInstalledCache icache cache

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
loadDatabase :: HasEnvConfig env
             => GetInstalledOpts
             -> Maybe InstalledCache -- ^ if Just, profiling or haddock is required
             -> Map PackageName PackageSource -- ^ to determine which installed things we should include
             -> Maybe (InstalledPackageLocation, Path Abs Dir) -- ^ package database, Nothing for global
             -> [LoadHelper] -- ^ from parent databases
             -> RIO env ([LoadHelper], [DumpPackage () () ()])
loadDatabase opts mcache sourceMap mdb lhs0 = do
    wc <- view $ actualCompilerVersionL.to whichCompiler
    (lhs1', dps) <- ghcPkgDump wc (fmap snd (maybeToList mdb))
                $ conduitDumpPackage .| sink
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
    conduitSymbolsCache =
        case mcache of
            Just cache | getInstalledSymbols opts -> addSymbols cache
            -- Just an optimization to avoid calculating the debugging
            -- symbol values when they aren't necessary
            _ -> CL.map (\dp -> dp { dpSymbols = False })
    mloc = fmap fst mdb
    sinkDP = conduitProfilingCache
           .| conduitHaddockCache
           .| conduitSymbolsCache
           .| CL.map (isAllowed opts mcache sourceMap mloc &&& toLoadHelper mloc)
           .| CL.consume
    sink = getZipSink $ (,)
        <$> ZipSink sinkDP
        <*> ZipSink CL.consume

processLoadResult :: HasLogFunc env
                  => Maybe (InstalledPackageLocation, Path Abs Dir)
                  -> Bool
                  -> (Allowed, LoadHelper)
                  -> RIO env (Maybe LoadHelper)
processLoadResult _ _ (Allowed, lh) = return (Just lh)
processLoadResult _ True (WrongVersion actual wanted, lh)
    -- Allow some packages in the ghcjs global DB to have the wrong
    -- versions.  Treat them as wired-ins by setting deps to [].
    | fst (lhPair lh) `HashSet.member` ghcjsBootPackages = do
        logWarn $
            "Ignoring that the GHCJS boot package \"" <>
            display (packageNameText (fst (lhPair lh))) <>
            "\" has a different version, " <>
            display (versionText actual) <>
            ", than the resolver's wanted version, " <>
            display (versionText wanted)
        return (Just lh)
processLoadResult mdb _ (reason, lh) = do
    logDebug $
        "Ignoring package " <>
        display (packageNameText (fst (lhPair lh))) <>
        maybe mempty (\db -> ", from " <> displayShow db <> ",") mdb <>
        " due to" <>
        case reason of
            Allowed -> " the impossible?!?!"
            NeedsProfiling -> " it needing profiling."
            NeedsHaddock -> " it needing haddocks."
            NeedsSymbols -> " it needing debugging symbols."
            UnknownPkg -> " it being unknown to the resolver / extra-deps."
            WrongLocation mloc loc -> " wrong location: " <> displayShow (mloc, loc)
            WrongVersion actual wanted ->
                " wanting version " <>
                display (versionText wanted) <>
                " instead of " <>
                display (versionText actual)
    return Nothing

data Allowed
    = Allowed
    | NeedsProfiling
    | NeedsHaddock
    | NeedsSymbols
    | UnknownPkg
    | WrongLocation (Maybe InstalledPackageLocation) InstallLocation
    | WrongVersion Version Version
    deriving (Eq, Show)

-- | Check if a can be included in the set of installed packages or not, based
-- on the package selections made by the user. This does not perform any
-- dirtiness or flag change checks.
isAllowed :: GetInstalledOpts
          -> Maybe InstalledCache
          -> Map PackageName PackageSource
          -> Maybe InstalledPackageLocation
          -> DumpPackage Bool Bool Bool
          -> Allowed
isAllowed opts mcache sourceMap mloc dp
    -- Check that it can do profiling if necessary
    | getInstalledProfiling opts && isJust mcache && not (dpProfiling dp) = NeedsProfiling
    -- Check that it has haddocks if necessary
    | getInstalledHaddock opts && isJust mcache && not (dpHaddock dp) = NeedsHaddock
    -- Check that it has haddocks if necessary
    | getInstalledSymbols opts && isJust mcache && not (dpSymbols dp) = NeedsSymbols
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

toLoadHelper :: Maybe InstalledPackageLocation -> DumpPackage Bool Bool Bool -> LoadHelper
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
    , lhPair = (name, (toPackageLocation mloc, Library ident gid (Right <$> dpLicense dp)))
    }
  where
    gid = dpGhcPkgId dp
    ident@(PackageIdentifier name _) = dpPackageIdent dp

toPackageLocation :: Maybe InstalledPackageLocation -> InstallLocation
toPackageLocation Nothing = Snap
toPackageLocation (Just ExtraGlobal) = Snap
toPackageLocation (Just (InstalledTo loc)) = loc
