{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

-- Determine which packages are already installed
module Stack.Build.Installed
  ( InstalledMap
  , Installed (..)
  , getInstalled
  , InstallMap
  , toInstallMap
  ) where

import           Data.Conduit ( ZipSink (..), getZipSink )
import qualified Data.Conduit.List as CL
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import           Stack.Build.Cache ( getInstalledExes )
import           Stack.Constants ( wiredInPackages )
import           Stack.PackageDump
                   ( conduitDumpPackage, ghcPkgDump, pruneDeps )
import           Stack.Prelude
import           Stack.SourceMap ( getPLIVersion, loadVersion )
import           Stack.Types.Config
                   ( DumpPackage (..), HasEnvConfig, getGhcPkgExe
                   , packageDatabaseDeps, packageDatabaseExtra
                   , packageDatabaseLocal
                   )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.Package
                   ( InstallLocation (..), InstallMap, Installed (..)
                   , InstalledMap, InstalledPackageLocation (..)
                   )
import           Stack.Types.SourceMap
                   ( DepPackage (..), ProjectPackage (..), SourceMap (..) )

toInstallMap :: MonadIO m => SourceMap -> m InstallMap
toInstallMap sourceMap = do
  projectInstalls <-
    for (smProject sourceMap) $ \pp -> do
      version <- loadVersion (ppCommon pp)
      pure (Local, version)
  depInstalls <-
    for (smDeps sourceMap) $ \dp ->
      case dpLocation dp of
        PLImmutable pli -> pure (Snap, getPLIVersion pli)
        PLMutable _ -> do
          version <- loadVersion (dpCommon dp)
          pure (Local, version)
  pure $ projectInstalls <> depInstalls

-- | Returns the new InstalledMap and all of the locally registered packages.
getInstalled :: HasEnvConfig env
             => InstallMap -- ^ does not contain any installed information
             -> RIO env
                  ( InstalledMap
                  , [DumpPackage] -- globally installed
                  , [DumpPackage] -- snapshot installed
                  , [DumpPackage] -- locally installed
                  )
getInstalled {-opts-} installMap = do
  logDebug "Finding out which packages are already installed"
  snapDBPath <- packageDatabaseDeps
  localDBPath <- packageDatabaseLocal
  extraDBPaths <- packageDatabaseExtra

  let loadDatabase' = loadDatabase {-opts mcache-} installMap

  (installedLibs0, globalDumpPkgs) <- loadDatabase' Nothing []
  (installedLibs1, _extraInstalled) <-
    foldM (\lhs' pkgdb ->
      loadDatabase' (Just (ExtraGlobal, pkgdb)) (fst lhs')
      ) (installedLibs0, globalDumpPkgs) extraDBPaths
  (installedLibs2, snapshotDumpPkgs) <-
    loadDatabase' (Just (InstalledTo Snap, snapDBPath)) installedLibs1
  (installedLibs3, localDumpPkgs) <-
    loadDatabase' (Just (InstalledTo Local, localDBPath)) installedLibs2
  let installedLibs = Map.fromList $ map lhPair installedLibs3

  -- Add in the executables that are installed, making sure to only trust a
  -- listed installation under the right circumstances (see below)
  let exesToSM loc = Map.unions . map (exeToSM loc)
      exeToSM loc (PackageIdentifier name version) =
        case Map.lookup name installMap of
          -- Doesn't conflict with anything, so that's OK
          Nothing -> m
          Just (iLoc, iVersion)
            -- Not the version we want, ignore it
            | version /= iVersion || mismatchingLoc loc iLoc -> Map.empty
            | otherwise -> m
       where
        m = Map.singleton name (loc, Executable $ PackageIdentifier name version)
        mismatchingLoc installed target
          | target == installed = False
          | installed == Local = False -- snapshot dependency could end up
                                       -- in a local install as being mutable
          | otherwise = True
  exesSnap <- getInstalledExes Snap
  exesLocal <- getInstalledExes Local
  let installedMap = Map.unions
        [ exesToSM Local exesLocal
        , exesToSM Snap exesSnap
        , installedLibs
        ]

  pure ( installedMap
       , globalDumpPkgs
       , snapshotDumpPkgs
       , localDumpPkgs
       )

-- | Outputs both the modified InstalledMap and the Set of all installed
-- packages in this database
--
-- The goal is to ascertain that the dependencies for a package are present,
-- that it has profiling if necessary, and that it matches the version and
-- location needed by the SourceMap
loadDatabase ::
     HasEnvConfig env
  => InstallMap -- ^ to determine which installed things we should include
  -> Maybe (InstalledPackageLocation, Path Abs Dir)
     -- ^ package database, Nothing for global
  -> [LoadHelper] -- ^ from parent databases
  -> RIO env ([LoadHelper], [DumpPackage])
loadDatabase installMap mdb lhs0 = do
  pkgexe <- getGhcPkgExe
  (lhs1', dps) <- ghcPkgDump pkgexe (fmap snd (maybeToList mdb)) $
                    conduitDumpPackage .| sink
  lhs1 <- mapMaybeM (processLoadResult mdb) lhs1'
  let lhs = pruneDeps id lhId lhDeps const (lhs0 ++ lhs1)
  pure (map (\lh -> lh { lhDeps = [] }) $ Map.elems lhs, dps)
 where
  mloc = fmap fst mdb
  sinkDP =  CL.map (isAllowed installMap mloc &&& toLoadHelper mloc)
         .| CL.consume
  sink =   getZipSink $ (,)
       <$> ZipSink sinkDP
       <*> ZipSink CL.consume

processLoadResult :: HasLogFunc env
                  => Maybe (InstalledPackageLocation, Path Abs Dir)
                  -> (Allowed, LoadHelper)
                  -> RIO env (Maybe LoadHelper)
processLoadResult _ (Allowed, lh) = pure (Just lh)
processLoadResult mdb (reason, lh) = do
  logDebug $
       "Ignoring package "
    <> fromString (packageNameString (fst (lhPair lh)))
    <> maybe
         mempty
         ( \db ->    ", from "
                  <> displayShow db
                  <> ","
         )
         mdb
    <> " due to"
    <> case reason of
         UnknownPkg -> " it being unknown to the resolver / extra-deps."
         WrongLocation mloc loc -> " wrong location: " <> displayShow (mloc, loc)
         WrongVersion actual wanted ->
              " wanting version "
          <> fromString (versionString wanted)
          <> " instead of "
          <> fromString (versionString actual)
  pure Nothing

data Allowed
  = Allowed
  | UnknownPkg
  | WrongLocation (Maybe InstalledPackageLocation) InstallLocation
  | WrongVersion Version Version
  deriving (Eq, Show)

-- | Check if a can be included in the set of installed packages or not, based
-- on the package selections made by the user. This does not perform any
-- dirtiness or flag change checks.
isAllowed :: InstallMap
          -> Maybe InstalledPackageLocation
          -> DumpPackage
          -> Allowed
isAllowed installMap mloc dp = case Map.lookup name installMap of
  Nothing ->
    -- If the sourceMap has nothing to say about this package,
    -- check if it represents a sublibrary first
    -- See: https://github.com/commercialhaskell/stack/issues/3899
    case dpParentLibIdent dp of
      Just (PackageIdentifier parentLibName version') ->
        case Map.lookup parentLibName installMap of
          Nothing -> checkNotFound
          Just instInfo
            | version' == version -> checkFound instInfo
            | otherwise -> checkNotFound -- different versions
      Nothing -> checkNotFound
  Just pii -> checkFound pii
 where
  PackageIdentifier name version = dpPackageIdent dp
  -- Ensure that the installed location matches where the sourceMap says it
  -- should be installed
  checkLocation Snap = True -- snapshot deps could become mutable after getting
                            -- any mutable dependency
  checkLocation Local =
    mloc == Just (InstalledTo Local) || mloc == Just ExtraGlobal -- 'locally' installed snapshot packages can come from extra dbs
  -- Check if a package is allowed if it is found in the sourceMap
  checkFound (installLoc, installVer)
    | not (checkLocation installLoc) = WrongLocation mloc installLoc
    | version /= installVer = WrongVersion version installVer
    | otherwise = Allowed
  -- check if a package is allowed if it is not found in the sourceMap
  checkNotFound = case mloc of
    -- The sourceMap has nothing to say about this global package, so we can use it
    Nothing -> Allowed
    Just ExtraGlobal -> Allowed
    -- For non-global packages, don't include unknown packages.
    -- See: https://github.com/commercialhaskell/stack/issues/292
    Just _ -> UnknownPkg

data LoadHelper = LoadHelper
  { lhId   :: !GhcPkgId
  , lhDeps :: ![GhcPkgId]
  , lhPair :: !(PackageName, (InstallLocation, Installed))
  }
  deriving Show

toLoadHelper :: Maybe InstalledPackageLocation -> DumpPackage -> LoadHelper
toLoadHelper mloc dp = LoadHelper
  { lhId = gid
  , lhDeps =
      -- We always want to consider the wired in packages as having all of their
      -- dependencies installed, since we have no ability to reinstall them.
      -- This is especially important for using different minor versions of GHC,
      -- where the dependencies of wired-in packages may change slightly and
      -- therefore not match the snapshot.
      if name `Set.member` wiredInPackages
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
