{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Determine which packages are already installed
module Stack.Build.Installed
  ( getInstalled
  , toInstallMap
  ) where

import           Data.Conduit ( ZipSink (..), getZipSink )
import qualified Data.Conduit.List as CL
import           Data.Foldable ( Foldable (..) )
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import           Stack.Build.Cache ( getInstalledExes )
import           Stack.Constants ( wiredInPackages )
import           Stack.PackageDump
                   ( conduitDumpPackage, ghcPkgDump, pruneDeps )
import           Stack.Prelude
import           Stack.SourceMap ( getPLIVersion, loadVersion )
import           Stack.Types.CompilerPaths ( getGhcPkgExe )
import           Stack.Types.DumpPackage
                   ( DumpPackage (..), SublibDump (..), dpParentLibIdent
                   , sdPackageName
                   )
import           Stack.Types.EnvConfig
                    ( HasEnvConfig, packageDatabaseDeps, packageDatabaseExtra
                    , packageDatabaseLocal
                    )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.Installed
                   ( InstallLocation (..), InstallMap, Installed (..)
                   , InstalledLibraryInfo (..), InstalledMap
                   , InstalledPackageLocation (..), PackageDatabase (..)
                   , PackageDbVariety (..), toPackageDbVariety
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

  (installedLibs0, globalDumpPkgs) <- loadDatabase' GlobalPkgDb []
  (installedLibs1, _extraInstalled) <-
    foldM (\lhs' pkgdb ->
      loadDatabase' (UserPkgDb ExtraPkgDb pkgdb) (fst lhs')
      ) (installedLibs0, globalDumpPkgs) extraDBPaths
  (installedLibs2, snapshotDumpPkgs) <-
    loadDatabase' (UserPkgDb (InstalledTo Snap) snapDBPath) installedLibs1
  (installedLibs3, localDumpPkgs) <-
    loadDatabase' (UserPkgDb (InstalledTo Local) localDBPath) installedLibs2
  let installedLibs =
        foldr' gatherAndTransformSubLoadHelper mempty installedLibs3

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
-- location needed by the SourceMap.
loadDatabase ::
     forall env. HasEnvConfig env
  => InstallMap
     -- ^ to determine which installed things we should include
  -> PackageDatabase
     -- ^ package database.
  -> [LoadHelper]
     -- ^ from parent databases
  -> RIO env ([LoadHelper], [DumpPackage])
loadDatabase installMap db lhs0 = do
  pkgexe <- getGhcPkgExe
  (lhs1', dps) <- ghcPkgDump pkgexe pkgDb $ conduitDumpPackage .| sink
  lhs1 <- mapMaybeM processLoadResult lhs1'
  let lhs = pruneDeps id lhId lhDeps const (lhs0 ++ lhs1)
  pure (map (\lh -> lh { lhDeps = [] }) $ Map.elems lhs, dps)
 where
  pkgDb = case db of
    GlobalPkgDb -> []
    UserPkgDb _ fp -> [fp]

  sinkDP =  CL.map (isAllowed installMap db' &&& toLoadHelper db')
         .| CL.consume
   where
    db' = toPackageDbVariety db
  sink =   getZipSink $ (,)
       <$> ZipSink sinkDP
       <*> ZipSink CL.consume

  processLoadResult :: (Allowed, LoadHelper) -> RIO env (Maybe LoadHelper)
  processLoadResult (Allowed, lh) = pure (Just lh)
  processLoadResult (reason, lh) = do
    logDebug $
         "Ignoring package "
      <> fromPackageName (fst (lhPair lh))
      <> case db of
           GlobalPkgDb -> mempty
           UserPkgDb loc fp -> ", from " <> displayShow (loc, fp) <> ","
      <> " due to"
      <> case reason of
           UnknownPkg -> " it being unknown to the resolver / extra-deps."
           WrongLocation db' loc ->
             " wrong location: " <> displayShow (db', loc)
           WrongVersion actual wanted ->
                " wanting version "
            <> fromString (versionString wanted)
            <> " instead of "
            <> fromString (versionString actual)
    pure Nothing

-- | Type representing results of 'isAllowed'.
data Allowed
  = Allowed
    -- ^ The installed package can be included in the set of relevant installed
    -- packages.
  | UnknownPkg
    -- ^ The installed package cannot be included in the set of relevant
    -- installed packages because the package is unknown.
  | WrongLocation PackageDbVariety InstallLocation
    -- ^ The installed package cannot be included in the set of relevant
    -- installed packages because the package is in the wrong package database.
  | WrongVersion Version Version
    -- ^ The installed package cannot be included in the set of relevant
    -- installed packages because the package has the wrong version.
  deriving (Eq, Show)

-- | Check if an installed package can be included in the set of relevant
-- installed packages or not, based on the package selections made by the user.
-- This does not perform any dirtiness or flag change checks.
isAllowed ::
     InstallMap
  -> PackageDbVariety
     -- ^ The package database providing the installed package.
  -> DumpPackage
     -- ^ The installed package to check.
  -> Allowed
isAllowed installMap pkgDb dp = case Map.lookup name installMap of
  Nothing ->
    -- If the sourceMap has nothing to say about this package,
    -- check if it represents a sub-library first
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
  -- should be installed.
  checkLocation Snap =
     -- snapshot deps could become mutable after getting any mutable dependency.
    True
  checkLocation Local = case pkgDb of
    GlobalDb -> False
    -- 'locally' installed snapshot packages can come from 'extra' package
    -- databases.
    ExtraDb -> True
    WriteOnlyDb -> False
    MutableDb -> True
  -- Check if an installed package is allowed if it is found in the sourceMap.
  checkFound (installLoc, installVer)
    | not (checkLocation installLoc) = WrongLocation pkgDb installLoc
    | version /= installVer = WrongVersion version installVer
    | otherwise = Allowed
  -- Check if an installed package is allowed if it is not found in the
  -- sourceMap.
  checkNotFound = case pkgDb of
    -- The sourceMap has nothing to say about this global package, so we can use
    -- it.
    GlobalDb -> Allowed
    ExtraDb -> Allowed
    -- For non-global packages, don't include unknown packages.
    -- See: https://github.com/commercialhaskell/stack/issues/292
    WriteOnlyDb -> UnknownPkg
    MutableDb -> UnknownPkg

-- | Type representing certain information about an installed package.
data LoadHelper = LoadHelper
  { lhId :: !GhcPkgId
    -- ^ The package's id.
  , lhSublibrary :: !(Maybe SublibDump)
  , lhDeps :: ![GhcPkgId]
    -- ^ Unless the package's name is that of a 'wired-in' package, a list of
    -- the ids of the installed packages that are the package's dependencies.
  , lhPair :: !(PackageName, (InstallLocation, Installed))
    -- ^ A pair of (a) the package's name and (b) a pair of the relevant
    -- database (write-only or mutable) and information about the library
    -- installed.
  }
  deriving Show

toLoadHelper :: PackageDbVariety -> DumpPackage -> LoadHelper
toLoadHelper pkgDb dp = LoadHelper
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
  , lhSublibrary = dpSublib dp
  , lhPair =
      ( name
      , (toInstallLocation pkgDb, Library ident installedLibInfo)
      )
  }
 where
  installedLibInfo = InstalledLibraryInfo gid (Right <$> dpLicense dp) mempty
  gid = dpGhcPkgId dp
  ident@(PackageIdentifier name _) = dpPackageIdent dp

  toInstallLocation :: PackageDbVariety -> InstallLocation
  toInstallLocation GlobalDb = Snap
  toInstallLocation ExtraDb = Snap
  toInstallLocation WriteOnlyDb = Snap
  toInstallLocation MutableDb = Local

-- | This is where sublibraries and main libraries are assembled into a single
-- entity Installed package, where all ghcPkgId live.
gatherAndTransformSubLoadHelper ::
     LoadHelper
  -> Map PackageName (InstallLocation, Installed)
  -> Map PackageName (InstallLocation, Installed)
gatherAndTransformSubLoadHelper lh =
  Map.insertWith onPreviousLoadHelper key value
 where
  -- Here we assume that both have the same location which already was a prior
  -- assumption in Stack.
  onPreviousLoadHelper
      (pLoc, Library pn incomingLibInfo)
      (_, Library _ existingLibInfo)
    = ( pLoc
      , Library pn existingLibInfo
          { iliSublib = Map.union
              (iliSublib incomingLibInfo)
              (iliSublib existingLibInfo)
          , iliId = if isJust $ lhSublibrary lh
                      then iliId existingLibInfo
                      else iliId incomingLibInfo
          }
      )
  onPreviousLoadHelper newVal _oldVal = newVal
  (key, value) = case lhSublibrary lh of
    Nothing -> (rawPackageName, rawValue)
    Just sd -> (sdPackageName sd, updateAsSublib sd <$> rawValue)
  (rawPackageName, rawValue) = lhPair lh
  updateAsSublib
      sd
      (Library (PackageIdentifier _sublibMungedPackageName version) libInfo)
    = Library
        (PackageIdentifier key version)
        libInfo {iliSublib = Map.singleton (sdLibraryName sd) (iliId libInfo)}
  updateAsSublib _ v = v
