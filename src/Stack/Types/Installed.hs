{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Stack.Types.Installed
License     : BSD-3-Clause

This module contains all the types related to the idea of installing a package
in the pkg-db or an executable on the file system.
-}

module Stack.Types.Installed
  ( InstallLocation (..)
  , InstalledPackageLocation (..)
  , PackageDatabase (..)
  , PackageDbVariety (..)
  , InstallMap
  , Installed (..)
  , InstalledMap
  , InstalledLibraryInfo (..)
  , toPackageDbVariety
  , installedLibraryInfoFromGhcPkgId
  , simpleInstalledLib
  , installedToPackageIdOpt
  , installedPackageIdentifier
  , installedVersion
  , foldOnGhcPkgId'
  ) where

import qualified Data.Map as M
import           Stack.Prelude
import           Stack.Types.ComponentUtils ( StackUnqualCompName )
import           Stack.Types.GhcPkgId ( GhcPkgId, ghcPkgIdString )

-- | Type representing user package databases that packages can be installed
-- into.
data InstallLocation
  = Snap
    -- ^ The write-only package database, formerly known as the snapshot
    -- database.
  | Local
    -- ^ The mutable package database, formerly known as the local database.
  deriving (Eq, Show)

instance Semigroup InstallLocation where
  Local <> _ = Local
  _ <> Local = Local
  Snap <> Snap = Snap

instance Monoid InstallLocation where
  mempty = Snap
  mappend = (<>)

-- | Type representing user (non-global) package databases that can provide
-- installed packages.
data InstalledPackageLocation
  = InstalledTo InstallLocation
    -- ^ A package database that a package can be installed into.
  | ExtraPkgDb
    -- ^ An \'extra\' package database, specified by @extra-package-dbs@.
  deriving (Eq, Show)

-- | Type representing package databases that can provide installed packages.
data PackageDatabase
  = GlobalPkgDb
    -- ^ GHC's global package database.
  | UserPkgDb InstalledPackageLocation (Path Abs Dir)
    -- ^ A user package database.
  deriving (Eq, Show)

-- | A function to yield the variety of package database for a given
-- package database that can provide installed packages.
toPackageDbVariety :: PackageDatabase -> PackageDbVariety
toPackageDbVariety GlobalPkgDb = GlobalDb
toPackageDbVariety (UserPkgDb ExtraPkgDb _) = ExtraDb
toPackageDbVariety (UserPkgDb (InstalledTo Snap) _) = WriteOnlyDb
toPackageDbVariety (UserPkgDb (InstalledTo Local) _) = MutableDb

-- | Type representing varieties of package databases that can provide
-- installed packages.
data PackageDbVariety
  = GlobalDb
    -- ^ GHC's global package database.
  | ExtraDb
    -- ^ An \'extra\' package database, specified by @extra-package-dbs@.
  | WriteOnlyDb
    -- ^ The write-only package database, for immutable packages.
  | MutableDb
    -- ^ The mutable package database.
  deriving (Eq, Show)

-- | Type synonym representing dictionaries of package names for a project's
-- packages and dependencies, and pairs of their relevant database (write-only
-- or mutable) and package versions.
type InstallMap = Map PackageName (InstallLocation, Version)

-- | Type synonym representing dictionaries of package names, and a pair of in
-- which package database the package is installed (write-only or mutable) and
-- information about what is installed.
type InstalledMap = Map PackageName (InstallLocation, Installed)

-- TODO: This may not be the best type, as it allows invalid values to be
-- represented.
data InstalledLibraryInfo = InstalledLibraryInfo
  { mMainGhcPkgId :: Maybe GhcPkgId
    -- ^ The main library, if present. If absent, there must be one or more
    -- installed sublibraries.
  , subLib :: Map StackUnqualCompName GhcPkgId
    -- ^ If there are no sublibraries, there must be a main library.
  }
  deriving (Eq, Show)

-- | Type representing information about what is installed.
data Installed
  = Library PackageIdentifier InstalledLibraryInfo
    -- ^ A library, including the ids of its installed packages.
  | Executable PackageIdentifier
    -- ^ An executable.
  deriving (Eq, Show)

installedLibraryInfoFromGhcPkgId :: GhcPkgId -> InstalledLibraryInfo
installedLibraryInfoFromGhcPkgId ghcPkgId =
  InstalledLibraryInfo (Just ghcPkgId) mempty

simpleInstalledLib ::
     PackageIdentifier
  -> Maybe GhcPkgId
     -- ^ The id of the installed main library, if any.
  -> Map StackUnqualCompName GhcPkgId
     -- ^ The id of any sublibraries.
  -> Installed
simpleInstalledLib pkgIdentifier mMainGhcPkgId =
  Library pkgIdentifier . InstalledLibraryInfo mMainGhcPkgId

installedToPackageIdOpt :: InstalledLibraryInfo -> [String]
installedToPackageIdOpt libInfo =
  let acc0 = toStr <$> maybeToList libInfo.mMainGhcPkgId
  in  M.foldr' (iterator (++)) acc0 libInfo.subLib
 where
  toStr ghcPkgId = "-package-id=" <> ghcPkgIdString ghcPkgId
  iterator op ghcPkgId acc = pure (toStr ghcPkgId) `op` acc

installedPackageIdentifier :: Installed -> PackageIdentifier
installedPackageIdentifier (Library pid _) = pid
installedPackageIdentifier (Executable pid) = pid

-- | A strict fold over the 'GhcPkgId' of the given installed package. This will
-- iterate on the main library (if any) and sublibraries (if any).
foldOnGhcPkgId' ::
     (Maybe StackUnqualCompName -> GhcPkgId -> a -> a)
  -> Installed
  -> a
  -> a
foldOnGhcPkgId' _ Executable{} res = res
foldOnGhcPkgId' fn (Library _ libInfo) res =
  M.foldrWithKey' (fn . Just) base libInfo.subLib
 where
  base = maybe res (\ghcPkgId -> fn Nothing ghcPkgId res) libInfo.mMainGhcPkgId

-- | Get the installed Version.
installedVersion :: Installed -> Version
installedVersion i =
  let PackageIdentifier _ version = installedPackageIdentifier i
  in  version
