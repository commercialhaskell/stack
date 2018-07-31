{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
-- | Shared types for various stackage packages.
module Stack.Types.BuildPlan
    ( -- * Types
      SnapshotDef (..)
    , snapshotDefVC
    , ExeName (..)
    , LoadedSnapshot (..)
    , loadedSnapshotVC
    , LoadedPackageInfo (..)
    , ModuleName (..)
    , fromCabalModuleName
    , ModuleInfo (..)
    , moduleInfoVC
    , sdGlobalHints
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Aeson (ToJSON (..), (.=), object)
import           Data.Store.Version
import           Data.Store.VersionTagged
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Distribution.ModuleName as C
import qualified Distribution.Version as C
import           Pantry
import           Stack.Prelude
import           Stack.Types.Compiler
import           Stack.Types.GhcPkgId
import           Stack.Types.Resolver
import           Stack.Types.VersionIntervals

-- | A definition of a snapshot. This could be a Stackage snapshot or
-- something custom. It does not include information on the global
-- package database, this is added later.
--
-- It may seem more logic to attach flags, options, etc, directly with
-- the desired package. However, this isn't possible yet: our
-- definition may contain tarballs or Git repos, and we don't actually
-- know the package names contained there. Therefore, we capture all
-- of this additional information by package name, and later in the
-- snapshot load step we will resolve the contents of tarballs and
-- repos, figure out package names, and assigned values appropriately.
data SnapshotDef = SnapshotDef -- FIXME temporary
    { sdResolver        :: !LoadedResolver
    , sdResolverName    :: !Text
    -- ^ The resolver that provides this definition.
    , sdSnapshots       :: ![Snapshot]
    , sdWantedCompilerVersion :: !WantedCompiler
    , sdUniqueHash :: !StaticSHA256
    }
    deriving (Show, Eq, Data, Generic, Typeable)
instance Store SnapshotDef
instance NFData SnapshotDef

sdGlobalHints :: SnapshotDef -> Map PackageName (Maybe Version)
sdGlobalHints = Map.unions . map snapshotGlobalHints . sdSnapshots

snapshotDefVC :: VersionConfig SnapshotDef
snapshotDefVC = storeVersionConfig "sd-v3" "MpkgNx8qOHakJTSePR1czDElNiU="

-- | Name of an executable.
newtype ExeName = ExeName { unExeName :: Text }
    deriving (Show, Eq, Ord, Hashable, IsString, Generic, Store, NFData, Data, Typeable)

-- | A fully loaded snapshot combined , including information gleaned from the
-- global database and parsing cabal files.
--
-- Invariant: a global package may not depend upon a snapshot package,
-- a snapshot may not depend upon a local or project, and all
-- dependencies must be satisfied.
data LoadedSnapshot = LoadedSnapshot
  { lsCompilerVersion :: !ActualCompiler
  , lsGlobals         :: !(Map PackageName (LoadedPackageInfo GhcPkgId))
  , lsPackages        :: !(Map PackageName (LoadedPackageInfo PackageLocationOrPath))
  -- ^ Snapshots themselves may not have a filepath in them, but once
  -- we start adding in local configuration it's possible.
  }
    deriving (Generic, Show, Data, Eq, Typeable)
instance Store LoadedSnapshot
instance NFData LoadedSnapshot

loadedSnapshotVC :: VersionConfig LoadedSnapshot
loadedSnapshotVC = storeVersionConfig "ls-v6" "onyC94ATlh8WmpG_DktKl-g12BU="

-- | Information on a single package for the 'LoadedSnapshot' which
-- can be installed.
--
-- Note that much of the information below (such as the package
-- dependencies or exposed modules) can be conditional in the cabal
-- file, which means it will vary based on flags, arch, and OS.
data LoadedPackageInfo loc = LoadedPackageInfo
    { lpiVersion :: !Version
    -- ^ This /must/ match the version specified within 'rpiDef'.
    , lpiLocation :: !loc
    -- ^ Where to get the package from. This could be a few different
    -- things:
    --
    -- * For a global package, it will be the @GhcPkgId@. (If we end
    -- up needing to rebuild this because we've changed a
    -- dependency, we will take it from the package index with no
    -- @CabalFileInfo@.
    --
    -- * For a dependency, it will be a @PackageLocation@.
    --
    -- * For a project package, it will be a @Path Abs Dir@.
    , lpiFlags :: !(Map FlagName Bool)
    -- ^ Flags to build this package with.
    , lpiGhcOptions :: ![Text]
    -- ^ GHC options to use when building this package.
    , lpiPackageDeps :: !(Map PackageName VersionIntervals)
    -- ^ All packages which must be built/copied/registered before
    -- this package.
    , lpiExposedModules :: !(Set ModuleName)
    -- ^ Modules exposed by this package's library
    , lpiHide :: !Bool
    -- ^ Should this package be hidden in the database. Affects the
    -- script interpreter's module name import parser.
    }
    deriving (Generic, Show, Eq, Data, Typeable, Functor)
instance Store a => Store (LoadedPackageInfo a)
instance NFData a => NFData (LoadedPackageInfo a)

data DepInfo = DepInfo
    { _diComponents :: !(Set Component)
    , _diRange      :: !VersionIntervals
    }
    deriving (Generic, Show, Eq, Data, Typeable)
instance Store DepInfo
instance NFData DepInfo

instance Semigroup DepInfo where
    DepInfo a x <> DepInfo b y = DepInfo
        (mappend a b)
        (intersectVersionIntervals x y)

instance Monoid DepInfo where
    mempty = DepInfo mempty (fromVersionRange C.anyVersion)
    mappend = (<>)

data Component = CompLibrary
               | CompExecutable
               | CompTestSuite
               | CompBenchmark
    deriving (Generic, Show, Eq, Ord, Data, Typeable, Enum, Bounded)
instance Store Component
instance NFData Component

newtype ModuleName = ModuleName { unModuleName :: ByteString }
  deriving (Show, Eq, Ord, Generic, Store, NFData, Typeable, Data)

fromCabalModuleName :: C.ModuleName -> ModuleName
fromCabalModuleName = ModuleName . encodeUtf8 . T.intercalate "." . map T.pack . C.components

newtype ModuleInfo = ModuleInfo
    { miModules      :: Map ModuleName (Set PackageName)
    }
  deriving (Show, Eq, Ord, Generic, Typeable, Data)
instance Store ModuleInfo
instance NFData ModuleInfo

instance Semigroup ModuleInfo where
  ModuleInfo x <> ModuleInfo y =
    ModuleInfo (Map.unionWith Set.union x y)

instance Monoid ModuleInfo where
  mempty = ModuleInfo mempty
  mappend = (<>)

moduleInfoVC :: VersionConfig ModuleInfo
moduleInfoVC = storeVersionConfig "mi-v2" "8ImAfrwMVmqoSoEpt85pLvFeV3s="
