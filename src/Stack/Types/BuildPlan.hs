{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
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
    , sdSnapshots
    , sdResolverName
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
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
    { sdResolver        :: !SnapshotLocation
    , sdSnapshot        :: !(Maybe (Snapshot, SnapshotDef))
    , sdWantedCompilerVersion :: !WantedCompiler
    , sdUniqueHash :: !StaticSHA256
    }
    deriving (Show, Eq, Data, Generic, Typeable)
instance Store SnapshotDef
instance NFData SnapshotDef

sdResolverName :: SnapshotDef -> Text
sdResolverName sd =
  case sdSnapshot sd of
    Nothing -> utf8BuilderToText $ display $ sdWantedCompilerVersion sd
    Just (snapshot, _) -> snapshotName snapshot

sdSnapshots :: SnapshotDef -> [Snapshot]
sdSnapshots sd =
  case sdSnapshot sd of
    Nothing -> []
    Just (snap, sd') -> snap : sdSnapshots sd'

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
  , lsPackages        :: !(Map PackageName (LoadedPackageInfo PackageLocation))
  -- ^ Snapshots themselves may not have a filepath in them, but once
  -- we start adding in local configuration it's possible.
  }
    deriving (Generic, Show, Data, Eq, Typeable)
instance Store LoadedSnapshot
instance NFData LoadedSnapshot

{-

MSS 2018-08-02: There's a big refactoring laid out in
https://github.com/commercialhaskell/stack/issues/3922. While working
on the pantry refactoring, I think I found a straightforward way to
approach implementing this (though there will still be a lot of code
churn involved). I don't want to lose the idea, but I also don't want
to include this change in the pantry work, so writing a note here.

Right now, we eagerly load up all packages in a snapshot the first
time we use it. This was necessary for build tool dependencies in the
past, but not anymore
(https://github.com/commercialhaskell/stack/pull/4132). Therefore:
let's delete the @LoadedSnapshot@ data type entirely!

Once you start down this path, you'll get to a point of not using the
@calculatePackagePromotion@ stuff as much. This is good! Delete that
function too!

Instead, we have a @SnapshotLocation@, which can be turned into a
@Snapshot@ via @loadPantrySnapshot@. We want to traverse that
@Snapshot@ and all of its parent @Snapshot@s and come up with a few
pieces of information:

* The wanted compiler version

* A @type SourceMap = Map PackageName PackageSource@

We'll want to augment that @SourceMap@ with information from the
@stack.yaml@ file, namely: extra-deps and local packages. We'll also
need to extend it with command line parameters, such as if a user runs
@stack build acme-missiles-0.3@.

There will be a lot of information in @PackageSource@ taken from these
various sources, but it will contain information on where the package
is from, flags, GHC options, and so on, whether it's a dependency or
part of the project, etc.

It will be easy to see if a package is _immutable_ or not: everything
but local file paths are immutable. Awesome.

In ConstructPlan, when figuring out dependencies of a package, we'll
use a simple rule: if the package and all of its dependencies are
immutable, we stick it in the precompiled cache, with a hash based on
the full transitive set of dependencies and their
configuration. Otherwise, we don't cache.


-}

loadedSnapshotVC :: VersionConfig LoadedSnapshot
loadedSnapshotVC = storeVersionConfig "ls-v6" "7BcCWNHwk_2JZXi8E1mTe84y0Cc="

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
