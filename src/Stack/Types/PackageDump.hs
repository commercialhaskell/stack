{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Stack.Types.PackageDump
    ( InstalledCache(..)
    , InstalledCacheInner(..)
    , InstalledCacheEntry(..)
    , installedCacheVC
    ) where

import Data.Data
import Data.IORef
import Data.Map (Map)
import Data.Store
import Data.Store.Version
import Data.Store.VersionTagged
import GHC.Generics (Generic)
import Stack.Types.GhcPkgId
import Stack.Types.PackageIdentifier

-- | Cached information on whether package have profiling libraries and haddocks.
newtype InstalledCache = InstalledCache (IORef InstalledCacheInner)
newtype InstalledCacheInner = InstalledCacheInner (Map GhcPkgId InstalledCacheEntry)
    deriving (Store, Generic, Eq, Show, Data, Typeable)

-- | Cached information on whether a package has profiling libraries and haddocks.
data InstalledCacheEntry = InstalledCacheEntry
    { installedCacheProfiling :: !Bool
    , installedCacheHaddock :: !Bool
    , installedCacheSymbols :: !Bool
    , installedCacheIdent :: !PackageIdentifier }
    deriving (Eq, Generic, Show, Data, Typeable)
instance Store InstalledCacheEntry

installedCacheVC :: VersionConfig InstalledCacheInner
installedCacheVC = storeVersionConfig "installed-v1" "GGyaE6qY9FOqeWtozuadKqS7_QM="
