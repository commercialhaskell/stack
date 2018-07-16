{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Pantry.Types
  ( PantryConfig (..)
  , HackageSecurityConfig (..)
  , Storage (..)
  , HasPantryConfig (..)
  , BlobKey (..)
  , PackageName
  , Version
  , CabalHash (..)
  ) where

import RIO
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend, PersistField, PersistFieldSql)
import Pantry.StaticSHA256
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.Version (Version)
import Data.Store (Store) -- FIXME remove

newtype Storage = Storage (Pool SqlBackend)

-- | A cryptographic hash of a Cabal file.
newtype CabalHash = CabalHash { unCabalHash :: StaticSHA256 }
    deriving (Generic, Show, Eq, NFData, Data, Typeable, Ord, Hashable, Store)

data PantryConfig = PantryConfig
  { pcHackageSecurity :: !HackageSecurityConfig
  , pcRootDir :: !FilePath
  , pcStorage :: !Storage
  }

data HackageSecurityConfig = HackageSecurityConfig
  { hscKeyIds :: ![Text]
  , hscKeyThreshold :: !Int
  , hscDownloadPrefix :: !Text
  }

class HasPantryConfig env where
  pantryConfigL :: Lens' env PantryConfig

newtype BlobKey = BlobKey StaticSHA256
  deriving (PersistField, PersistFieldSql)
