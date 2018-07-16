{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.Types
  ( PantryConfig (..)
  , HackageSecurityConfig (..)
  , Storage (..)
  , HasPantryConfig (..)
  , BlobKey (..)
  , PackageName
  , Version
  , CabalHash (..)
  , PackageNameP (..)
  , VersionP (..)
  ) where

import RIO
import qualified RIO.Text as T
import Data.Pool (Pool)
import Database.Persist
import Database.Persist.Sql
import Pantry.StaticSHA256
import Distribution.Types.PackageName (PackageName)
import qualified Distribution.Text
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

newtype PackageNameP = PackageNameP PackageName
instance PersistField PackageNameP where
  toPersistValue (PackageNameP pn) = PersistText $ T.pack $ Distribution.Text.display pn
  fromPersistValue v = do
    str <- fromPersistValue v
    case Distribution.Text.simpleParse str of
      Nothing -> Left $ "Invalid package name: " <> T.pack str
      Just pn -> Right $ PackageNameP pn
instance PersistFieldSql PackageNameP where
  sqlType _ = SqlString

newtype VersionP = VersionP Version
instance PersistField VersionP where
  toPersistValue (VersionP v) = PersistText $ T.pack $ Distribution.Text.display v
  fromPersistValue v = do
    str <- fromPersistValue v
    case Distribution.Text.simpleParse str of
      Nothing -> Left $ "Invalid version number: " <> T.pack str
      Just ver -> Right $ VersionP ver
instance PersistFieldSql VersionP where
  sqlType _ = SqlString
