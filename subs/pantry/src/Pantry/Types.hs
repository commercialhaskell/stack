{-# LANGUAGE NoImplicitPrelude #-}
module Pantry.Types
  ( PantryConfig (..)
  , HackageSecurityConfig (..)
  , Storage (..)
  , HasPantryConfig (..)
  , BlobKey (..)
  ) where

import RIO
import Crypto.Hash
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)

newtype Storage = Storage (Pool SqlBackend)

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

newtype BlobKey = BlobKey (Digest SHA256)
