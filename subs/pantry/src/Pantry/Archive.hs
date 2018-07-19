{-# LANGUAGE NoImplicitPrelude #-}
-- | Logic for loading up trees from HTTPS archives.
module Pantry.Archive
  ( getArchive
  ) where

import RIO
import Pantry.StaticSHA256
import Pantry.Storage
import Pantry.Types

getArchive
  :: (HasPantryConfig env, HasLogFunc env)
  => Text -- ^ URL
  -> Maybe StaticSHA256 -- ^ hash of the raw file
  -> Maybe Word -- ^ size of the raw file
  -> RIO env (TreeSId, TreeKey, Tree)
getArchive = undefined
