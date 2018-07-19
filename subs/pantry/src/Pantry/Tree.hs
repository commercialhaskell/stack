{-# LANGUAGE NoImplicitPrelude #-}
module Pantry.Tree
  ( unpackTree
  ) where

import RIO
import Pantry.Types

unpackTree
  :: (HasPantryConfig env, HasLogFunc env)
  => FilePath -- ^ dest dir, will be created if necessary
  -> Tree
  -> RIO env ()
unpackTree = undefined
