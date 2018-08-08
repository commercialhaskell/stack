{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Unpack packages and write out a stack.yaml
module Curator.Unpack
  ( unpackSnapshot
  ) where

import RIO
import Pantry
import Curator.Types
import Path

unpackSnapshot
  :: ()
  => Constraints
  -> Snapshot
  -> Path Abs Dir
  -> RIO env ()
unpackSnapshot = undefined