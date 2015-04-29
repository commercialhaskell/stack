-- | A ghc-pkg id.

module Stackage.GhcPkgId where

import Data.ByteString (ByteString)

newtype GhcPkgId = GhcPkgId ByteString
