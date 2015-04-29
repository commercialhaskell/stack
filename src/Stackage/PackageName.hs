-- | Names for packages.

module Stackage.PackageName
  (PackageName)
  where

import Data.ByteString (ByteString)

-- | A package name.
newtype PackageName =
  PackageName ByteString
  deriving (Eq,Ord)
