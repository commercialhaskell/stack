{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A ghc-pkg id.

module Stackage.GhcPkgId where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Data
import           Data.Hashable
import           GHC.Generics

-- | A ghc-pkg package identifier.
newtype GhcPkgId = GhcPkgId ByteString
  deriving (Eq,Ord,Data,Typeable,Generic)

instance Hashable GhcPkgId

instance Show GhcPkgId where
  show (GhcPkgId x) = S8.unpack x
