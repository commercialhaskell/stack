{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A ghc-pkg id.

module Stack.GhcPkgId
  (GhcPkgId
  ,parseGhcPkgIdFromString)
  where

import           Control.Monad.Catch
import           Data.Aeson
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

instance FromJSON GhcPkgId where
  parseJSON x =
    do s <- parseJSON x
       let !b = S8.pack s
       return (GhcPkgId b)

instance ToJSON GhcPkgId where
  toJSON (GhcPkgId x) =
    toJSON (S8.unpack x)

-- | Parse a GHC package id from a string.
parseGhcPkgIdFromString :: MonadThrow m => String -> m GhcPkgId
parseGhcPkgIdFromString = return . GhcPkgId . S8.pack
