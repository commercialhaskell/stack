{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Versions for packages.

module Stackage.PackageVersion
  (PackageVersion
  ,VersionRange
  ,packageVersionParser
  ,parsePackageVersion
  ,parsePackageVersionFromString)
  where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Data
import           Data.Hashable
import           Data.List
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Distribution.Version
import           GHC.Generics

-- | A package version.
newtype PackageVersion =
  PackageVersion {unPackageVersion :: Vector Int}
  deriving (Eq,Ord,Typeable,Data,Generic)

instance Hashable PackageVersion where
  hashWithSalt i = hashWithSalt i . V.toList . unPackageVersion

instance Show PackageVersion where
  show (PackageVersion v) =
    intercalate "."
                (map show (V.toList v))

-- | Attoparsec parser for a package version from bytestring.
packageVersionParser :: Parser PackageVersion
packageVersionParser =
  do ls <- ((:) <$> num <*> many num')
     let !v = V.fromList ls
     return (PackageVersion v)
  where num = decimal
        num' = point *> num
        point = satisfy (== '.')

-- | Convenient way to parse a package version from a bytestring.
parsePackageVersion :: ByteString -> Maybe PackageVersion
parsePackageVersion =
  either (const Nothing) Just .
  parseOnly (packageVersionParser <* endOfInput)

-- | Migration function.
parsePackageVersionFromString :: String -> Maybe PackageVersion
parsePackageVersionFromString =
  parsePackageVersion . S8.pack
