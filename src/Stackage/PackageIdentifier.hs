{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Package identifier (name-version).

module Stackage.PackageIdentifier where

import Data.Data
import GHC.Generics
import Stackage.PackageName
import Stackage.PackageVersion

import Data.Attoparsec.ByteString.Char8
import Data.Hashable
import Prelude hiding (FilePath)

-- | A pkg-ver combination.
data PackageIdentifier =
  PackageIdentifier !PackageName
                    !PackageVersion
  deriving (Eq,Ord,Generic,Data,Typeable)

instance Hashable PackageIdentifier

instance Show PackageIdentifier where
  show (PackageIdentifier n v) = show n ++ "-" ++ show v

-- | Convert from a package identifier to a tuple.
toTuple :: PackageIdentifier -> (PackageName,PackageVersion)
toTuple (PackageIdentifier n v) = (n,v)

-- | Convert from a tuple to a package identifier.
fromTuple :: (PackageName,PackageVersion) -> PackageIdentifier
fromTuple (n,v) = PackageIdentifier n v

-- | A parser for a package-version pair.
packageIdentifierParser :: Parser PackageIdentifier
packageIdentifierParser =
  do name <- packageNameParser
     char8 '-'
     version <- packageVersionParser
     return (PackageIdentifier name version)
