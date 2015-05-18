{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Package identifier (name-version).

module Stack.PackageIdentifier
  (PackageIdentifier(..)
  ,toTuple
  ,fromTuple
  ,packageIdentifierVersion
  ,packageIdentifierName
  ,packageIdentifierParser
  ,packageIdentifierString)
  where

import Data.Attoparsec.ByteString.Char8
import Data.Data
import Data.Hashable
import GHC.Generics
import Prelude hiding (FilePath)
import Stack.PackageName
import Stack.Version

-- | A pkg-ver combination.
data PackageIdentifier =
  PackageIdentifier !PackageName
                    !Version
  deriving (Eq,Ord,Generic,Data,Typeable)

instance Hashable PackageIdentifier

instance Show PackageIdentifier where
  show = show . packageIdentifierString

-- | Convert from a package identifier to a tuple.
toTuple :: PackageIdentifier -> (PackageName,Version)
toTuple (PackageIdentifier n v) = (n,v)

-- | Convert from a tuple to a package identifier.
fromTuple :: (PackageName,Version) -> PackageIdentifier
fromTuple (n,v) = PackageIdentifier n v

-- | Get the version part of the identifier.
packageIdentifierVersion :: PackageIdentifier -> Version
packageIdentifierVersion (PackageIdentifier _ ver) = ver

-- | Get the name part of the identifier.
packageIdentifierName :: PackageIdentifier -> PackageName
packageIdentifierName (PackageIdentifier name _) = name

-- | A parser for a package-version pair.
packageIdentifierParser :: Parser PackageIdentifier
packageIdentifierParser =
  do name <- packageNameParser
     char8 '-'
     version <- versionParser
     return (PackageIdentifier name version)

-- | Get a string representation of the package identifier; name-ver.
packageIdentifierString :: PackageIdentifier -> String
packageIdentifierString (PackageIdentifier n v) = show n ++ "-" ++ show v
