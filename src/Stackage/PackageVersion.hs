{-# LANGUAGE OverloadedStrings #-}
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
  ,parsePackageVersionFromString
  ,packageVersionString
  ,packageVersionText)
  where

import           Control.Applicative
import           Data.Aeson
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Data
import           Data.Hashable
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Data.Word
import           Distribution.Version
import           GHC.Generics

-- | A package version.
newtype PackageVersion =
  PackageVersion {unPackageVersion :: Vector Word}
  deriving (Eq,Ord,Typeable,Data,Generic)

instance Hashable PackageVersion where
  hashWithSalt i = hashWithSalt i . V.toList . unPackageVersion

instance Show PackageVersion where
  show (PackageVersion v) =
    intercalate "."
                (map show (V.toList v))

instance FromJSON PackageVersion where
  parseJSON j =
    do s <- parseJSON j
       case parsePackageVersionFromString s of
         Nothing ->
           fail ("Couldn't parse package version: " ++ s)
         Just ver -> return ver

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

-- | Get a string representation of a package version.
packageVersionString :: PackageVersion -> String
packageVersionString (PackageVersion v) =
  intercalate "."
              (map show (V.toList v))

-- | Get a string representation of a package version.
packageVersionText :: PackageVersion -> Text
packageVersionText (PackageVersion v) =
  T.intercalate
    "."
    (map (T.pack . show)
         (V.toList v))
