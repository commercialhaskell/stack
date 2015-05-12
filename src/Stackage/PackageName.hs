{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Names for packages.

module Stackage.PackageName
  (PackageName
  ,PackageNameParseFail(..)
  ,packageNameParser
  ,parsePackageName
  ,parsePackageNameFromString
  ,packageNameString
  ,packageNameText
  ,fromCabalPackageName)
  where

import           Control.Applicative
import           Control.Monad.Catch
import           Data.Aeson
import           Data.Attoparsec.ByteString.Char8
import           Data.Attoparsec.Combinators
import qualified Data.ByteString.Char8 as S8
import           Data.ByteString.Char8 as S8
import           Data.Char (isLetter)
import           Data.Data
import           Data.Hashable
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Distribution.Package as Cabal
import           GHC.Generics

-- | A parse fail.
data PackageNameParseFail =
  PackageNameParseFail ByteString
  deriving (Show,Typeable)
instance Exception PackageNameParseFail

-- | A package name.
newtype PackageName =
  PackageName ByteString
  deriving (Eq,Ord,Typeable,Data,Generic,Hashable)

instance Show PackageName where
  show (PackageName n) = S8.unpack n

instance FromJSON PackageName where
  parseJSON j =
    do s <- parseJSON j
       case parsePackageNameFromString s of
         Nothing ->
           fail ("Couldn't parse package name: " ++ s)
         Just ver -> return ver

-- | Attoparsec parser for a package name from bytestring.
packageNameParser :: Parser PackageName
packageNameParser =
  fmap (PackageName . S8.pack)
       (appending (many1 (satisfy isLetter))
                  (concating (many (alternating
                                      (pured (satisfy (\c -> isLetter c ||
                                                              isDigit c)))
                                      (appending (pured (satisfy (== '-')))
                                                 (pured (satisfy isLetter)))))))

-- | Convenient way to parse a package name from a bytestring.
parsePackageName :: MonadThrow m => ByteString -> m PackageName
parsePackageName x = go x
  where go =
          either (const (throwM (PackageNameParseFail x))) return .
          parseOnly (packageNameParser <* endOfInput)

-- | Migration function.
parsePackageNameFromString :: MonadThrow m => String -> m PackageName
parsePackageNameFromString =
  parsePackageName . S8.pack

-- | Produce a string representation of a package name.
packageNameString :: PackageName -> String
packageNameString (PackageName n) = S8.unpack n

-- | Produce a string representation of a package name.
packageNameText :: PackageName -> Text
packageNameText (PackageName n) = T.decodeUtf8 n

-- | Convert from a Cabal package name.
fromCabalPackageName :: Cabal.PackageName -> PackageName
fromCabalPackageName (Cabal.PackageName name) =
  let !x = S8.pack name
  in PackageName x
