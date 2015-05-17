{-# LANGUAGE TemplateHaskell #-}
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
  ,packageVersionText
  ,toCabalVersion
  ,fromCabalVersion
  ,mkPackageVersion
  ,versionRangeText
  ,withinRange)
  where

import           Control.Applicative
import           Control.Monad.Catch
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
import           Distribution.Version hiding (withinRange)
import           GHC.Generics
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Distribution.Version as C             (withinRange)
import Distribution.Text (disp)
import Text.PrettyPrint (render)

-- | A parse fail.
data PackageVersionParseFail =
  PackageVersionParseFail ByteString
  deriving (Show,Typeable)
instance Exception PackageVersionParseFail

-- | A package version.
newtype PackageVersion =
  PackageVersion {unPackageVersion :: Vector Word}
  deriving (Eq,Ord,Typeable,Data,Generic)

instance Hashable PackageVersion where
  hashWithSalt i = hashWithSalt i . V.toList . unPackageVersion

instance Lift PackageVersion where
  lift (PackageVersion n) =
    appE (conE 'PackageVersion)
         (appE (varE 'V.fromList)
               (listE (map (litE . IntegerL . fromIntegral)
                           (V.toList n))))

instance Show PackageVersion where
  show (PackageVersion v) =
    intercalate "."
                (map show (V.toList v))

instance ToJSON PackageVersion where
  toJSON = toJSON . packageVersionText
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
parsePackageVersion :: MonadThrow m => ByteString -> m PackageVersion
parsePackageVersion x = go x
  where go =
          either (const (throwM (PackageVersionParseFail x))) return .
          parseOnly (packageVersionParser <* endOfInput)

-- | Migration function.
parsePackageVersionFromString :: MonadThrow m => String -> m PackageVersion
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

-- | Convert to a Cabal version.
toCabalVersion :: PackageVersion -> Version
toCabalVersion (PackageVersion v) =
  Version (map fromIntegral (V.toList v)) []

-- | Convert from a Cabal version.
fromCabalVersion :: Version -> PackageVersion
fromCabalVersion (Version vs _) =
  let !v = V.fromList (map fromIntegral vs)
  in PackageVersion v

-- | Make a package version.
mkPackageVersion :: String -> Q Exp
mkPackageVersion s =
  case parsePackageVersionFromString s of
    Nothing -> error ("Invalid package version: " ++ show s)
    Just pn -> [|pn|]

-- | Display a version range
versionRangeText :: VersionRange -> Text
versionRangeText = T.pack . render . disp

-- | Check if a version is within a version range.
withinRange :: PackageVersion -> VersionRange -> Bool
withinRange v r = toCabalVersion v `C.withinRange` r
