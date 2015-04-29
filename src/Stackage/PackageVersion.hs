-- | Versions for packages.

module Stackage.PackageVersion
  (PackageVersion
  ,packageVersionParser
  ,parsePackageVersion
  ,parsePackageVersionFromString)
  where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 as S8
import Data.Word (Word8)

-- | A package version.
data PackageVersion
  = PackageVersion1 !Word8
  | PackageVersion2 !Word8
                    !Word8
  | PackageVersion3 !Word8
                    !Word8
                    !Word8
  | PackageVersion4 !Word8
                    !Word8
                    !Word8
                    !Word8
  deriving (Ord,Eq)

instance Show PackageVersion where
  show p =
    case p of
      PackageVersion1 a       -> show a
      PackageVersion2 a b     -> show a ++ "." ++ show b
      PackageVersion3 a b c   -> show a ++ "." ++ show b ++ "." ++ show c
      PackageVersion4 a b c d -> show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d

-- | Attoparsec parser for a package version from bytestring.
packageVersionParser :: Parser PackageVersion
packageVersionParser = v4 <|> v3 <|> v2 <|> v1
  where v1 =
          PackageVersion1 <$> num
        v2 =
          PackageVersion2 <$> num <*> num'
        v3 =
          PackageVersion3 <$> num <*> num' <*> num'
        v4 =
          PackageVersion4 <$> num <*> num' <*> num' <*> num'
        num = hexadecimal
        num' = point *> num
        point = satisfy (=='.')

-- | Convenient way to parse a package version from a bytestring.
parsePackageVersion :: ByteString -> Maybe PackageVersion
parsePackageVersion =
  either (const Nothing) Just .
  parseOnly packageVersionParser

-- | Migration function.
parsePackageVersionFromString :: String -> Maybe PackageVersion
parsePackageVersionFromString =
  parsePackageVersion . S8.pack
