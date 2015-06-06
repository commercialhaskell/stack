{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A ghc-pkg id.

module Stack.Types.GhcPkgId
  (GhcPkgId
  ,ghcPkgIdParser
  ,parseGhcPkgId
  ,ghcPkgIdString
  ,ghcPkgIdPackageIdentifier)
  where

import           Control.Applicative
import           Control.Monad.Catch
import           Data.Aeson
import           Data.Attoparsec.ByteString.Char8
import           Data.Binary (Binary)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Char (isLetter)
import           Data.Data
import           Data.Hashable
import           Data.Text.Encoding (encodeUtf8)
import           GHC.Generics
import           Prelude -- Fix AMP warning
import           Stack.Types.PackageIdentifier

-- | A parse fail.
data GhcPkgIdParseFail
  = GhcPkgIdParseFail ByteString
  deriving (Show,Typeable)
instance Exception GhcPkgIdParseFail

-- | A ghc-pkg package identifier.
data GhcPkgId =
  GhcPkgId !PackageIdentifier
           !ByteString
  deriving (Eq,Ord,Data,Typeable,Generic)

instance Hashable GhcPkgId
instance Binary GhcPkgId

instance Show GhcPkgId where
  show = show . ghcPkgIdString

instance FromJSON GhcPkgId where
  parseJSON = withText "GhcPkgId" $ \t ->
    case parseGhcPkgId $ encodeUtf8 t of
      Left e -> fail $ show (e, t)
      Right x -> return x

instance ToJSON GhcPkgId where
  toJSON g =
    toJSON (ghcPkgIdString g)

-- | Convenient way to parse a package name from a bytestring.
parseGhcPkgId :: MonadThrow m => ByteString -> m GhcPkgId
parseGhcPkgId x = go x
  where go =
          either (const (throwM (GhcPkgIdParseFail x))) return .
          parseOnly (ghcPkgIdParser <* endOfInput)

-- | A parser for a package-version-hash pair.
ghcPkgIdParser :: Parser GhcPkgId
ghcPkgIdParser =
  do ident <- packageIdentifierParser
     _ <- char8 '-'
     h <- many1 (satisfy isAlphaNum)
     let !bytes = S8.pack h
     return (GhcPkgId ident bytes)
  where isAlphaNum c = isLetter c || isDigit c

-- | Get a string representation of GHC package id.
ghcPkgIdString :: GhcPkgId -> String
ghcPkgIdString (GhcPkgId ident x) =
  packageIdentifierString ident ++ "-" ++ S8.unpack x

-- | Get the package identifier of the GHC package id.
ghcPkgIdPackageIdentifier :: GhcPkgId -> PackageIdentifier
ghcPkgIdPackageIdentifier (GhcPkgId ident _) = ident
