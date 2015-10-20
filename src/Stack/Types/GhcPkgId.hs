{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A ghc-pkg id.

module Stack.Types.GhcPkgId
  (GhcPkgId(..)
  ,ghcPkgIdParser
  ,parseGhcPkgId
  ,ghcPkgIdString)
  where

import           Control.Applicative
import           Control.Monad.Catch
import           Data.Aeson.Extended
import           Data.Attoparsec.ByteString.Char8 as A8
import           Data.Binary (getWord8, putWord8)
import           Data.Binary.VersionTagged
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Data
import           Data.Hashable
import           Data.Text.Encoding (encodeUtf8)
import           GHC.Generics
import           Prelude -- Fix AMP warning

-- | A parse fail.
data GhcPkgIdParseFail
  = GhcPkgIdParseFail ByteString
  deriving Typeable
instance Show GhcPkgIdParseFail where
    show (GhcPkgIdParseFail bs) = "Invalid package ID: " ++ show bs
instance Exception GhcPkgIdParseFail

-- | A ghc-pkg package identifier.
newtype GhcPkgId = GhcPkgId ByteString
  deriving (Eq,Ord,Data,Typeable,Generic)

instance Hashable GhcPkgId
instance Binary GhcPkgId where
    put (GhcPkgId x) = do
        -- magic string
        putWord8 1
        putWord8 3
        putWord8 4
        putWord8 7
        put x
    get = do
        1 <- getWord8
        3 <- getWord8
        4 <- getWord8
        7 <- getWord8
        fmap GhcPkgId get
instance NFData GhcPkgId
instance HasStructuralInfo GhcPkgId

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
    fmap GhcPkgId (A8.takeWhile isValid)
  where
    isValid c =
        ('A' <= c && c <= 'Z') ||
        ('a' <= c && c <= 'z') ||
        ('0' <= c && c <= '9') ||
        c == '.' ||
        c == '-' ||
        c == '_'

-- | Get a string representation of GHC package id.
ghcPkgIdString :: GhcPkgId -> String
ghcPkgIdString (GhcPkgId x) = S8.unpack x
