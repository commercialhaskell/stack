{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A ghc-pkg id.

module Stack.Types.GhcPkgId
  (GhcPkgId
  ,ghcPkgIdParser
  ,parseGhcPkgId
  ,ghcPkgIdString)
  where

import           Stack.Prelude
import           Data.Aeson.Extended
import           Data.Attoparsec.Text
import qualified Data.Text as T

-- | A parse fail.
newtype GhcPkgIdParseFail
  = GhcPkgIdParseFail Text
  deriving Typeable
instance Show GhcPkgIdParseFail where
    show (GhcPkgIdParseFail bs) = "Invalid package ID: " ++ show bs
instance Exception GhcPkgIdParseFail

-- | A ghc-pkg package identifier.
newtype GhcPkgId = GhcPkgId Text
  deriving (Eq,Ord,Data,Typeable,Generic)

instance Hashable GhcPkgId
instance NFData GhcPkgId
instance Store GhcPkgId

instance Show GhcPkgId where
  show = show . ghcPkgIdString

instance FromJSON GhcPkgId where
  parseJSON = withText "GhcPkgId" $ \t ->
    case parseGhcPkgId t of
      Left e -> fail $ show (e, t)
      Right x -> return x

instance ToJSON GhcPkgId where
  toJSON g =
    toJSON (ghcPkgIdString g)

-- | Convenient way to parse a package name from a 'Text'.
parseGhcPkgId :: MonadThrow m => Text -> m GhcPkgId
parseGhcPkgId x = go x
  where go =
          either (const (throwM (GhcPkgIdParseFail x))) return .
          parseOnly (ghcPkgIdParser <* endOfInput)

-- | A parser for a package-version-hash pair.
ghcPkgIdParser :: Parser GhcPkgId
ghcPkgIdParser =
    let elements =  "_.-" :: String in
    GhcPkgId . T.pack <$> many1 (choice [digit, letter, satisfy (`elem` elements)])

-- | Get a string representation of GHC package id.
ghcPkgIdString :: GhcPkgId -> String
ghcPkgIdString (GhcPkgId x) = T.unpack x
