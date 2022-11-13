{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A ghc-pkg id.

module Stack.Types.GhcPkgId
  (GhcPkgId
  ,unGhcPkgId
  ,ghcPkgIdParser
  ,parseGhcPkgId
  ,ghcPkgIdString)
  where

import           Stack.Prelude
import           Pantry.Internal.AesonExtended
import           Data.Attoparsec.Text
import qualified Data.Text as T
import           Database.Persist.Sql (PersistField, PersistFieldSql)
import           Prelude (Read (..))

-- | A parse fail.
newtype GhcPkgIdParseFail
  = GhcPkgIdParseFail Text
  deriving Typeable

instance Show GhcPkgIdParseFail where
    show (GhcPkgIdParseFail bs) = concat
        [ "Error: [S-5359]\n"
        , "Invalid package ID: "
        , show bs
        ]

instance Exception GhcPkgIdParseFail

-- | A ghc-pkg package identifier.
newtype GhcPkgId = GhcPkgId Text
  deriving (Eq,Ord,Data,Typeable,Generic,PersistField,PersistFieldSql)

instance Hashable GhcPkgId
instance NFData GhcPkgId

instance Show GhcPkgId where
  show = show . ghcPkgIdString
instance Read GhcPkgId where
  readsPrec i = map (first (GhcPkgId . T.pack)) . readsPrec i

instance FromJSON GhcPkgId where
  parseJSON = withText "GhcPkgId" $ \t ->
    case parseGhcPkgId t of
      Left e -> fail $ show (e, t)
      Right x -> pure x

instance ToJSON GhcPkgId where
  toJSON g =
    toJSON (ghcPkgIdString g)

-- | Convenient way to parse a package name from a 'Text'.
parseGhcPkgId :: MonadThrow m => Text -> m GhcPkgId
parseGhcPkgId x = go x
  where go =
          either (const (throwM (GhcPkgIdParseFail x))) pure .
          parseOnly (ghcPkgIdParser <* endOfInput)

-- | A parser for a package-version-hash pair.
ghcPkgIdParser :: Parser GhcPkgId
ghcPkgIdParser =
    let elements =  "_.-" :: String in
    GhcPkgId . T.pack <$> many1 (choice [digit, letter, satisfy (`elem` elements)])

-- | Get a string representation of GHC package id.
ghcPkgIdString :: GhcPkgId -> String
ghcPkgIdString (GhcPkgId x) = T.unpack x

-- | Get a text value of GHC package id
unGhcPkgId :: GhcPkgId -> Text
unGhcPkgId (GhcPkgId v) = v
