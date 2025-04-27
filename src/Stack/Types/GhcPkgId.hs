{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Types.GhcPkgId
Description : A ghc-pkg id.
License     : BSD-3-Clause

A ghc-pkg id.
-}

module Stack.Types.GhcPkgId
  ( GhcPkgId
  , ghcPkgIdToText
  , ghcPkgIdParser
  , parseGhcPkgId
  , ghcPkgIdString
  ) where

import           Data.Aeson.Types ( FromJSON (..), ToJSON (..), withText )
import           Data.Attoparsec.Text
                   ( Parser, (<?>), choice, endOfInput, many1, parseOnly
                   , satisfy
                   )
import           Data.Char ( isAlphaNum )
import           Data.Hashable ( Hashable(..) )
import           Database.Persist.Sql
                   ( PersistField (..), PersistFieldSql (..) )
import           Distribution.Compat.Binary ( decode, encode )
import           Distribution.Types.UnitId ( UnitId, mkUnitId, unUnitId )
import           Stack.Prelude
import           Text.Read ( Read (..) )

-- | A parse fail.
newtype GhcPkgIdParseFail
  = GhcPkgIdParseFail Text
  deriving (Show, Typeable)

instance Exception GhcPkgIdParseFail where
  displayException (GhcPkgIdParseFail bs) = concat
    [ "Error: [S-5359]\n"
    , "Invalid package ID: "
    , show bs
    ]

-- | A ghc-pkg package identifier.
newtype GhcPkgId
  = GhcPkgId UnitId
  deriving (Data, Eq, Generic, Ord, Typeable)

instance PersistField GhcPkgId where
  toPersistValue = toPersistValue . ghcPkgIdToText
  fromPersistValue = (fmap . fmap) ghcPkgIdFromText fromPersistValue

instance PersistFieldSql GhcPkgId where
  sqlType _ = sqlType @Text Proxy

instance Hashable GhcPkgId where
  hashWithSalt a (GhcPkgId v) = hashWithSalt a (encode v)

instance NFData GhcPkgId

instance Show GhcPkgId where
  show = show . ghcPkgIdString

instance Read GhcPkgId where
  readsPrec i = map (first (GhcPkgId . mkUnitId)) . readsPrec i

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
 where
  go = either
         (const (throwM (GhcPkgIdParseFail x)))
         pure . parseOnly (ghcPkgIdParser <* endOfInput)

-- | A parser for a package-version-hash pair.
ghcPkgIdParser :: Parser GhcPkgId
ghcPkgIdParser =
  let elements = "_.-" :: String
  in  GhcPkgId . mkUnitId <$>
        many1 (choice [alphaNum, satisfy (`elem` elements)])

-- | Parse an alphanumerical character, as recognised by `isAlphaNum`.
alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum <?> "alphanumeric"
{-# INLINE alphaNum #-}

-- | Get a string representation of GHC package id.
ghcPkgIdString :: GhcPkgId -> String
ghcPkgIdString (GhcPkgId x) = unUnitId x

-- | Get a text value of GHC package id.
ghcPkgIdToText :: GhcPkgId -> Text
ghcPkgIdToText (GhcPkgId v) = decode . encode $ v
-- | Create GhcPkgId from Text.
ghcPkgIdFromText :: Text -> GhcPkgId
ghcPkgIdFromText = GhcPkgId . decode . encode
