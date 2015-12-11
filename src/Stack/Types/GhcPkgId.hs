{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A ghc-pkg id.

module Stack.Types.GhcPkgId
  (GhcPkgId
  ,ghcPkgIdParser
  ,parseGhcPkgId
  ,ghcPkgIdString)
  where

import           Control.Applicative
import           Control.Monad.Catch
import           Data.Aeson.Extended
import           Data.Attoparsec.Text
import           Data.Binary (getWord8, putWord8)
import           Data.Binary.VersionTagged
import           Data.Data
import           Data.Hashable
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Prelude -- Fix AMP warning

-- | A parse fail.
data GhcPkgIdParseFail
  = GhcPkgIdParseFail Text
  deriving Typeable
instance Show GhcPkgIdParseFail where
    show (GhcPkgIdParseFail bs) = "Invalid package ID: " ++ show bs
instance Exception GhcPkgIdParseFail

-- | A ghc-pkg package identifier.
newtype GhcPkgId = GhcPkgId Text
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
    GhcPkgId . T.pack <$> many1 (choice [digit, letter, satisfy (`elem` "_.-")])

-- | Get a string representation of GHC package id.
ghcPkgIdString :: GhcPkgId -> String
ghcPkgIdString (GhcPkgId x) = T.unpack x
