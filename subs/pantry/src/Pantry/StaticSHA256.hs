{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.StaticSHA256
  ( StaticSHA256
  , mkStaticSHA256FromText
  , mkStaticSHA256FromFile
  , mkStaticSHA256FromDigest
  , mkStaticSHA256FromBytes
  , mkStaticSHA256FromRaw
  , staticSHA256ToText
  , staticSHA256ToBase16
  , staticSHA256ToRaw
  ) where

import RIO
import Data.Aeson
import Database.Persist.Sql
import Pantry.StaticBytes
import Data.Store (Store) -- FIXME remove

import           Crypto.Hash.Conduit (hashFile)
import           Crypto.Hash as Hash (hash, Digest, SHA256)
import qualified Data.ByteArray
import qualified Data.ByteArray.Encoding as Mem

-- | A SHA256 hash, stored in a static size for more efficient
-- serialization with store.
newtype StaticSHA256 = StaticSHA256 Bytes32
    deriving (Generic, Eq, NFData, Data, Typeable, Ord, Hashable, Store)

instance Show StaticSHA256 where
  show s = "StaticSHA256 " ++ show (staticSHA256ToText s)

instance PersistField StaticSHA256 where
  toPersistValue = PersistByteString . staticSHA256ToRaw
  fromPersistValue (PersistByteString bs) =
    case toStaticExact bs of
      Left e -> Left $ tshow e
      Right ss -> pure $ StaticSHA256 ss
  fromPersistValue x = Left $ "Unexpected value: " <> tshow x

instance PersistFieldSql StaticSHA256 where
  sqlType _ = SqlBlob

instance Display StaticSHA256 where
  display = display . staticSHA256ToText

-- | Generate a 'StaticSHA256' value from the contents of a file.
mkStaticSHA256FromFile :: MonadIO m => FilePath -> m StaticSHA256
mkStaticSHA256FromFile fp = liftIO $ mkStaticSHA256FromDigest <$> hashFile fp

mkStaticSHA256FromBytes :: ByteString -> StaticSHA256
mkStaticSHA256FromBytes = mkStaticSHA256FromDigest . Hash.hash

mkStaticSHA256FromDigest :: Hash.Digest Hash.SHA256 -> StaticSHA256
mkStaticSHA256FromDigest digest
  = StaticSHA256
  $ either impureThrow id
  $ toStaticExact
    (Data.ByteArray.convert digest :: ByteString)

-- | Convert a 'StaticSHA256' into a base16-encoded SHA256 hash.
staticSHA256ToText :: StaticSHA256 -> Text
staticSHA256ToText ss =
  case decodeUtf8' $ staticSHA256ToBase16 ss of
    Left e -> error $ "Impossible failure in staticSHA256ToText: " ++ show (ss, e)
    Right t -> t

-- | Convert a 'StaticSHA256' into a base16-encoded SHA256 hash.
staticSHA256ToBase16 :: StaticSHA256 -> ByteString
staticSHA256ToBase16 (StaticSHA256 x) = Mem.convertToBase Mem.Base16 x

staticSHA256ToRaw :: StaticSHA256 -> ByteString
staticSHA256ToRaw (StaticSHA256 x) = Data.ByteArray.convert x

mkStaticSHA256FromRaw :: ByteString -> Either StaticBytesException StaticSHA256
mkStaticSHA256FromRaw = fmap StaticSHA256 . toStaticExact

-- | Generate a 'StaticSHA256' value from a base16-encoded SHA256 hash.
mkStaticSHA256FromText :: Text -> Either SomeException StaticSHA256
mkStaticSHA256FromText t =
  mapLeft (toException . stringException) (Mem.convertFromBase Mem.Base16 (encodeUtf8 t))
  >>= either (Left . toE) (Right . StaticSHA256)
    . toStaticExact
    . (id :: ByteString -> ByteString)
  where
    toE e = toException $ stringException $ concat
      [ "Unable to convert "
      , show t
      , " into SHA256: "
      , show e
      ]

instance ToJSON StaticSHA256 where
  toJSON = toJSON . staticSHA256ToText
instance FromJSON StaticSHA256 where
  parseJSON = withText "StaticSHA256" $ \t ->
    case mkStaticSHA256FromText t of
      Right x -> pure x
      Left e -> fail $ concat
        [ "Invalid SHA256 "
        , show t
        , ": "
        , show e
        ]
