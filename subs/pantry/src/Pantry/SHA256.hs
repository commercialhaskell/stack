{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provides a data type ('SHA256') for efficient memory
-- representation of a sha-256 hash value, together with helper
-- functions for converting to and from that value. This module is
-- intended to be imported qualified as @SHA256@.
--
-- Some nomenclature:
--
-- * Hashing calculates a new hash value from some input. @from@ takes a value that representats an existing hash.
--
-- * Raw means a raw binary representation of the hash value, without any hex encoding.
--
-- * Text always uses lower case hex encoding
--
-- @since 0.1.0.0
module Pantry.SHA256
  ( -- * Types
    SHA256
  , SHA256Exception (..)
    -- * Hashing
  , hashFile
  , hashBytes
  , hashLazyBytes
  , sinkHash
    -- * Convert from a hash representation
  , fromHexText
  , fromHexBytes
  , fromDigest
  , fromRaw
    -- * Convert to a hash representation
  , toHexText
  , toHexBytes
  , toRaw
  ) where

import RIO
import Data.Aeson
import Database.Persist.Sql
import Pantry.Internal.StaticBytes
import Data.Store (Store) -- FIXME remove
import Conduit
import qualified RIO.Text as T

import qualified Crypto.Hash.Conduit as Hash (hashFile, sinkHash)
import qualified Crypto.Hash as Hash (hash, hashlazy, Digest, SHA256)
import qualified Data.ByteArray
import qualified Data.ByteArray.Encoding as Mem

-- | A SHA256 hash, stored in a static size for more efficient
-- memory representation.
--
-- @since 0.1.0.0
newtype SHA256 = SHA256 Bytes32
    deriving (Generic, Eq, NFData, Data, Typeable, Ord, Hashable, Store)

-- | Exceptions which can occur in this module
--
-- @since 0.1.0.0
data SHA256Exception
  = InvalidByteCount !ByteString !StaticBytesException
  | InvalidHexBytes !ByteString !Text
  deriving (Typeable)

-- | Generate a 'SHA256' value by hashing the contents of a file.
--
-- @since 0.1.0.0
hashFile :: MonadIO m => FilePath -> m SHA256
hashFile fp = fromDigest <$> Hash.hashFile fp

-- | Generate a 'SHA256' value by hashing a @ByteString@.
--
-- @since 0.1.0.0
hashBytes :: ByteString -> SHA256
hashBytes = fromDigest . Hash.hash

-- | Generate a 'SHA256' value by hashing a lazy @ByteString@.
--
-- @since 0.1.0.0
hashLazyBytes :: LByteString -> SHA256
hashLazyBytes = fromDigest . Hash.hashlazy

-- | Generate a 'SHA256' value by hashing the contents of a stream.
--
-- @since 0.1.0.0
sinkHash :: Monad m => ConduitT ByteString o m SHA256
sinkHash = fromDigest <$> Hash.sinkHash

-- | Convert a base16-encoded 'Text' value containing a hash into a 'SHA256'.
--
-- @since 0.1.0.0
fromHexText :: Text -> Either SHA256Exception SHA256
fromHexText = fromHexBytes . encodeUtf8

-- | Convert a base16-encoded 'ByteString' value containing a hash into a 'SHA256'.
--
-- @since 0.1.0.0
fromHexBytes :: ByteString -> Either SHA256Exception SHA256
fromHexBytes hexBS = do
  mapLeft (InvalidHexBytes hexBS . T.pack) (Mem.convertFromBase Mem.Base16 hexBS) >>= fromRaw

-- | Convert a 'Hash.Digest' into a 'SHA256'
--
-- @since 0.1.0.0
fromDigest :: Hash.Digest Hash.SHA256 -> SHA256
fromDigest digest =
  case toStaticExact (Data.ByteArray.convert digest :: ByteString) of
    Left e -> error $ "Impossible failure in fromDigest: " ++ show (digest, e)
    Right x -> SHA256 x

-- | Convert a raw representation of a hash into a 'SHA256'.
--
-- @since 0.1.0.0
fromRaw :: ByteString -> Either SHA256Exception SHA256
fromRaw bs = either (Left . InvalidByteCount bs) (Right . SHA256) (toStaticExact bs)

-- | Convert a 'SHA256' into a base16-encoded SHA256 hash.
--
-- @since 0.1.0.0
toHexText :: SHA256 -> Text
toHexText ss =
  case decodeUtf8' $ toHexBytes ss of
    Left e -> error $ "Impossible failure in staticSHA256ToText: " ++ show (ss, e)
    Right t -> t

-- | Convert a 'SHA256' into a base16-encoded SHA256 hash.
--
-- @since 0.1.0.0
toHexBytes :: SHA256 -> ByteString
toHexBytes (SHA256 x) = Mem.convertToBase Mem.Base16 x

-- | Convert a 'SHA256' into a raw binary representation.
--
-- @since 0.1.0.0
toRaw :: SHA256 -> ByteString
toRaw (SHA256 x) = Data.ByteArray.convert x

-- Instances

instance Show SHA256 where
  show s = "SHA256 " ++ show (toHexText s)

instance PersistField SHA256 where
  toPersistValue = PersistByteString . toRaw
  fromPersistValue (PersistByteString bs) =
    case toStaticExact bs of
      Left e -> Left $ tshow e
      Right ss -> pure $ SHA256 ss
  fromPersistValue x = Left $ "Unexpected value: " <> tshow x

instance PersistFieldSql SHA256 where
  sqlType _ = SqlBlob

instance Display SHA256 where
  display = displayBytesUtf8 . toHexBytes

instance ToJSON SHA256 where
  toJSON = toJSON . toHexText
instance FromJSON SHA256 where
  parseJSON = withText "SHA256" $ \t ->
    case fromHexText t of
      Right x -> pure x
      Left e -> fail $ concat
        [ "Invalid SHA256 "
        , show t
        , ": "
        , show e
        ]

instance Exception SHA256Exception
instance Show SHA256Exception where
  show = T.unpack . utf8BuilderToText . display
instance Display SHA256Exception where
  display (InvalidByteCount bs sbe) =
    "Invalid byte count creating a SHA256 from " <>
    displayShow bs <>
    ": " <>
    displayShow sbe
  display (InvalidHexBytes bs t) =
    "Invalid hex bytes creating a SHA256: " <>
    displayShow bs <>
    ": " <>
    display t
