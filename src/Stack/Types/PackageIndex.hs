-- FIXME remove this module too
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Stack.Types.PackageIndex
    ( OffsetSize (..)
    -- ** PackageIndex, IndexName & IndexLocation
    , PackageIndex(..)
    , IndexName(..)
    , indexNameText
    , IndexType (..)
    , HackageSecurity (..)
    ) where

import           Data.Aeson.Extended
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Path
import           Stack.Prelude

-- | offset in bytes into the 01-index.tar file for the .cabal file
-- contents, and size in bytes of the .cabal file
data OffsetSize = OffsetSize !Int64 !Int64
    deriving (Generic, Eq, Show, Data, Typeable)

instance Store OffsetSize
instance NFData OffsetSize

-- | Unique name for a package index
newtype IndexName = IndexName { unIndexName :: ByteString }
    deriving (Show, Eq, Ord, Hashable, Store)
indexNameText :: IndexName -> Text
indexNameText = decodeUtf8 . unIndexName
instance ToJSON IndexName where
    toJSON = toJSON . indexNameText

instance FromJSON IndexName where
    parseJSON = withText "IndexName" $ \t ->
        case parseRelDir (T.unpack t) of
            Left e -> fail $ "Invalid index name: " ++ show e
            Right _ -> return $ IndexName $ encodeUtf8 t

data IndexType = ITHackageSecurity !HackageSecurity | ITVanilla
    deriving (Show, Eq, Ord)

data HackageSecurity = HackageSecurity
    { hsKeyIds :: ![Text]
    , hsKeyThreshold :: !Int
    }
    deriving (Show, Eq, Ord)
instance FromJSON HackageSecurity where
    parseJSON = withObject "HackageSecurity" $ \o -> HackageSecurity
        <$> o .: "keyids"
        <*> o .: "key-threshold"

-- | Information on a single package index
data PackageIndex = PackageIndex
    { indexName :: !IndexName
    , indexLocation :: !Text
    -- ^ URL for the tarball or, in the case of Hackage Security, the
    -- root of the directory
    , indexType :: !IndexType
    , indexDownloadPrefix :: !Text
    -- ^ URL prefix for downloading packages
    , indexRequireHashes :: !Bool
    -- ^ Require that hashes and package size information be available for packages in this index
    }
    deriving Show
instance FromJSON (WithJSONWarnings PackageIndex) where
    parseJSON = withObjectWarnings "PackageIndex" $ \o -> do
        name <- o ..: "name"
        prefix <- o ..: "download-prefix"
        http <- o ..: "http"
        mhackageSecurity <- o ..:? "hackage-security"
        let indexType' = maybe ITVanilla ITHackageSecurity mhackageSecurity
        reqHashes <- o ..:? "require-hashes" ..!= False
        return PackageIndex
            { indexName = name
            , indexLocation = http
            , indexType = indexType'
            , indexDownloadPrefix = prefix
            , indexRequireHashes = reqHashes
            }
