{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Stack.Types.PackageIndex
    ( PackageDownload (..)
    , PackageCache (..)
    , PackageCacheMap (..)
    , OffsetSize (..)
    -- ** PackageIndex, IndexName & IndexLocation
    , PackageIndex(..)
    , IndexName(..)
    , indexNameText
    , IndexLocation(..)
    ) where

import           Control.DeepSeq (NFData)
import           Control.Monad (mzero)
import           Data.Aeson.Extended
import           Data.ByteString (ByteString)
import           Data.Hashable (Hashable)
import           Data.Data (Data, Typeable)
import           Data.HashMap.Strict (HashMap)
import           Data.Int (Int64)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Store (Store)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Path
import           Stack.Types.BuildPlan (GitSHA1)
import           Stack.Types.PackageIdentifier

data PackageCache = PackageCache
    { pcOffsetSize :: {-# UNPACK #-}!OffsetSize
    , pcDownload :: !(Maybe PackageDownload)
    }
    deriving (Generic, Eq, Show, Data, Typeable)

instance Store PackageCache
instance NFData PackageCache

-- | offset in bytes into the 01-index.tar file for the .cabal file
-- contents, and size in bytes of the .cabal file
data OffsetSize = OffsetSize !Int64 !Int64
    deriving (Generic, Eq, Show, Data, Typeable)

instance Store OffsetSize
instance NFData OffsetSize

data PackageCacheMap = PackageCacheMap
    { pcmIdent :: !(Map PackageIdentifier PackageCache)
    -- ^ most recent revision of the package
    , pcmSHA :: !(HashMap GitSHA1 OffsetSize)
    -- ^ lookup via the GitSHA1 of the cabal file contents
    }
    deriving (Generic, Eq, Show, Data, Typeable)
instance Store PackageCacheMap
instance NFData PackageCacheMap

data PackageDownload = PackageDownload
    { pdSHA512 :: !ByteString
    , pdUrl    :: !ByteString
    , pdSize   :: !Word64
    }
    deriving (Show, Generic, Eq, Data, Typeable)
instance Store PackageDownload
instance NFData PackageDownload
instance FromJSON PackageDownload where
    parseJSON = withObject "Package" $ \o -> do
        hashes <- o .: "package-hashes"
        sha512 <- maybe mzero return (Map.lookup ("SHA512" :: Text) hashes)
        locs <- o .: "package-locations"
        url <-
            case reverse locs of
                [] -> mzero
                x:_ -> return x
        size <- o .: "package-size"
        return PackageDownload
            { pdSHA512 = encodeUtf8 sha512
            , pdUrl = encodeUtf8 url
            , pdSize = size
            }

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

-- | Location of the package index. This ensures that at least one of Git or
-- HTTP is available.
data IndexLocation = ILGit !Text | ILHttp !Text | ILGitHttp !Text !Text
    deriving (Show, Eq, Ord)


-- | Information on a single package index
data PackageIndex = PackageIndex
    { indexName :: !IndexName
    , indexLocation :: !IndexLocation
    , indexDownloadPrefix :: !Text
    -- ^ URL prefix for downloading packages
    , indexGpgVerify :: !Bool
    -- ^ GPG-verify the package index during download. Only applies to Git
    -- repositories for now.
    , indexRequireHashes :: !Bool
    -- ^ Require that hashes and package size information be available for packages in this index
    }
    deriving Show
instance FromJSON (WithJSONWarnings PackageIndex) where
    parseJSON = withObjectWarnings "PackageIndex" $ \o -> do
        name <- o ..: "name"
        prefix <- o ..: "download-prefix"
        mgit <- o ..:? "git"
        mhttp <- o ..:? "http"
        loc <-
            case (mgit, mhttp) of
                (Nothing, Nothing) -> fail $
                    "Must provide either Git or HTTP URL for " ++
                    T.unpack (indexNameText name)
                (Just git, Nothing) -> return $ ILGit git
                (Nothing, Just http) -> return $ ILHttp http
                (Just git, Just http) -> return $ ILGitHttp git http
        gpgVerify <- o ..:? "gpg-verify" ..!= False
        reqHashes <- o ..:? "require-hashes" ..!= False
        return PackageIndex
            { indexName = name
            , indexLocation = loc
            , indexDownloadPrefix = prefix
            , indexGpgVerify = gpgVerify
            , indexRequireHashes = reqHashes
            }
