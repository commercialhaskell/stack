{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.DownloadInfo
  ( DownloadInfo (..)
  , parseDownloadInfoFromObject
  ) where

import           Data.Aeson.Types ( FromJSON (..), Object )
import           Data.Aeson.WarningParser
                   ( WarningParser, WithJSONWarnings (..), (..:), (..:?)
                   , withObjectWarnings
                   )
import           Stack.Prelude

-- | Build of the compiler distribution (e.g. standard, gmp4, tinfo6)
-- | Information for a file to download.
data DownloadInfo = DownloadInfo
  { url :: Text
    -- ^ URL or absolute file path
  , contentLength :: Maybe Int
  , sha1 :: Maybe ByteString
  , sha256 :: Maybe ByteString
  }
  deriving Show

instance FromJSON (WithJSONWarnings DownloadInfo) where
  parseJSON = withObjectWarnings "DownloadInfo" parseDownloadInfoFromObject

-- | Parse JSON in existing object for 'DownloadInfo'
parseDownloadInfoFromObject :: Object -> WarningParser DownloadInfo
parseDownloadInfoFromObject o = do
  url <- o ..: "url"
  contentLength <- o ..:? "content-length"
  sha1TextMay <- o ..:? "sha1"
  sha256TextMay <- o ..:? "sha256"
  let sha1 = fmap encodeUtf8 sha1TextMay
      sha256 = fmap encodeUtf8 sha256TextMay
  pure
    DownloadInfo
    { url
    , contentLength
    , sha1
    , sha256
    }
