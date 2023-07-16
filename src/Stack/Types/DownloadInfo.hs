{-# LANGUAGE NoImplicitPrelude #-}
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
  { downloadInfoUrl :: Text
    -- ^ URL or absolute file path
  , downloadInfoContentLength :: Maybe Int
  , downloadInfoSha1 :: Maybe ByteString
  , downloadInfoSha256 :: Maybe ByteString
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
  pure
    DownloadInfo
    { downloadInfoUrl = url
    , downloadInfoContentLength = contentLength
    , downloadInfoSha1 = fmap encodeUtf8 sha1TextMay
    , downloadInfoSha256 = fmap encodeUtf8 sha256TextMay
    }
