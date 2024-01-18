{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.VersionedDownloadInfo
  ( VersionedDownloadInfo (..)
  ) where

import           Data.Aeson.Types ( FromJSON (..) )
import           Data.Aeson.WarningParser
                   ( WithJSONWarnings (..), (..:), withObjectWarnings )
import           Stack.Prelude
import           Stack.Types.DownloadInfo
                   ( DownloadInfo, parseDownloadInfoFromObject )

data VersionedDownloadInfo = VersionedDownloadInfo
  { version :: Version
  , downloadInfo :: DownloadInfo
  }
  deriving Show

instance FromJSON (WithJSONWarnings VersionedDownloadInfo) where
  parseJSON = withObjectWarnings "VersionedDownloadInfo" $ \o -> do
    CabalString version <- o ..: "version"
    downloadInfo <- parseDownloadInfoFromObject o
    pure VersionedDownloadInfo
      { version
      , downloadInfo
      }
