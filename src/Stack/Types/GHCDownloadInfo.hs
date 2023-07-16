{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.GHCDownloadInfo
  ( GHCDownloadInfo (..)
  ) where

import           Data.Aeson.Types ( FromJSON (..) )
import           Data.Aeson.WarningParser
                   ( WithJSONWarnings (..), (..:?), (..!=), withObjectWarnings )
import           Stack.Prelude
import           Stack.Types.DownloadInfo
                   ( DownloadInfo, parseDownloadInfoFromObject )

data GHCDownloadInfo = GHCDownloadInfo
  { gdiConfigureOpts :: [Text]
  , gdiConfigureEnv :: Map Text Text
  , gdiDownloadInfo :: DownloadInfo
  }
  deriving Show

instance FromJSON (WithJSONWarnings GHCDownloadInfo) where
  parseJSON = withObjectWarnings "GHCDownloadInfo" $ \o -> do
    configureOpts <- o ..:? "configure-opts" ..!= mempty
    configureEnv <- o ..:? "configure-env" ..!= mempty
    downloadInfo <- parseDownloadInfoFromObject o
    pure GHCDownloadInfo
      { gdiConfigureOpts = configureOpts
      , gdiConfigureEnv = configureEnv
      , gdiDownloadInfo = downloadInfo
      }
