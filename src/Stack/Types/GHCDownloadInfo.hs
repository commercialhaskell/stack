{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Types.GHCDownloadInfo
License     : BSD-3-Clause
-}

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
  { configureOpts :: [Text]
  , configureEnv :: Map Text Text
  , downloadInfo :: DownloadInfo
  }
  deriving Show

instance FromJSON (WithJSONWarnings GHCDownloadInfo) where
  parseJSON = withObjectWarnings "GHCDownloadInfo" $ \o -> do
    configureOpts <- o ..:? "configure-opts" ..!= mempty
    configureEnv <- o ..:? "configure-env" ..!= mempty
    downloadInfo <- parseDownloadInfoFromObject o
    pure GHCDownloadInfo
      { configureOpts
      , configureEnv
      , downloadInfo
      }
