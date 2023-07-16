{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.DumpLogs
  ( DumpLogs (..)
  ) where

import           Data.Aeson.Types ( FromJSON (..), Value (..), withText )
import           Stack.Prelude

-- | Which build log files to dump
data DumpLogs
  = DumpNoLogs -- ^ don't dump any logfiles
  | DumpWarningLogs -- ^ dump logfiles containing warnings
  | DumpAllLogs -- ^ dump all logfiles
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance FromJSON DumpLogs where
  parseJSON (Bool True) = pure DumpAllLogs
  parseJSON (Bool False) = pure DumpNoLogs
  parseJSON v =
    withText
      "DumpLogs"
      (\t ->
          if | t == "none" -> pure DumpNoLogs
             | t == "warning" -> pure DumpWarningLogs
             | t == "all" -> pure DumpAllLogs
             | otherwise -> fail ("Invalid DumpLogs: " ++ show t))
      v
