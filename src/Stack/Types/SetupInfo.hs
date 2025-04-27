{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}

{-|
Module      : Stack.Types.SetupInfo
License     : BSD-3-Clause
-}

module Stack.Types.SetupInfo
  ( SetupInfo (..)
  ) where

import           Data.Aeson.Types ( FromJSON (..) )
import           Data.Aeson.WarningParser
                   ( WithJSONWarnings, (..:?), (..!=), jsonSubWarningsT
                   , jsonSubWarningsTT, withObjectWarnings
                   )
import qualified Data.Map as Map
import           Stack.Prelude
import           Stack.Types.DownloadInfo ( DownloadInfo )
import           Stack.Types.VersionedDownloadInfo ( VersionedDownloadInfo )
import           Stack.Types.GHCDownloadInfo ( GHCDownloadInfo )

data SetupInfo = SetupInfo
  { sevenzExe :: Maybe DownloadInfo
  , sevenzDll :: Maybe DownloadInfo
  , msys2 :: Map Text VersionedDownloadInfo
  , ghcByVersion :: Map Text (Map Version GHCDownloadInfo)
  , stackByVersion :: Map Text (Map Version DownloadInfo)
  }
  deriving Show

instance FromJSON (WithJSONWarnings SetupInfo) where
  parseJSON = withObjectWarnings "SetupInfo" $ \o -> do
    sevenzExe <- jsonSubWarningsT (o ..:? "sevenzexe-info")
    sevenzDll <- jsonSubWarningsT (o ..:? "sevenzdll-info")
    msys2 <- jsonSubWarningsT (o ..:? "msys2" ..!= mempty)
    (fmap unCabalStringMap -> ghcByVersion) <-
      jsonSubWarningsTT (o ..:? "ghc" ..!= mempty)
    (fmap unCabalStringMap -> stackByVersion) <-
      jsonSubWarningsTT (o ..:? "stack" ..!= mempty)
    pure SetupInfo
      { sevenzExe
      , sevenzDll
      , msys2
      , ghcByVersion
      , stackByVersion
      }

-- | For the @siGHCs@ field maps are deeply merged. For all fields the values
-- from the first @SetupInfo@ win.
instance Semigroup SetupInfo where
  l <> r =
    SetupInfo
      { sevenzExe = l.sevenzExe <|> r.sevenzExe
      , sevenzDll = l.sevenzDll <|> r.sevenzDll
      , msys2 = l.msys2 <> r.msys2
      , ghcByVersion = Map.unionWith (<>) l.ghcByVersion r.ghcByVersion
      , stackByVersion = Map.unionWith (<>) l.stackByVersion r.stackByVersion
      }

instance Monoid SetupInfo where
  mempty =
    SetupInfo
      { sevenzExe = Nothing
      , sevenzDll = Nothing
      , msys2 = Map.empty
      , ghcByVersion = Map.empty
      , stackByVersion = Map.empty
      }
  mappend = (<>)
