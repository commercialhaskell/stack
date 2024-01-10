{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}


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
  { siSevenzExe :: Maybe DownloadInfo
  , siSevenzDll :: Maybe DownloadInfo
  , siMsys2 :: Map Text VersionedDownloadInfo
  , siGHCs :: Map Text (Map Version GHCDownloadInfo)
  , siStack :: Map Text (Map Version DownloadInfo)
  }
  deriving Show

instance FromJSON (WithJSONWarnings SetupInfo) where
  parseJSON = withObjectWarnings "SetupInfo" $ \o -> do
    siSevenzExe <- jsonSubWarningsT (o ..:? "sevenzexe-info")
    siSevenzDll <- jsonSubWarningsT (o ..:? "sevenzdll-info")
    siMsys2 <- jsonSubWarningsT (o ..:? "msys2" ..!= mempty)
    (fmap unCabalStringMap -> siGHCs) <-
      jsonSubWarningsTT (o ..:? "ghc" ..!= mempty)
    (fmap unCabalStringMap -> siStack) <-
      jsonSubWarningsTT (o ..:? "stack" ..!= mempty)
    pure $ SetupInfo
      { siSevenzExe
      , siSevenzDll
      , siMsys2
      , siGHCs
      , siStack
      }

-- | For the @siGHCs@ field maps are deeply merged. For all fields the values
-- from the first @SetupInfo@ win.
instance Semigroup SetupInfo where
  l <> r =
    SetupInfo
      { siSevenzExe = l.siSevenzExe <|> r.siSevenzExe
      , siSevenzDll = l.siSevenzDll <|> r.siSevenzDll
      , siMsys2 = l.siMsys2 <> r.siMsys2
      , siGHCs = Map.unionWith (<>) l.siGHCs r.siGHCs
      , siStack = Map.unionWith (<>) l.siStack r.siStack
      }

instance Monoid SetupInfo where
  mempty =
    SetupInfo
      { siSevenzExe = Nothing
      , siSevenzDll = Nothing
      , siMsys2 = Map.empty
      , siGHCs = Map.empty
      , siStack = Map.empty
      }
  mappend = (<>)
