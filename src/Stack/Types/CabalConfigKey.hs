{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.CabalConfigKey
  ( CabalConfigKey (..)
  , parseCabalConfigKey
  ) where

import qualified Data.Text as T
import           Pantry.Internal.AesonExtended
                   ( FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..)
                   , withText
                   )
import           Stack.Prelude

-- | Which packages do configure opts apply to?
data CabalConfigKey
  = CCKTargets -- ^ See AGOTargets
  | CCKLocals -- ^ See AGOLocals
  | CCKEverything -- ^ See AGOEverything
  | CCKPackage !PackageName -- ^ A specific package
  deriving (Show, Read, Eq, Ord)

instance FromJSON CabalConfigKey where
  parseJSON = withText "CabalConfigKey" parseCabalConfigKey

instance FromJSONKey CabalConfigKey where
  fromJSONKey = FromJSONKeyTextParser parseCabalConfigKey

parseCabalConfigKey :: (Monad m, MonadFail m) => Text -> m CabalConfigKey
parseCabalConfigKey "$targets" = pure CCKTargets
parseCabalConfigKey "$locals" = pure CCKLocals
parseCabalConfigKey "$everything" = pure CCKEverything
parseCabalConfigKey name =
  case parsePackageName $ T.unpack name of
    Nothing -> fail $ "Invalid CabalConfigKey: " ++ show name
    Just x -> pure $ CCKPackage x
