{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Types.Cache
License     : BSD-3-Clause
-}

module Stack.Types.Cache
  ( ConfigCacheType (..)
  , Action (..)
  ) where

import qualified Data.Text as T
import           Database.Persist.Sql
                   ( PersistField (..), PersistFieldSql (..), PersistValue (..)
                   , SqlType (..)
                   )
import           Stack.Prelude
import           Stack.Types.GhcPkgId
                   ( GhcPkgId, ghcPkgIdToText, parseGhcPkgId )

-- | Type of config cache
data ConfigCacheType
  = ConfigCacheTypeConfig
  | ConfigCacheTypeFlagLibrary GhcPkgId
  | ConfigCacheTypeFlagExecutable PackageIdentifier
  deriving (Eq, Show)

instance PersistField ConfigCacheType where
  toPersistValue ConfigCacheTypeConfig = PersistText "config"
  toPersistValue (ConfigCacheTypeFlagLibrary v) =
    PersistText $ "lib:" <> ghcPkgIdToText v
  toPersistValue (ConfigCacheTypeFlagExecutable v) =
    PersistText $ "exe:" <> T.pack (packageIdentifierString v)
  fromPersistValue (PersistText t) =
    fromMaybe (Left $ "Unexpected ConfigCacheType value: " <> t) $
    config <|> fmap lib (T.stripPrefix "lib:" t) <|>
    fmap exe (T.stripPrefix "exe:" t)
   where
    config
      | t == "config" = Just (Right ConfigCacheTypeConfig)
      | otherwise = Nothing
    lib v = do
      ghcPkgId <- mapLeft tshow (parseGhcPkgId v)
      Right $ ConfigCacheTypeFlagLibrary ghcPkgId
    exe v = do
      pkgId <-
        maybe (Left $ "Unexpected ConfigCacheType value: " <> t) Right $
        parsePackageIdentifier (T.unpack v)
      Right $ ConfigCacheTypeFlagExecutable pkgId
  fromPersistValue _ = Left "Unexpected ConfigCacheType type"

instance PersistFieldSql ConfigCacheType where
  sqlType _ = SqlString

data Action
  = UpgradeCheck
  deriving (Eq, Ord, Show)

instance PersistField Action where
  toPersistValue UpgradeCheck = PersistInt64 1
  fromPersistValue (PersistInt64 1) = Right UpgradeCheck
  fromPersistValue x = Left $ T.pack $ "Invalid Action: " ++ show x

instance PersistFieldSql Action where
  sqlType _ = SqlInt64
