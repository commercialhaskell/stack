{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Types.GhcOptionKey
License     : BSD-3-Clause
-}

module Stack.Types.GhcOptionKey
  ( GhcOptionKey (..)
  ) where

import           Data.Aeson.Types ( FromJSONKey (..), FromJSONKeyFunction (..) )
import qualified Data.Text as T
import           Stack.Prelude

data GhcOptionKey
  = GOKOldEverything
  | GOKEverything
  | GOKLocals
  | GOKTargets
  | GOKPackage !PackageName
  deriving (Eq, Ord)

instance FromJSONKey GhcOptionKey where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    case t of
      "*" -> pure GOKOldEverything
      "$everything" -> pure GOKEverything
      "$locals" -> pure GOKLocals
      "$targets" -> pure GOKTargets
      _ ->
        case parsePackageName $ T.unpack t of
          Nothing -> fail $ "Invalid package name: " ++ show t
          Just x -> pure $ GOKPackage x
  fromJSONKeyList =
    FromJSONKeyTextParser $ \_ -> fail "GhcOptionKey.fromJSONKeyList"
