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

-- | Type representing scopes of the application by Stack of GHC options.
data GhcOptionKey
  = GOKOldEverything
    -- ^ All packages, project packages or otherwise (specified with legacy
    -- syntax).
  | GOKEverything
    -- ^ All packages, project packages or otherwise.
  | GOKLocals
    -- ^ All project packages, targets or otherwise.
  | GOKTargets
    -- ^ All project packages that are targets.
  | GOKPackage !PackageName
    -- ^ A named package.
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
