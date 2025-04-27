{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Types.SCM
License     : BSD-3-Clause
-}

module Stack.Types.SCM
  ( SCM (..)
  ) where

import           Data.Aeson.Types ( FromJSON (..), ToJSON (..) )
import           Stack.Prelude

-- | A software control system.
data SCM
  = Git
  deriving Show

instance FromJSON SCM where
  parseJSON v = do
    s <- parseJSON v
    case s of
      "git" -> pure Git
      _ -> fail ("Unknown or unsupported SCM: " <> s)

instance ToJSON SCM where
  toJSON Git = toJSON ("git" :: Text)
