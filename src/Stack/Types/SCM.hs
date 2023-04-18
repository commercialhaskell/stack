{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.SCM
  ( SCM (..)
  ) where

import           Pantry.Internal.AesonExtended ( FromJSON (..), ToJSON (..) )
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
