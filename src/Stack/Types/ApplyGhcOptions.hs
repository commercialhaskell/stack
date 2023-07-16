{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.ApplyGhcOptions
  ( ApplyGhcOptions (..)
  ) where

import           Data.Aeson.Types ( FromJSON (..), withText )
import           Stack.Prelude

-- | Which packages do ghc-options on the command line apply to?
data ApplyGhcOptions
  = AGOTargets -- ^ all local targets
  | AGOLocals -- ^ all local packages, even non-targets
  | AGOEverything -- ^ every package
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance FromJSON ApplyGhcOptions where
  parseJSON = withText "ApplyGhcOptions" $ \t ->
    case t of
      "targets" -> pure AGOTargets
      "locals" -> pure AGOLocals
      "everything" -> pure AGOEverything
      _ -> fail $ "Invalid ApplyGhcOptions: " ++ show t
