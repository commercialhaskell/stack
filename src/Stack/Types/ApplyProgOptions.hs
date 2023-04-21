{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.ApplyProgOptions
  ( ApplyProgOptions (..)
  ) where

import           Pantry.Internal.AesonExtended ( FromJSON (..), withText )
import           Stack.Prelude

-- | Which packages do all and any --PROG-option options on the command line
-- apply to?
data ApplyProgOptions
  = APOTargets -- ^ All local packages that are targets.
  | APOLocals -- ^ All local packages (targets or otherwise).
  | APOEverything -- ^ All packages (local or otherwise).
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance FromJSON ApplyProgOptions where
  parseJSON = withText "ApplyProgOptions" $ \t ->
    case t of
      "targets" -> pure APOTargets
      "locals" -> pure APOLocals
      "everything" -> pure APOEverything
      _ -> fail $ "Invalid ApplyProgOptions: " ++ show t
