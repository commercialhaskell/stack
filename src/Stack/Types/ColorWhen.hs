{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Types.ColorWhen
  ( ColorWhen (..)
  , readColorWhen
  ) where

import           Options.Applicative ( ReadM )
import qualified Options.Applicative.Types as OA
import           Pantry.Internal.AesonExtended ( FromJSON (..) )
import           Stack.Prelude

data ColorWhen
  = ColorNever
  | ColorAlways
  | ColorAuto
  deriving (Eq, Generic, Show)

instance FromJSON ColorWhen where
  parseJSON v = do
    s <- parseJSON v
    case s of
      "never"  -> pure ColorNever
      "always" -> pure ColorAlways
      "auto"   -> pure ColorAuto
      _ -> fail ("Unknown color use: " <> s <> ". Expected values of " <>
                 "option are 'never', 'always', or 'auto'.")

readColorWhen :: ReadM ColorWhen
readColorWhen = do
  s <- OA.readerAsk
  case s of
    "never" -> pure ColorNever
    "always" -> pure ColorAlways
    "auto" -> pure ColorAuto
    _ -> OA.readerError "Expected values of color option are 'never', \
                        \'always', or 'auto'."
