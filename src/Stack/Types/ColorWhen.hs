{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Stack.Types.ColorWhen
License     : BSD-3-Clause
-}

module Stack.Types.ColorWhen
  ( ColorWhen (..)
  , readColorWhen
  ) where

import           Data.Aeson.Types ( FromJSON (..) )
import           Options.Applicative ( ReadM )
import qualified Options.Applicative.Types as OA
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
readColorWhen =
  OA.readerAsk >>= \case
    "never" -> pure ColorNever
    "always" -> pure ColorAlways
    "auto" -> pure ColorAuto
    _ -> OA.readerError "Expected values of color option are 'never', \
                        \'always', or 'auto'."
