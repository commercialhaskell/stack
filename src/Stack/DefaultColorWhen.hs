{-# LANGUAGE LambdaCase #-}

{-|
Module      : Stack.DefaultColorWhen
License     : BSD-3-Clause
-}

module Stack.DefaultColorWhen
  ( defaultColorWhen
  ) where

import           Stack.Prelude ( stdout )
import           Stack.Types.ColorWhen ( ColorWhen (..) )
import           System.Console.ANSI ( hNowSupportsANSI )
import           System.Environment ( lookupEnv )

-- | The default adopts the standard proposed at http://no-color.org/, that
-- color should not be added by default if the @NO_COLOR@ environment variable
-- is present.
defaultColorWhen :: IO ColorWhen
defaultColorWhen = lookupEnv "NO_COLOR" >>= \case
  Just _ -> pure ColorNever
  _ -> hNowSupportsANSI stdout >>= \case
    False -> pure ColorNever
    _ -> pure ColorAuto
