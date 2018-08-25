{- | This version of the module is only for non-Windows (eg unix-like)
operating systems.
-}
module Stack.DefaultColorWhen
  ( defaultColorWhen
  ) where

import Stack.Types.Runner (ColorWhen (ColorAuto, ColorNever))
import System.Environment (lookupEnv)

-- |The default adopts the standard proposed at http://no-color.org/, that color
-- should not be added by default if the @NO_COLOR@ environment variable is
-- present.
defaultColorWhen :: IO ColorWhen
defaultColorWhen =  do
  mIsNoColor <- lookupEnv "NO_COLOR"
  return $ case mIsNoColor of
    Just _ -> ColorNever
    _      -> ColorAuto
