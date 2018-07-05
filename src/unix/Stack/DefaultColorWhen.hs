{- | This version of the module is only for non-Windows (eg unix-like)
operating systems.
-}
module Stack.DefaultColorWhen
  ( defaultColorWhen
  ) where

import Stack.Types.Runner (ColorWhen (ColorAuto))

defaultColorWhen :: IO ColorWhen
defaultColorWhen = return ColorAuto
