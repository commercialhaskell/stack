module Stack.DefaultColorWhen
  ( defaultColorWhen
  ) where

import           Stack.Prelude ( stdout )
import           Stack.Types.Config ( ColorWhen (ColorAuto, ColorNever) )
import           System.Console.ANSI ( hSupportsANSIWithoutEmulation )
import           System.Environment ( lookupEnv )

-- | The default adopts the standard proposed at http://no-color.org/, that
-- color should not be added by default if the @NO_COLOR@ environment variable
-- is present.
defaultColorWhen :: IO ColorWhen
defaultColorWhen = do
  -- On Windows, 'hSupportsANSIWithoutEmulation' has the side effect of enabling
  -- ANSI for ANSI-capable native (ConHost) terminals, if not already
  -- ANSI-enabled. Consequently, it is actioned even if @NO_COLOR@ might exist,
  -- as @NO_COLOR@ might be overridden in a yaml configuration file or at the
  -- command line.
  supportsANSI <- hSupportsANSIWithoutEmulation stdout
  mIsNoColor <- lookupEnv "NO_COLOR"
  pure $ case mIsNoColor of
    Just _ -> ColorNever
    _      -> case supportsANSI of
      Just False -> ColorNever
      _          -> ColorAuto
