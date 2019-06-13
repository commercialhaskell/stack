module System.Terminal
( fixCodePage
, hIsTerminalDeviceOrMinTTY
) where

import           RIO (MonadIO, Handle, hIsTerminalDevice)

fixCodePage :: x -> y -> a -> a
fixCodePage _ _ = id

-- | hIsTerminaDevice does not recognise handles to mintty terminals as terminal
-- devices, but isMinTTYHandle does.
hIsTerminalDeviceOrMinTTY :: MonadIO m => Handle -> m Bool
hIsTerminalDeviceOrMinTTY = hIsTerminalDevice
