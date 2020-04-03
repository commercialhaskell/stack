{-# LANGUAGE ForeignFunctionInterface #-}
module System.Terminal
( fixCodePage
, getTerminalWidth
, hIsTerminalDeviceOrMinTTY
) where

import           Foreign
import           Foreign.C.Types
import           RIO (MonadIO, Handle, hIsTerminalDevice)

#include <sys/ioctl.h>
#include <unistd.h>


newtype WindowWidth = WindowWidth CUShort
    deriving (Eq, Ord, Show)

instance Storable WindowWidth where
  sizeOf _ = (#size struct winsize)
  alignment _ = (#alignment struct winsize)
  peek p = WindowWidth <$> (#peek struct winsize, ws_col) p
  poke p (WindowWidth w) = do
    (#poke struct winsize, ws_col) p w

foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr WindowWidth -> IO CInt

getTerminalWidth :: IO (Maybe Int)
getTerminalWidth =
    alloca $ \p -> do
        errno <- ioctl (#const STDOUT_FILENO) (#const TIOCGWINSZ) p
        if errno < 0
        then return Nothing
        else do
            WindowWidth w <- peek p
            return . Just . fromIntegral $ w

fixCodePage :: x -> y -> a -> a
fixCodePage _ _ = id

-- | hIsTerminaDevice does not recognise handles to mintty terminals as terminal
-- devices, but isMinTTYHandle does.
hIsTerminalDeviceOrMinTTY :: MonadIO m => Handle -> m Bool
hIsTerminalDeviceOrMinTTY = hIsTerminalDevice
