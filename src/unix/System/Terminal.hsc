{-# LANGUAGE CApiFFI                  #-}

-- | The module of this name differs as between Windows and non-Windows builds.
-- This is the non-Windows version.
module System.Terminal
( getTerminalWidth
, hIsTerminalDeviceOrMinTTY
) where

import           Foreign
import           Foreign.C.Types
import           RIO ( Handle, MonadIO, hIsTerminalDevice )

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

-- `ioctl` is variadic, so `capi` is needed, see:
-- https://www.haskell.org/ghc/blog/20210709-capi-usage.html
foreign import capi "sys/ioctl.h ioctl"
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

-- | hIsTerminaDevice does not recognise handles to mintty terminals as terminal
-- devices, but isMinTTYHandle does.
hIsTerminalDeviceOrMinTTY :: MonadIO m => Handle -> m Bool
hIsTerminalDeviceOrMinTTY = hIsTerminalDevice
