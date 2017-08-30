{-# LANGUAGE CPP #-}
#ifndef WINDOWS
{-# LANGUAGE ForeignFunctionInterface #-}
#endif

module System.Terminal
( getTerminalWidth
) where

import           Foreign
import           Foreign.C.Types

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

-- | Get the width, in columns, of the terminal if we can.
getTerminalWidth :: IO (Maybe Int)
#ifndef WINDOWS
getTerminalWidth = pure Nothing
#else
getTerminalWidth = pure Nothing
#endif
