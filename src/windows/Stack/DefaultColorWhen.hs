{-# LANGUAGE CPP #-}

{- | This version of the module is only for Windows operating systems.
-}
module Stack.DefaultColorWhen
  ( defaultColorWhen
  ) where

-- The Win32 package provides CPP macro WINDOWS_CCONV.
#include "windows_cconv.h"

import Stack.Prelude (stdout)
import Stack.Types.Runner (ColorWhen (ColorAuto, ColorNever))

import Data.Bits ((.|.), (.&.))
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.Win32.Types (BOOL, DWORD, HANDLE, iNVALID_HANDLE_VALUE,
  nullHANDLE, withHandleToHANDLE)

defaultColorWhen :: IO ColorWhen
defaultColorWhen = withHandleToHANDLE stdout aNSISupport

-- The following is based on extracts from the modules
-- System.Console.ANSI.Windows.Foreign and System.Console.ANSI.Windows.Detect
-- from the ansi-terminal package, simplified.

-- | This function first checks if the Windows handle is valid and yields
-- 'never' if it is not. It then tries to get a ConHost console mode for that
-- handle. If it can not, it assumes that the handle is ANSI-enabled. If virtual
-- termimal (VT) processing is already enabled, the handle supports 'auto'.
-- Otherwise, it trys to enable processing. If it can, the handle supports
-- 'auto'. If it can not, the function yields 'never'.
aNSISupport :: HANDLE -> IO ColorWhen
aNSISupport h =
  if h == iNVALID_HANDLE_VALUE || h == nullHANDLE
    then return ColorNever  -- Invalid handle or no handle
    else do
      tryMode <- getConsoleMode h
      case tryMode of
        Nothing     -> return ColorAuto  -- No ConHost mode
        Just mode   -> if mode .&. eNABLE_VIRTUAL_TERMINAL_PROCESSING /= 0
          then return ColorAuto  -- VT processing already enabled
          else do
            let mode' = mode .|. eNABLE_VIRTUAL_TERMINAL_PROCESSING
            succeeded <- cSetConsoleMode h mode'
            if succeeded
              then return ColorAuto  -- VT processing enabled
              else return ColorNever -- Can't enable VT processing
 where
  eNABLE_VIRTUAL_TERMINAL_PROCESSING = 4

foreign import WINDOWS_CCONV unsafe "windows.h GetConsoleMode"
  cGetConsoleMode :: HANDLE -> Ptr DWORD -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleMode"
  cSetConsoleMode :: HANDLE -> DWORD -> IO BOOL

getConsoleMode :: HANDLE -> IO (Maybe DWORD)
getConsoleMode handle = alloca $ \ptr_mode -> do
  succeeded <- cGetConsoleMode handle ptr_mode
  if succeeded
    then Just <$> peek ptr_mode
    else pure Nothing
