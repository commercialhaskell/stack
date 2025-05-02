{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE OverloadedStrings        #-}

{-|
Module      : System.Terminal
License     : BSD-3-Clause

The module of this name differs as between Windows and non-Windows builds. This
is the Windows version.
-}

module System.Terminal
  ( getTerminalWidth
  , hIsTerminalDeviceOrMinTTY
  ) where

import           Foreign.Marshal.Alloc ( allocaBytes )
import           Foreign.Ptr ( Ptr )
import           Foreign.Storable ( peekByteOff )
import           Stack.Prelude
import           System.IO ( hGetContents )
import           System.Process
                   ( StdStream (..), createProcess, shell, std_err, std_in
                   , std_out, waitForProcess
                   )
import           System.Win32 ( isMinTTYHandle, withHandleToHANDLE )

type HANDLE = Ptr ()

data CONSOLE_SCREEN_BUFFER_INFO

sizeCONSOLE_SCREEN_BUFFER_INFO :: Int
sizeCONSOLE_SCREEN_BUFFER_INFO = 22

posCONSOLE_SCREEN_BUFFER_INFO_srWindow :: Int
posCONSOLE_SCREEN_BUFFER_INFO_srWindow = 10 -- 4 x Word16 Left,Top,Right,Bottom

c_STD_OUTPUT_HANDLE :: Int
c_STD_OUTPUT_HANDLE = -11

foreign import ccall unsafe "windows.h GetConsoleScreenBufferInfo"
  c_GetConsoleScreenBufferInfo :: HANDLE -> Ptr CONSOLE_SCREEN_BUFFER_INFO -> IO Bool

foreign import ccall unsafe "windows.h GetStdHandle"
  c_GetStdHandle :: Int -> IO HANDLE

-- | If available, yields the width of the terminal.
getTerminalWidth :: IO (Maybe Int)
getTerminalWidth = do
  hdl <- c_GetStdHandle c_STD_OUTPUT_HANDLE
  allocaBytes sizeCONSOLE_SCREEN_BUFFER_INFO $ \p -> do
    b <- c_GetConsoleScreenBufferInfo hdl p
    if not b
      then do -- This could happen on Cygwin or MSYS
        let stty = (shell "stty size") {
              std_in  = UseHandle stdin
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
        (_, mbStdout, _, rStty) <- createProcess stty
        exStty <- waitForProcess rStty
        case exStty of
          ExitFailure _ -> pure Nothing
          ExitSuccess ->
            maybe (pure Nothing)
                  (\hSize -> do
                      sizeStr <- hGetContents hSize
                      case map readMaybe $ words sizeStr :: [Maybe Int] of
                        [Just _r, Just c] -> pure $ Just c
                        _ -> pure Nothing
                  )
                  mbStdout
      else do
        [left,_top,right,_bottom] <- forM [0..3] $ \i -> do
          v <- peekByteOff p (i * 2 + posCONSOLE_SCREEN_BUFFER_INFO_srWindow)
          pure $ fromIntegral (v :: Word16)
        pure $ Just (1 + right - left)

-- | hIsTerminaDevice does not recognise handles to mintty terminals as terminal
-- devices, but isMinTTYHandle does.
hIsTerminalDeviceOrMinTTY :: MonadIO m => Handle -> m Bool
hIsTerminalDeviceOrMinTTY h = do
  isTD <- hIsTerminalDevice h
  if isTD
    then pure True
    else liftIO $ withHandleToHANDLE h isMinTTYHandle
