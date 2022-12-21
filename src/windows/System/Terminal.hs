{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}

module System.Terminal
( fixCodePage
, getTerminalWidth
, hIsTerminalDeviceOrMinTTY
) where

import Distribution.Types.Version (mkVersion)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Stack.Prelude
import System.IO hiding (hIsTerminalDevice)
import System.Process
import System.Win32 (isMinTTYHandle, withHandleToHANDLE)
import System.Win32.Console (setConsoleCP, setConsoleOutputCP, getConsoleCP, getConsoleOutputCP)
import RIO.Partial (read)

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
                                  case map read $ words sizeStr :: [Int] of
                                    [_r, c] -> pure $ Just c
                                    _ -> pure Nothing
                              )
                              mbStdout
            else do
                [left,_top,right,_bottom] <- forM [0..3] $ \i -> do
                    v <- peekByteOff p ((i*2) + posCONSOLE_SCREEN_BUFFER_INFO_srWindow)
                    pure $ fromIntegral (v :: Word16)
                pure $ Just (1+right-left)

-- | Set the code page for this process as necessary. Only applies to Windows.
-- See: https://github.com/commercialhaskell/stack/issues/738
fixCodePage ::
     HasLogFunc env
  => Bool -- ^ modify code page?
  -> Version -- ^ GHC version
  -> RIO env a
  -> RIO env a
fixCodePage mcp ghcVersion inner = do
    if mcp && ghcVersion < mkVersion [7, 10, 3]
        then fixCodePage'
        -- GHC >=7.10.3 doesn't need this code page hack.
        else inner
  where
    fixCodePage' = do
        origCPI <- liftIO getConsoleCP
        origCPO <- liftIO getConsoleOutputCP

        let setInput = origCPI /= expected
            setOutput = origCPO /= expected
            fixInput
                | setInput = bracket_
                    (liftIO $ do
                        setConsoleCP expected)
                    (liftIO $ setConsoleCP origCPI)
                | otherwise = id
            fixOutput
                | setOutput = bracket_
                    (liftIO $ do
                        setConsoleOutputCP expected)
                    (liftIO $ setConsoleOutputCP origCPO)
                | otherwise = id

        case (setInput, setOutput) of
            (False, False) -> pure ()
            (True, True) -> warn ""
            (True, False) -> warn " input"
            (False, True) -> warn " output"

        fixInput $ fixOutput inner
    expected = 65001 -- UTF-8
    warn typ = logInfo $
        "Setting" <>
        typ <>
        " codepage to UTF-8 (65001) to ensure correct output from GHC"

-- | hIsTerminaDevice does not recognise handles to mintty terminals as terminal
-- devices, but isMinTTYHandle does.
hIsTerminalDeviceOrMinTTY :: MonadIO m => Handle -> m Bool
hIsTerminalDeviceOrMinTTY h = do
  isTD <- hIsTerminalDevice h
  if isTD
    then pure True
    else liftIO $ withHandleToHANDLE h isMinTTYHandle
