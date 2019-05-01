{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Terminal
( fixCodePage
, hIsTerminalDeviceOrMinTTY
) where

import Distribution.Types.Version (mkVersion)
import Stack.Prelude
import System.Win32 (isMinTTYHandle, withHandleToHANDLE)
import System.Win32.Console (setConsoleCP, setConsoleOutputCP, getConsoleCP, getConsoleOutputCP)

-- | Set the code page for this process as necessary. Only applies to Windows.
-- See: https://github.com/commercialhaskell/stack/issues/738
fixCodePage
  :: HasLogFunc env
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
            (False, False) -> return ()
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
    then return True
    else liftIO $ withHandleToHANDLE h isMinTTYHandle
