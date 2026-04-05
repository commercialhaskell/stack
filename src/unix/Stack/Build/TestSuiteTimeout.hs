{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Stack.Build.TestSuiteTimeout
Description : OS-specific test suite timeout termination helpers.
License     : BSD-3-Clause
-}

module Stack.Build.TestSuiteTimeout
  ( prepareForEscalation
  , terminateGracefully
  , forceKill
  ) where

import           RIO.Process ( ProcessConfig, setNewSession )
import qualified RIO.Process as RP ( Process, unsafeProcessHandle )
import           Stack.Prelude
import           System.Posix.Signals
                   ( sigKILL, sigTERM, signalProcess, signalProcessGroup )
import qualified System.Process as Process

prepareForEscalation :: ProcessConfig stdin stdout stderr -> ProcessConfig stdin stdout stderr
prepareForEscalation = setNewSession True

terminateGracefully :: RP.Process stdin stdout stderr -> RIO env ()
terminateGracefully p = do
  let processHandle = RP.unsafeProcessHandle p
  mpid <- liftIO $ Process.getPid processHandle
  forM_ mpid $ \pid -> do
    -- In a new session, the initial pid is also the process group id.
    void $ tryAny $ liftIO $ signalProcessGroup sigTERM pid
    void $ tryAny $ liftIO $ signalProcess sigTERM pid

forceKill :: RP.Process stdin stdout stderr -> RIO env ()
forceKill p = do
  let processHandle = RP.unsafeProcessHandle p
  mpid <- liftIO $ Process.getPid processHandle
  forM_ mpid $ \pid -> do
    void $ tryAny $ liftIO $ signalProcessGroup sigKILL pid
    void $ tryAny $ liftIO $ signalProcess sigKILL pid
