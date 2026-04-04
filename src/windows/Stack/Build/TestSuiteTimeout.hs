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

import           RIO.Process ( ProcessConfig )
import qualified RIO.Process as RP ( Process, unsafeProcessHandle )
import           Stack.Prelude
import qualified System.Process as Process

prepareForEscalation :: ProcessConfig stdin stdout stderr -> ProcessConfig stdin stdout stderr
prepareForEscalation = id

terminateGracefully :: RP.Process stdin stdout stderr -> RIO env ()
terminateGracefully p =
  void $ tryAny $ liftIO $ Process.terminateProcess $ RP.unsafeProcessHandle p

forceKill :: RP.Process stdin stdout stderr -> RIO env ()
forceKill p =
  void $ tryAny $ liftIO $ Process.terminateProcess $ RP.unsafeProcessHandle p
