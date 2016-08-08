{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

-- | Execute commands within the properly configured Stack
-- environment.

module Stack.Exec where

import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Stack.Types
import           System.Process.Log

import           Control.Exception.Lifted
import           Data.Streaming.Process (ProcessExitedUnsuccessfully(..))
import           System.Exit
import           System.Process.Run (callProcess, callProcessObserveStdout, Cmd(..))
#ifdef WINDOWS
import           System.Process.Read (EnvOverride)
#else
import           System.Posix.Process (executeFile)
import           System.Process.Read (EnvOverride, envHelper, preProcess)
#endif

-- | Default @EnvSettings@ which includes locals and GHC_PACKAGE_PATH
defaultEnvSettings :: EnvSettings
defaultEnvSettings = EnvSettings
    { esIncludeLocals = True
    , esIncludeGhcPackagePath = True
    , esStackExe = True
    , esLocaleUtf8 = False
    }

-- | Environment settings which do not embellish the environment
plainEnvSettings :: EnvSettings
plainEnvSettings = EnvSettings
    { esIncludeLocals = False
    , esIncludeGhcPackagePath = False
    , esStackExe = False
    , esLocaleUtf8 = False
    }

-- | Execute a process within the Stack configured environment.
--
-- Execution will not return, because either:
--
-- 1) On non-windows, execution is taken over by execv of the
-- sub-process. This allows signals to be propagated (#527)
--
-- 2) On windows, an 'ExitCode' exception will be thrown.
exec :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
     => EnvOverride -> String -> [String] -> m b
#ifdef WINDOWS
exec = execSpawn
#else
exec menv cmd0 args = do
    $logProcessRun cmd0 args
    cmd <- preProcess Nothing menv cmd0
    liftIO $ executeFile cmd True args (envHelper menv)
#endif

-- | Like 'exec', but does not use 'execv' on non-windows. This way, there
-- is a sub-process, which is helpful in some cases (#1306)
--
-- This function only exits by throwing 'ExitCode'.
execSpawn :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
     => EnvOverride -> String -> [String] -> m b
execSpawn menv cmd0 args = do
    $logProcessRun cmd0 args
    e <- try (callProcess (Cmd Nothing cmd0 menv args))
    liftIO $ case e of
        Left (ProcessExitedUnsuccessfully _ ec) -> exitWith ec
        Right () -> exitSuccess

execObserve :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
    => EnvOverride -> String -> [String] -> m String
execObserve menv cmd0 args = do
    $logProcessRun cmd0 args
    e <- try (callProcessObserveStdout (Cmd Nothing cmd0 menv args))
    case e of
        Left (ProcessExitedUnsuccessfully _ ec) -> liftIO $ exitWith ec
        Right s -> return s
