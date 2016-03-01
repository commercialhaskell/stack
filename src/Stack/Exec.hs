{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Execute commands within the properly configured Stack
-- environment.

module Stack.Exec where

import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.Catch hiding (try)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Stack.Types
import           System.Process.Log

import           Control.Exception.Lifted
import           Data.Streaming.Process (ProcessExitedUnsuccessfully(..))
import           System.Exit
import           System.Process.Run (callProcess, Cmd(..))
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
exec :: (MonadIO m, MonadLogger m, MonadThrow m, MonadBaseControl IO m)
     => EnvOverride -> String -> [String] -> m b
#ifdef WINDOWS
exec = execSpawn
#else
exec menv cmd0 args = do
    $logProcessRun cmd0 args
    cmd <- preProcess Nothing menv cmd0
    liftIO $ executeFile cmd True args (envHelper menv)
#endif

-- | Execute a spawned process within the Stack configured environment.
execSpawn :: (MonadIO m, MonadLogger m, MonadThrow m, MonadBaseControl IO m)
     => EnvOverride -> String -> [String] -> m b
execSpawn menv cmd0 args = do
    $logProcessRun cmd0 args
    e <- try (callProcess (Cmd Nothing cmd0 menv args))
    liftIO $ case e of
        Left (ProcessExitedUnsuccessfully _ ec) -> exitWith ec
        Right () -> exitSuccess
