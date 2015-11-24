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
import           System.Process.Read (EnvOverride)

#ifdef WINDOWS
import           Control.Exception.Lifted
import           Data.Streaming.Process (ProcessExitedUnsuccessfully(..))
import           System.Exit
import           System.Process.Run (callProcess, Cmd(..))
#else
import           System.Process.Read (envHelper, preProcess)
import           System.Posix.Process (executeFile)
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
exec menv cmd0 args = do
    $logProcessRun cmd0 args
#ifdef WINDOWS
    e <- try (callProcess (Cmd Nothing cmd0 menv args))
    liftIO $ case e of
        Left (ProcessExitedUnsuccessfully _ ec) -> exitWith ec
        Right () -> exitSuccess
#else
    cmd <- preProcess Nothing menv cmd0
    liftIO $ executeFile cmd True args (envHelper menv)
#endif
