{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Execute commands within the properly configured Stack
-- environment.

module Stack.Exec where

import           Control.Exception.Lifted
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.Catch hiding (try)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Streaming.Process (ProcessExitedUnsuccessfully(..))
import           Stack.Types
import           System.Exit
import           System.Process.Run

-- | Default @EnvSettings@ which includes locals and GHC_PACKAGE_PATH
defaultEnvSettings :: EnvSettings
defaultEnvSettings = EnvSettings
    { esIncludeLocals = True
    , esIncludeGhcPackagePath = True
    , esStackExe = True
    }

-- | Environment settings which do not embellish the environment
plainEnvSettings :: EnvSettings
plainEnvSettings = EnvSettings
    { esIncludeLocals = False
    , esIncludeGhcPackagePath = False
    , esStackExe = False
    }

-- | Execute a process within the Stack configured environment.
exec :: (HasConfig r, MonadReader r m, MonadIO m, MonadLogger m, MonadThrow m, MonadBaseControl IO m)
     => EnvSettings -> String -> [String] -> m b
exec envSettings cmd args = do
    config <- asks getConfig
    menv <- liftIO (configEnvOverride config envSettings)
    e <- try (callProcess Nothing menv cmd args)
    liftIO $ case e of
        Left (ProcessExitedUnsuccessfully _ ec) -> exitWith ec
        Right () -> exitSuccess
