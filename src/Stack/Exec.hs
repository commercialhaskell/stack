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

#ifdef mingw32_HOST_OS
import           Control.Exception.Lifted
import           Data.Streaming.Process (ProcessExitedUnsuccessfully(..))
import           System.Exit
import           System.Process.Run (callProcess)
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
exec envSettings cmd0 args = do
    config <- asks getConfig
    menv <- liftIO (configEnvOverride config envSettings)
#ifdef mingw32_HOST_OS
    e <- try (callProcess Nothing menv cmd0 args)
    liftIO $ case e of
        Left (ProcessExitedUnsuccessfully _ ec) -> exitWith ec
        Right () -> exitSuccess
#else
    cmd <- preProcess Nothing menv cmd0
    liftIO $ executeFile cmd False args (envHelper menv)
#endif
