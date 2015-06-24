{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Execute commands within the properly configured Stack
-- environment.

module Stack.Exec where

import           Control.Monad.Reader
import           Control.Monad.Logger
import           Control.Monad.Catch


import           Path
import           Stack.Types
import           System.Directory (doesFileExist)
import           System.Exit
import qualified System.Process as P
import           System.Process.Read

-- | Default @EnvSettings@ which includes locals and GHC_PACKAGE_PATH
defaultEnvSettings :: EnvSettings
defaultEnvSettings = EnvSettings
    { esIncludeLocals = True
    , esIncludeGhcPackagePath = True
    }

-- | Execute a process within the Stack configured environment.
exec :: (HasConfig r, MonadReader r m, MonadIO m, MonadLogger m, MonadThrow m)
        => EnvSettings -> String -> [String] -> m b
exec envSettings cmd args = do
    config <- asks getConfig
    menv <- liftIO (configEnvOverride config envSettings)
    exists <- liftIO $ doesFileExist cmd
    cmd' <-
        if exists
            then return cmd
            else liftM toFilePath $ join $ System.Process.Read.findExecutable menv cmd
    let cp = (P.proc cmd' args)
            { P.env = envHelper menv
            , P.delegate_ctlc = True
            }
    $logProcessRun cmd' args
    (Nothing, Nothing, Nothing, ph) <- liftIO (P.createProcess cp)
    ec <- liftIO (P.waitForProcess ph)
    liftIO (exitWith ec)
