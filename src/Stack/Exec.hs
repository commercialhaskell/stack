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
import           System.Exit
import qualified System.Process as P
import           System.Process.Read


-- | Execute a process within the Stack configured environment.
exec :: (HasConfig r, MonadReader r m, MonadIO m, MonadLogger m, MonadThrow m)
        => String -> [String] -> m b
exec cmd args = do
    config <- asks getConfig
    menv <- liftIO (configEnvOverride config
                            EnvSettings
                                { esIncludeLocals = True
                                , esIncludeGhcPackagePath = True
                                })
    cmd' <- join $ System.Process.Read.findExecutable menv cmd
    let cp = (P.proc (toFilePath cmd') args)
            { P.env = envHelper menv
            , P.delegate_ctlc = True
            }
    $logProcessRun (toFilePath cmd') args
    (Nothing, Nothing, Nothing, ph) <- liftIO (P.createProcess cp)
    ec <- liftIO (P.waitForProcess ph)
    liftIO (exitWith ec)
