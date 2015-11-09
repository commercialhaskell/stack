{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Run sub-processes.

module System.Process.Run
    (runIn
    ,callProcess
    ,callProcess'
    ,ProcessExitedUnsuccessfully)
    where

import           Control.Exception.Lifted
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (MonadLogger, logError)
import           Data.Conduit.Process hiding (callProcess)
import           Data.Foldable (forM_)
import           Data.Text (Text)
import qualified Data.Text as T
import           Path (Path, Abs, Dir, toFilePath)
import           Prelude -- Fix AMP warning
import           System.Exit (exitWith, ExitCode (..))
import qualified System.Process
import           System.Process.Read

-- | Run the given command in the given directory, inheriting stdout and stderr.
--
-- If it exits with anything but success, prints an error
-- and then calls 'exitWith' to exit the program.
runIn :: forall (m :: * -> *).
         (MonadLogger m,MonadIO m,MonadBaseControl IO m)
      => Path Abs Dir -- ^ directory to run in
      -> FilePath -- ^ command to run
      -> EnvOverride
      -> [String] -- ^ command line arguments
      -> Maybe Text -- ^ optional additional error message
      -> m ()
runIn wd cmd menv args errMsg = do
    result <- try (callProcess (Just wd) menv cmd args)
    case result of
        Left (ProcessExitedUnsuccessfully _ ec) -> do
            $logError $
                T.pack $
                concat
                    [ "Exit code "
                    , show ec
                    , " while running "
                    , show (cmd : args)
                    , " in "
                    , toFilePath wd]
            forM_ errMsg $logError
            liftIO (exitWith ec)
        Right () -> return ()

-- | Like 'System.Process.callProcess', but takes an optional working directory and
-- environment override, and throws 'ProcessExitedUnsuccessfully' if the
-- process exits unsuccessfully.
--
-- Inherits stdout and stderr.
callProcess :: (MonadIO m, MonadLogger m)
            => Maybe (Path Abs Dir) -- ^ optional directory to run in
            -> EnvOverride
            -> String -- ^ command to run
            -> [String] -- ^ command line arguments
            -> m ()
callProcess =
    callProcess' id

-- | Like 'System.Process.callProcess', but takes an optional working directory and
-- environment override, and throws 'ProcessExitedUnsuccessfully' if the
-- process exits unsuccessfully.
--
-- Inherits stdout and stderr.
callProcess' :: (MonadIO m, MonadLogger m)
             => (CreateProcess -> CreateProcess)
             -> Maybe (Path Abs Dir) -- ^ optional directory to run in
             -> EnvOverride
             -> String -- ^ command to run
             -> [String] -- ^ command line arguments
             -> m ()
callProcess' modCP wd menv cmd0 args = do
    cmd <- preProcess wd menv cmd0
    let c = modCP $ (proc cmd args) { delegate_ctlc = True
                                    , cwd = fmap toFilePath wd
                                    , env = envHelper menv }
        action (_, _, _, p) = do
            exit_code <- waitForProcess p
            case exit_code of
              ExitSuccess   -> return ()
              ExitFailure _ -> throwIO (ProcessExitedUnsuccessfully c exit_code)
    $logProcessRun cmd args
    liftIO (System.Process.createProcess c >>= action)
