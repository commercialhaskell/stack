{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
-- | Run sub-processes.

module System.Process.Run
    (runCmd
    ,runCmd'
    ,callProcess
    ,callProcess'
    ,callProcessInheritStderrStdout
    ,createProcess'
    ,ProcessExitedUnsuccessfully
    ,Cmd(..)
    )
    where

import           Control.Exception.Lifted
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (MonadLogger, logError)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Conduit.Process hiding (callProcess)
import           Data.Foldable (forM_)
import           Data.Text (Text)
import qualified Data.Text as T
import           Path (Dir, Abs, Path)
import           Path (toFilePath)
import           Prelude -- Fix AMP warning
import           System.Exit (exitWith, ExitCode (..))
import           System.IO
import qualified System.Process
import           System.Process.Log
import           System.Process.Read

-- | Cmd holds common infos needed to running a process in most cases
data Cmd = Cmd
  { cmdDirectoryToRunIn :: Maybe (Path Abs Dir) -- ^ directory to run in
  , cmdCommandToRun :: FilePath -- ^ command to run
  , cmdEnvOverride::EnvOverride
  , cmdCommandLineArguments :: [String] -- ^ command line arguments
  }

-- | Run the given command in the given directory, inheriting stdout and stderr.
--
-- If it exits with anything but success, prints an error
-- and then calls 'exitWith' to exit the program.
runCmd :: forall (m :: * -> *).
         (MonadLogger m,MonadIO m,MonadBaseControl IO m)
      => Cmd
      -> Maybe Text  -- ^ optional additional error message
      -> m ()
runCmd = runCmd' id

runCmd' :: forall (m :: * -> *).
         (MonadLogger m,MonadIO m,MonadBaseControl IO m)
      => (CreateProcess -> CreateProcess)
      -> Cmd
      -> Maybe Text  -- ^ optional additional error message
      -> m ()
runCmd' modCP cmd@(Cmd{..}) mbErrMsg = do
    result <- try (callProcess' modCP cmd)
    case result of
        Left (ProcessExitedUnsuccessfully _ ec) -> do
            $logError $
                T.pack $
                concat $
                    [ "Exit code "
                    , show ec
                    , " while running "
                    , show (cmdCommandToRun : cmdCommandLineArguments)
                    ] ++ (case cmdDirectoryToRunIn of
                            Nothing -> []
                            Just mbDir -> [" in ", toFilePath mbDir]
                            )
            forM_ mbErrMsg $logError
            liftIO (exitWith ec)
        Right () -> return ()

-- | Like 'System.Process.callProcess', but takes an optional working directory and
-- environment override, and throws 'ProcessExitedUnsuccessfully' if the
-- process exits unsuccessfully.
--
-- Inherits stdout and stderr.
callProcess :: (MonadIO m, MonadLogger m) => Cmd -> m ()
callProcess = callProcess' id

-- | Like 'System.Process.callProcess', but takes an optional working directory and
-- environment override, and throws 'ProcessExitedUnsuccessfully' if the
-- process exits unsuccessfully.
--
-- Inherits stdout and stderr.
callProcess' :: (MonadIO m, MonadLogger m)
             => (CreateProcess -> CreateProcess) -> Cmd -> m ()
callProcess' modCP cmd = do
    c <- liftM modCP (cmdToCreateProcess cmd)
    $logCreateProcess c
    liftIO $ do
        (_, _, _, p) <- System.Process.createProcess c
        exit_code <- waitForProcess p
        case exit_code of
            ExitSuccess   -> return ()
            ExitFailure _ -> throwIO (ProcessExitedUnsuccessfully c exit_code)

callProcessInheritStderrStdout :: (MonadIO m, MonadLogger m) => Cmd -> m ()
callProcessInheritStderrStdout cmd = do
    let inheritOutput cp = cp { std_in = CreatePipe, std_out = Inherit, std_err = Inherit }
    callProcess' inheritOutput cmd

-- | Like 'System.Process.Internal.createProcess_', but taking a 'Cmd'.
-- Note that the 'Handle's provided by 'UseHandle' are not closed
-- automatically.
createProcess' :: (MonadIO m, MonadLogger m)
               => String
               -> (CreateProcess -> CreateProcess)
               -> Cmd
               -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess' tag modCP cmd = do
    c <- liftM modCP (cmdToCreateProcess cmd)
    $logCreateProcess c
    liftIO $ System.Process.createProcess_ tag c

cmdToCreateProcess :: MonadIO m => Cmd -> m CreateProcess
cmdToCreateProcess (Cmd wd cmd0 menv args) = do
    cmd <- preProcess wd menv cmd0
    return $ (proc cmd args) { delegate_ctlc = True
                             , cwd = fmap toFilePath wd
                             , env = envHelper menv }
