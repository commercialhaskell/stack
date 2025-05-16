{- |
Integration-test helpers & fixtures for testing `stack repl`
-}
module StackTest.Repl
    ( Repl
    , ReplConnection (..)
    , nextPrompt
    , repl
    , replCommand
    , replGetLine
    ) where

import Control.Concurrent (forkIO)
import Control.Exception (throw, catch)
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO
    ( BufferMode (NoBuffering), Handle, IOMode (WriteMode)
    , hGetChar, hGetLine, hPutChar, hPutStrLn, hSetBuffering
    , withFile
    )
import System.IO.Error (isEOFError)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State qualified as State
import System.Process
    ( CreateProcess (std_err, std_in, std_out)
    , StdStream (CreatePipe)
    , createProcess, proc, waitForProcess
    )

import StackTest

type Repl = ReaderT ReplConnection IO

data ReplConnection = ReplConnection
  { replStdin  :: Handle
  , replStdout :: Handle
  }

nextPrompt :: Repl ()
nextPrompt = State.evalStateT poll "" where
  poll = do
    c <- lift (asks replStdout) >>= liftIO . hGetChar
    State.modify (++ [c]) -- FIXME crap perf
    when (c == '\n') $ do
      State.get >>= liftIO . putStr . ("ghci> " <>)
      State.put ""
    buf <- State.get
    unless (buf == "ghci> ")
      poll

replCommand :: String -> Repl ()
replCommand cmd = do
  (ReplConnection replStdinHandle _) <- ask
  liftIO $ hPutStrLn replStdinHandle cmd

replGetLine :: Repl String
replGetLine = ask >>= liftIO . hGetLine . replStdout

runRepl
  :: HasCallStack
  => FilePath
  -> [String]
  -> ReaderT ReplConnection IO ()
  -> IO ExitCode
runRepl cmd args actions = do
  logInfo $ "Running: " ++ cmd ++ " " ++ unwords (map showProcessArgDebug args)
  (Just rStdin, Just rStdout, Just rStderr, ph) <-
    createProcess (proc cmd args)
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }
  hSetBuffering rStdin NoBuffering
  hSetBuffering rStdout NoBuffering
  hSetBuffering rStderr NoBuffering
  -- Log stack repl's standard error output
  tempDir <- if isWindows
                then fromMaybe "" <$> lookupEnv "TEMP"
                else pure "/tmp"
  let tempLogFile = tempDir ++ "/stderr"
  _ <- forkIO $ withFile tempLogFile WriteMode $ \logFileHandle -> do
    --hSetBuffering logFileHandle NoBuffering
    forever $
      catch
        (hGetChar rStderr >>= hPutChar logFileHandle)
        (\e -> unless (isEOFError e) $ throw e)
  runReaderT actions (ReplConnection rStdin rStdout)
  waitForProcess ph

repl :: HasCallStack => [String] -> Repl () -> IO ()
repl args action = do
  stackExe' <- stackExe
  ec <- runRepl stackExe' ("repl" : "--ghci-options=-ignore-dot-ghci" : args) action
  unless (ec == ExitSuccess) $ pure ()
  -- TODO: Understand why the exit code is 1 despite running GHCi tests
  -- successfully.
  -- else error $ "Exited with exit code: " ++ show ec
