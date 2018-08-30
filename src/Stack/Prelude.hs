{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Stack.Prelude
  ( withSystemTempDir
  , withKeepSystemTempDir
  , sinkProcessStderrStdout
  , sinkProcessStdout
  , logProcessStderrStdout
  , readProcessNull
  , withProcessContext
  , stripCR
  , prompt
  , promptPassword
  , promptBool
  , stackProgName
  , module X
  ) where

import           RIO                  as X
import           Data.Conduit         as X (ConduitM, runConduit, (.|))
import           Path                 as X (Abs, Dir, File, Path, Rel,
                                            toFilePath)
import           Pantry               as X hiding (Package (..), loadSnapshot)

import           Data.Monoid          as X (First (..), Any (..), Sum (..), Endo (..))

import qualified Path.IO

import           System.IO.Echo (withoutInputEcho)

import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process.Typed (withLoggedProcess_, createSource)
import           RIO.Process (HasProcessContext (..), ProcessContext, setStdin, closed, getStderr, getStdout, proc, withProcess_, setStdout, setStderr, ProcessConfig, readProcess_, workingDirL)
import           Data.Store           as X (Store)
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)

import qualified Data.Text.IO as T
import qualified RIO.Text as T

-- | Path version
withSystemTempDir :: MonadUnliftIO m => String -> (Path Abs Dir -> m a) -> m a
withSystemTempDir str inner = withRunInIO $ \run -> Path.IO.withSystemTempDir str $ run . inner

-- | Like `withSystemTempDir`, but the temporary directory is not deleted.
withKeepSystemTempDir :: MonadUnliftIO m => String -> (Path Abs Dir -> m a) -> m a
withKeepSystemTempDir str inner = withRunInIO $ \run -> do
  path <- Path.IO.getTempDir
  dir <- Path.IO.createTempDir path str
  run $ inner dir

-- | Consume the stdout and stderr of a process feeding strict 'ByteString's to the consumers.
--
-- Throws a 'ReadProcessException' if unsuccessful in launching, or 'ProcessExitedUnsuccessfully' if the process itself fails.
sinkProcessStderrStdout
  :: forall e o env. (HasProcessContext env, HasLogFunc env, HasCallStack)
  => String -- ^ Command
  -> [String] -- ^ Command line arguments
  -> ConduitM ByteString Void (RIO env) e -- ^ Sink for stderr
  -> ConduitM ByteString Void (RIO env) o -- ^ Sink for stdout
  -> RIO env (e,o)
sinkProcessStderrStdout name args sinkStderr sinkStdout =
  proc name args $ \pc0 -> do
    let pc = setStdout createSource
           $ setStderr createSource
             pc0
    withProcess_ pc $ \p ->
      runConduit (getStderr p .| sinkStderr) `concurrently`
      runConduit (getStdout p .| sinkStdout)

-- | Consume the stdout of a process feeding strict 'ByteString's to a consumer.
-- If the process fails, spits out stdout and stderr as error log
-- level. Should not be used for long-running processes or ones with
-- lots of output; for that use 'sinkProcessStderrStdout'.
--
-- Throws a 'ReadProcessException' if unsuccessful.
sinkProcessStdout
    :: (HasProcessContext env, HasLogFunc env, HasCallStack)
    => String -- ^ Command
    -> [String] -- ^ Command line arguments
    -> ConduitM ByteString Void (RIO env) a -- ^ Sink for stdout
    -> RIO env a
sinkProcessStdout name args sinkStdout =
  proc name args $ \pc ->
  withLoggedProcess_ (setStdin closed pc) $ \p -> runConcurrently
    $ Concurrently (runConduit $ getStderr p .| CL.sinkNull)
   *> Concurrently (runConduit $ getStdout p .| sinkStdout)

logProcessStderrStdout
    :: (HasCallStack, HasProcessContext env, HasLogFunc env)
    => ProcessConfig stdin stdoutIgnored stderrIgnored
    -> RIO env ()
logProcessStderrStdout pc = withLoggedProcess_ pc $ \p ->
    let logLines = CB.lines .| CL.mapM_ (logInfo . displayBytesUtf8)
     in runConcurrently
            $ Concurrently (runConduit $ getStdout p .| logLines)
           *> Concurrently (runConduit $ getStderr p .| logLines)

-- | Read from the process, ignoring any output.
--
-- Throws a 'ReadProcessException' exception if the process fails.
readProcessNull :: (HasProcessContext env, HasLogFunc env, HasCallStack)
                => String -- ^ Command
                -> [String] -- ^ Command line arguments
                -> RIO env ()
readProcessNull name args =
  -- We want the output to appear in any exceptions, so we capture and drop it
  void $ proc name args readProcess_

-- | Use the new 'ProcessContext', but retain the working directory
-- from the parent environment.
withProcessContext :: HasProcessContext env => ProcessContext -> RIO env a -> RIO env a
withProcessContext pcNew inner = do
  pcOld <- view processContextL
  let pcNew' = set workingDirL (view workingDirL pcOld) pcNew
  local (set processContextL pcNew') inner

-- | Remove a trailing carriage return if present
stripCR :: Text -> Text
stripCR = T.dropSuffix "\r"

-- | Prompt the user by sending text to stdout, and taking a line of
-- input from stdin.
prompt :: MonadIO m => Text -> m Text
prompt txt = liftIO $ do
  T.putStr txt
  hFlush stdout
  T.getLine

-- | Prompt the user by sending text to stdout, and collecting a line
-- of input from stdin. While taking input from stdin, input echoing is
-- disabled, to hide passwords.
--
-- Based on code from cabal-install, Distribution.Client.Upload
promptPassword :: MonadIO m => Text -> m Text
promptPassword txt = liftIO $ do
  T.putStr txt
  hFlush stdout
  -- Save/restore the terminal echoing status (no echoing for entering
  -- the password).
  password <- withoutInputEcho T.getLine
  -- Since the user's newline is not echoed, one needs to be inserted.
  T.putStrLn ""
  return password

-- | Prompt the user by sending text to stdout, and collecting a line of
-- input from stdin. If something other than "y" or "n" is entered, then
-- print a message indicating that "y" or "n" is expected, and ask
-- again.
promptBool :: MonadIO m => Text -> m Bool
promptBool txt = liftIO $ do
  input <- prompt txt
  case input of
    "y" -> return True
    "n" -> return False
    _ -> do
      T.putStrLn "Please press either 'y' or 'n', and then enter."
      promptBool txt

-- | Name of the 'stack' program.
--
-- NOTE: Should be defined in "Stack.Constants", but not doing so due to the
-- GHC stage restrictions.
stackProgName :: String
stackProgName = "stack"
