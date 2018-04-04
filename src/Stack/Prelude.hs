{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Stack.Prelude
  ( withSourceFile
  , withSinkFile
  , withSinkFileCautious
  , withSystemTempDir
  , withKeepSystemTempDir
  , sinkProcessStderrStdout
  , sinkProcessStdout
  , logProcessStderrStdout
  , readProcessNull
  , withProcessContext
  , stripCR
  , module X
  ) where

import           RIO                  as X
import           Data.Conduit         as X (ConduitM, runConduit, (.|))
import           Path                 as X (Abs, Dir, File, Path, Rel,
                                            toFilePath)

import           Data.Monoid          as X (First (..), Any (..), Sum (..), Endo (..))

import qualified Path.IO

import qualified System.IO as IO
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import           System.IO.Error (isDoesNotExistError)

import           Data.Conduit.Binary (sourceHandle, sinkHandle)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process.Typed (withLoggedProcess_, createSource)
import           RIO.Process (HasProcessContext (..), ProcessContext, setStdin, closed, getStderr, getStdout, proc, withProcess_, setStdout, setStderr, ProcessConfig, readProcessStdout_, workingDirL)
import           Data.Store           as X (Store)
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)

import qualified RIO.Text as T

-- | Get a source for a file. Unlike @sourceFile@, doesn't require
-- @ResourceT@. Unlike explicit @withBinaryFile@ and @sourceHandle@
-- usage, you can't accidentally use @WriteMode@ instead of
-- @ReadMode@.
withSourceFile :: MonadUnliftIO m => FilePath -> (ConduitM i ByteString m () -> m a) -> m a
withSourceFile fp inner = withBinaryFile fp ReadMode $ inner . sourceHandle

-- | Same idea as 'withSourceFile', see comments there.
withSinkFile :: MonadUnliftIO m => FilePath -> (ConduitM ByteString o m () -> m a) -> m a
withSinkFile fp inner = withBinaryFile fp WriteMode $ inner . sinkHandle

-- | Like 'withSinkFile', but ensures that the file is atomically
-- moved after all contents are written.
withSinkFileCautious
  :: MonadUnliftIO m
  => FilePath
  -> (ConduitM ByteString o m () -> m a)
  -> m a
withSinkFileCautious fp inner =
    withRunInIO $ \run -> bracket acquire cleanup $ \(tmpFP, h) ->
      run (inner $ sinkHandle h) <* (IO.hClose h *> Dir.renameFile tmpFP fp)
  where
    acquire = IO.openBinaryTempFile (FP.takeDirectory fp) (FP.takeFileName fp FP.<.> "tmp")
    cleanup (tmpFP, h) = do
        IO.hClose h
        Dir.removeFile tmpFP `catch` \e ->
            if isDoesNotExistError e
                then return ()
                else throwIO e

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
  :: forall e o env. (HasProcessContext env, HasLogFunc env)
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
    :: (HasProcessContext env, HasLogFunc env)
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
readProcessNull :: (HasProcessContext env, HasLogFunc env)
                => String -- ^ Command
                -> [String] -- ^ Command line arguments
                -> RIO env ()
readProcessNull name args =
  -- We want the output to appear in any exceptions, so we capture and drop it
  void $ proc name args readProcessStdout_

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
