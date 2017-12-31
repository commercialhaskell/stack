{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
module Stack.Prelude
  ( withSourceFile
  , withSinkFile
  , withSinkFileCautious
  , withSystemTempDir
  , module X
  ) where

import RIO as X
import qualified Data.Text            as T
import           Path                 as X (Abs, Dir, File, Path, Rel,
                                            toFilePath)
import qualified Path.IO

import           Data.Conduit.Binary (sourceHandle, sinkHandle)
import qualified Data.ByteString.Lazy as BL

import qualified System.IO as IO
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import           System.IO.Error (isDoesNotExistError)

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
