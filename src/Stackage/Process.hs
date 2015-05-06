{-# LANGUAGE RankNTypes #-}

-- | Reading from external processes.

module Stackage.Process
  (readProcessStdout
  ,tryProcessStdout
  ,sinkProcessStdout)
  where

import           Conduit
import           Control.Applicative ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Exception
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Conduit.Process

-- | Try to produce a strict 'S.ByteString' from the stdout of a
-- process.
tryProcessStdout :: MonadIO m
                 => String
                 -> [String]
                 -> m (Either ProcessExitedUnsuccessfully S.ByteString)
tryProcessStdout name args =
  liftIO (try (readProcessStdout name args))

-- | Produce a strict 'S.ByteString' from the stdout of a
-- process. Throws a 'ProcessExitedUnsuccessfully' exception if the
-- process fails.
readProcessStdout :: MonadIO m => String
                    -> [String]
                    -> m S.ByteString
readProcessStdout name args =
  sinkProcessStdout name args sinkLazy >>=
  liftIO . evaluate . L.toStrict

-- | Consume the stdout of a process feeding strict 'S.ByteString's to a consumer.
sinkProcessStdout :: MonadIO m
                  => String
                  -> [String]
                  -> Consumer S.ByteString IO a
                  -> m a
sinkProcessStdout name args sink =
  liftIO (withCheckedProcess
            (proc name args)
            (\ClosedStream out err ->
               runConcurrently $
               Concurrently (asBSSource err $$ sinkNull) *>
               Concurrently (asBSSource out $$ sink)))
  where asBSSource :: Source m S.ByteString -> Source m S.ByteString
        asBSSource = id
