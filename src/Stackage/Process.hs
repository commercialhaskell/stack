-- | Reading from external processes.

module Stackage.Process
  (lazyProcessStdout
  ,sinkProcessStdout)
  where

import           Conduit
import           Control.Applicative ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Conduit.Process
import           Data.Void

-- | Produce a lazy 'L.ByteString' from the stdout of a process.
lazyProcessStdout :: MonadIO m => String
                  -> [String]
                  -> m L.ByteString
lazyProcessStdout name args =
  sinkProcessStdout name args sinkLazy

-- | Read a lazy 'ByteString' of the stdout from running
-- a process.
sinkProcessStdout :: MonadIO m
                  => String
                  -> [String]
                  -> ConduitM S.ByteString Void IO a
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
