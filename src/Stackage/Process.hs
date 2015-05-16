{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Reading from external processes.

module Stackage.Process
  (readProcessStdout
  ,tryProcessStdout
  ,sinkProcessStdout
  ,runIn)
  where

import           Conduit
import           Control.Applicative ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Exception
import           Control.Monad (when)
import           Control.Monad.Logger (MonadLogger, logError)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Conduit.Process
import           Data.Foldable (forM_)
import qualified Data.Text as T
import           Path (Path, Abs, Dir, File, toFilePath)
import           System.Directory (createDirectoryIfMissing)
import           System.Exit (ExitCode(ExitSuccess), exitWith)

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

runIn :: forall (m :: * -> *).
         (MonadLogger m,MonadIO m)
      => Path Abs Dir -> Path Abs File -> [String] -> Maybe String -> m ()
runIn dir cmd args errMsg =
  do let dir' = toFilePath dir
         cmd' = toFilePath cmd
     liftIO (createDirectoryIfMissing True dir')
     (Nothing,Nothing,Nothing,ph) <-
       liftIO (createProcess
                 (proc cmd' args) {cwd =
                                     Just dir'})
     ec <- liftIO (waitForProcess ph)
     when (ec /= ExitSuccess)
          (do $logError (T.pack (concat ["Exit code "
                                        ,show ec
                                        ," while running "
                                        ,show (cmd' : args)
                                        ," in "
                                        ,dir']))
              forM_ errMsg
                    (\e -> ($logError (T.pack e)))
              liftIO (exitWith ec))
