{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Reading from external processes.

module System.Process.Read
  (callProcess
  ,readProcessStdout
  ,tryProcessStdout
  ,sinkProcessStdout
  ,runIn
  ,EnvOverride(..)
  ,envHelper)
  where

import           Control.Applicative ((*>))
import           Control.Arrow ((***))
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Exception
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (MonadLogger, logError)
import qualified Data.ByteString as S
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process hiding (callProcess)
import           Data.Foldable (forM_)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Path (Path, Abs, Dir, File, toFilePath)
import           System.Directory (createDirectoryIfMissing)
import           System.Exit (ExitCode(ExitSuccess), exitWith)

-- | Override the environment received by a child process
newtype EnvOverride = EnvOverride { unEnvOverride :: Map Text Text }
  deriving Monoid

-- | Helper conversion function
envHelper :: EnvOverride -> Maybe [(String, String)]
envHelper = Just . map (T.unpack *** T.unpack) . Map.toList . unEnvOverride

-- | Try to produce a strict 'S.ByteString' from the stdout of a
-- process.
tryProcessStdout :: (MonadIO m)
                 => EnvOverride
                 -> String
                 -> [String]
                 -> m (Either ProcessExitedUnsuccessfully S.ByteString)
tryProcessStdout menv name args = do
  liftIO (try (readProcessStdout menv name args))

-- | Produce a strict 'S.ByteString' from the stdout of a
-- process. Throws a 'ProcessExitedUnsuccessfully' exception if the
-- process fails.
readProcessStdout :: (MonadIO m)
                  => EnvOverride
                  -> String
                  -> [String]
                  -> m S.ByteString
readProcessStdout menv name args =
  sinkProcessStdout menv name args CL.consume >>=
  liftIO . evaluate . S.concat

-- | Same as @System.Process.callProcess@, but takes an environment override
callProcess :: (MonadIO m)
            => EnvOverride
            -> String
            -> [String]
            -> m ()
callProcess menv name args = sinkProcessStdout menv name args CL.sinkNull

-- | Consume the stdout of a process feeding strict 'S.ByteString's to a consumer.
sinkProcessStdout :: (MonadIO m)
                  => EnvOverride
                  -> String
                  -> [String]
                  -> Consumer S.ByteString IO a
                  -> m a
sinkProcessStdout menv name args sink = do
  liftIO (withCheckedProcess
            (proc name args) { env = envHelper menv }
            (\ClosedStream out err ->
               runConcurrently $
               Concurrently (asBSSource err $$ CL.sinkNull) *>
               Concurrently (asBSSource out $$ sink)))
  where asBSSource :: Source m S.ByteString -> Source m S.ByteString
        asBSSource = id

-- | Run the given command in the given directory. If it exits with anything
-- but success, throw an exception.
runIn :: forall (m :: * -> *).
         (MonadLogger m,MonadIO m)
      => Path Abs Dir -- ^ directory to run in
      -> Path Abs File -- ^ command to run
      -> EnvOverride
      -> [String] -- ^ command line arguments
      -> Maybe String -- ^ error message on failure, if Nothing uses a default
      -> m ()
runIn dir cmd menv args errMsg =
  do let dir' = toFilePath dir
         cmd' = toFilePath cmd
     liftIO (createDirectoryIfMissing True dir')
     (Nothing,Nothing,Nothing,ph) <-
       liftIO (createProcess
                 (proc cmd' args) {cwd = Just dir'
                                  ,env = envHelper menv
                                  })
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
