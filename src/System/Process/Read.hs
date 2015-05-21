{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Reading from external processes.

module System.Process.Read
  (HasExternalEnv(..)
  ,callProcess
  ,readProcessStdout
  ,tryProcessStdout
  ,sinkProcessStdout
  ,runIn)
  where

import           Control.Applicative ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Exception
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (MonadLogger, logError)
import           Control.Monad.Reader (MonadReader, asks, runReaderT)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process hiding (callProcess)
import           Data.Foldable (forM_)
import qualified Data.Text as T
import           Path (Path, Abs, Dir, File, toFilePath)
import           System.Directory (createDirectoryIfMissing)
import           System.Exit (ExitCode(ExitSuccess), exitWith)

-- | Reader values which have optional environment variable overrides
class HasExternalEnv env where
    getExternalEnv :: env -> Maybe [(String, String)]

newtype EnvHelper = EnvHelper (Maybe [(String, String)])
instance HasExternalEnv EnvHelper where
    getExternalEnv (EnvHelper x) = x

-- | Try to produce a strict 'S.ByteString' from the stdout of a
-- process.
tryProcessStdout :: (MonadIO m, MonadReader env m, HasExternalEnv env)
                 => String
                 -> [String]
                 -> m (Either ProcessExitedUnsuccessfully S.ByteString)
tryProcessStdout name args = do
  menv <- asks (EnvHelper . getExternalEnv)
  liftIO (try (runReaderT (readProcessStdout name args) menv))

-- | Produce a strict 'S.ByteString' from the stdout of a
-- process. Throws a 'ProcessExitedUnsuccessfully' exception if the
-- process fails.
readProcessStdout :: (MonadIO m, MonadReader env m, HasExternalEnv env)
                  => String
                  -> [String]
                  -> m S.ByteString
readProcessStdout name args =
  sinkProcessStdout name args CL.consume >>=
  liftIO . evaluate . S.concat

-- | Same as @System.Process.callProcess@, but respect @HasExternalEnv@
callProcess :: (MonadIO m, MonadReader env m, HasExternalEnv env)
            => String
            -> [String]
            -> m ()
callProcess name args = sinkProcessStdout name args CL.sinkNull

-- | Consume the stdout of a process feeding strict 'S.ByteString's to a consumer.
sinkProcessStdout :: (MonadIO m, MonadReader env m, HasExternalEnv env)
                  => String
                  -> [String]
                  -> Consumer S.ByteString IO a
                  -> m a
sinkProcessStdout name args sink = do
  env' <- asks getExternalEnv
  liftIO (withCheckedProcess
            (proc name args) { env = env' }
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
      -> [String] -- ^ command line arguments
      -> Maybe String -- ^ error message on failure, if Nothing uses a default
      -> m ()
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
