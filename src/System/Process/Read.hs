{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading from external processes.

module System.Process.Read
  (readProcessStdoutLogStderr
  ,readProcessStdout
  ,tryProcessStdout
  ,sinkProcessStdoutLogStderr
  ,sinkProcessStdout
  ,sinkProcessStderrStdout
  ,EnvOverride
  ,unEnvOverride
  ,mkEnvOverride
  ,envHelper
  ,doesExecutableExist
  ,findExecutable
  ,getEnvOverride
  ,envSearchPath
  ,preProcess
  ,readProcessNull
  ,readInNull)
  where

import           Control.Applicative
import           Control.Arrow ((***), first)
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Exception
import           Control.Monad (join, liftM, void)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (MonadLogger, logError)
import           Control.Monad.Trans.Control (MonadBaseControl,liftBaseWith)
import qualified Data.ByteString as S
import           Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process hiding (callProcess)
import           Data.Foldable (forM_)
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Distribution.System (OS (Windows), Platform (Platform))
import           Path (Path, Abs, Dir, toFilePath, File, parseAbsFile)
import           Prelude -- Fix AMP warning
import           System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import           System.Environment (getEnvironment)
import           System.Exit
import qualified System.FilePath as FP

-- | Override the environment received by a child process
data EnvOverride = EnvOverride
    { eoTextMap :: Map Text Text
    , eoStringList :: [(String, String)]
    , eoPath :: [FilePath]
    , eoExeCache :: IORef (Map FilePath (Either FindExecutableException (Path Abs File)))
    , eoExeExtension :: String
    }

-- | Get the environment variables from @EnvOverride@
unEnvOverride :: EnvOverride -> Map Text Text
unEnvOverride = eoTextMap

-- | Get the list of directories searched
envSearchPath :: EnvOverride -> [FilePath]
envSearchPath = eoPath

-- | Create a new @EnvOverride@
mkEnvOverride :: MonadIO m
              => Platform
              -> Map Text Text
              -> m EnvOverride
mkEnvOverride platform tm' = do
    ref <- liftIO $ newIORef Map.empty
    return EnvOverride
        { eoTextMap = tm
        , eoStringList = map (T.unpack *** T.unpack) $ Map.toList tm
        , eoPath = maybe [] (FP.splitSearchPath . T.unpack) (Map.lookup "PATH" tm)
        , eoExeCache = ref
        , eoExeExtension = if isWindows then ".exe" else ""
        }
  where
    -- Fix case insensitivity of the PATH environment variable on Windows.
    tm
        | isWindows = Map.fromList $ map (first T.toUpper) $ Map.toList tm'
        | otherwise = tm'

    -- Don't use CPP so that the Windows code path is at least type checked
    -- regularly
    isWindows =
        case platform of
            Platform _ Windows -> True
            _ -> False

-- | Helper conversion function
envHelper :: EnvOverride -> Maybe [(String, String)]
envHelper = Just . eoStringList

-- | Read from the process, ignoring any output.
readProcessNull
    :: (MonadIO m)
    => Maybe (Path Abs Dir)
    -> EnvOverride
    -> String
    -> [String]
    -> m ()
readProcessNull wd menv name args =
    sinkProcessStdout wd menv name args CL.sinkNull

-- | Run the given command in the given directory. If it exits with anything
-- but success, prints an error and then calls 'exitWith' to exit the program.
readInNull :: forall (m :: * -> *).
               (MonadLogger m,MonadIO m)
            => Path Abs Dir -- ^ directory to run in
            -> FilePath -- ^ command to run
            -> EnvOverride
            -> [String] -- ^ command line arguments
            -> Maybe Text
            -> m ()
readInNull wd cmd menv args errMsg = do
    result <- liftIO (try (readProcessNull (Just wd) menv cmd args))
    case result of
        Left (ProcessExitedUnsuccessfully _ ec) -> do
            $logError $
                T.pack $
                concat
                    [ "Exit code "
                    , show ec
                    , " while running "
                    , show (cmd : args)
                    , " in "
                    , toFilePath wd]
            forM_ errMsg $logError
            liftIO (exitWith ec)
        Right () -> return ()

-- | Try to produce a strict 'S.ByteString' from the stdout of a
-- process.
tryProcessStdout :: (MonadIO m)
                 => Maybe (Path Abs Dir)
                 -> EnvOverride
                 -> String
                 -> [String]
                 -> m (Either ProcessExitedUnsuccessfully S.ByteString)
tryProcessStdout wd menv name args = do
  liftIO (try (readProcessStdout wd menv name args))

-- | Produce a strict 'S.ByteString' from the stdout of a
-- process. Throws a 'ProcessExitedUnsuccessfully' exception if the
-- process fails.
readProcessStdout :: (MonadIO m)
                  => Maybe (Path Abs Dir)
                  -> EnvOverride
                  -> String
                  -> [String]
                  -> m S.ByteString
readProcessStdout wd menv name args =
  sinkProcessStdout wd menv name args CL.consume >>=
  liftIO . evaluate . S.concat

-- | Produce a strict 'S.ByteString' from the stdout of a
-- process. Throws a 'ProcessExitedUnsuccessfully' exception if the
-- process fails.  Logs process's stderr using @$logError@.
readProcessStdoutLogStderr :: (MonadIO m,MonadLogger m,MonadBaseControl IO m)
                           => Text
                           -> Maybe (Path Abs Dir)
                           -> EnvOverride
                           -> String
                           -> [String]
                           -> m S.ByteString
readProcessStdoutLogStderr stderrPrefix wd menv name args = do
  stdout <- sinkProcessStdoutLogStderr stderrPrefix wd menv name args CL.consume
  liftIO (evaluate (S.concat stdout))

-- | Consume the stdout of a process feeding strict 'S.ByteString's to a consumer.
-- Logs process's stderr using @$logError@.
sinkProcessStdoutLogStderr :: (MonadIO m,MonadLogger m,MonadBaseControl IO m)
                           => Text -- ^ Prefix for any logged stderr message
                           -> Maybe (Path Abs Dir)
                           -> EnvOverride
                           -> String
                           -> [String]
                           -> Sink S.ByteString IO a -- ^ Sink for stdout
                           -> m a
sinkProcessStdoutLogStderr stderrPrefix wd menv name args sinkStdout = do
  runInBase <- liftBaseWith $ \run -> return (void . run)
  let logSink = CC.mapM_ (liftIO . runInBase . $logError . T.append stderrPrefix)
      sinkStderr = CC.decodeUtf8 =$= CC.line logSink
  (_,stdout) <- sinkProcessStderrStdout wd menv name args sinkStderr sinkStdout
  return stdout

-- | Consume the stdout of a process feeding strict 'S.ByteString's to a consumer.
sinkProcessStdout :: (MonadIO m)
                  => Maybe (Path Abs Dir)
                  -> EnvOverride
                  -> String
                  -> [String]
                  -> Sink S.ByteString IO a
                  -> m a
sinkProcessStdout wd menv name args sink = do
  (_,stdout) <- sinkProcessStderrStdout wd menv name args CL.sinkNull sink
  return stdout

-- | Consume the stdout and stderr of a process feeding strict 'S.ByteString's to the consumers.
sinkProcessStderrStdout :: (MonadIO m)
                        => Maybe (Path Abs Dir)
                        -> EnvOverride
                        -> String
                        -> [String]
                        -> Sink S.ByteString IO e -- ^ Sink for stderr
                        -> Sink S.ByteString IO o -- ^ Sink for stdout
                        -> m (e,o)
sinkProcessStderrStdout wd menv name args sinkStderr sinkStdout = do
  name' <- preProcess wd menv name
  liftIO (withCheckedProcess
            (proc name' args) { env = envHelper menv, cwd = fmap toFilePath wd }
            (\ClosedStream out err ->
               runConcurrently $
               (,) <$>
               Concurrently (asBSSource err $$ sinkStderr) <*>
               Concurrently (asBSSource out $$ sinkStdout)))
  where asBSSource :: Source m S.ByteString -> Source m S.ByteString
        asBSSource = id


-- | Perform pre-call-process tasks.  Ensure the working directory exists and find the
-- executable path.
preProcess :: (MonadIO m) => Maybe (Path Abs Dir) -> EnvOverride -> String -> m FilePath
preProcess wd menv name = do
  name' <- liftIO $ liftM toFilePath $ join $ findExecutable menv name
  liftIO (maybe (return ()) (createDirectoryIfMissing True . toFilePath) wd)
  return name'

-- | Check if the given executable exists on the given PATH
doesExecutableExist :: MonadIO m => EnvOverride -> String -> m Bool
doesExecutableExist menv name = liftM isJust $ findExecutable menv name

-- | Turn a relative path into an absolute path.
--
--   Note: this function duplicates the functionality of makeAbsolute
--   in recent versions of "System.Directory", and can be removed once
--   we no longer need to support older versions of GHC.
makeAbsolute :: FilePath -> IO FilePath
makeAbsolute = fmap FP.normalise . absolutize
  where absolutize path
          | FP.isRelative path = fmap (FP.</> path) getCurrentDirectory
          | otherwise          = return path

-- | Find the complete path for the executable
findExecutable :: (MonadIO m, MonadThrow n) => EnvOverride -> String -> m (n (Path Abs File))
findExecutable eo name = liftIO $ do
    m <- readIORef $ eoExeCache eo
    epath <- case Map.lookup name m of
        Just epath -> return epath
        Nothing -> do
            let loop [] = return $ Left $ ExecutableNotFound name (eoPath eo)
                loop (dir:dirs) = do
                    let fp = dir FP.</> name ++ eoExeExtension eo
                    exists <- doesFileExist fp
                    if exists
                        then do
                            fp' <- makeAbsolute fp >>= parseAbsFile
                            return $ return fp'
                        else loop dirs
            epath <- loop $ eoPath eo
            !() <- atomicModifyIORef (eoExeCache eo) $ \m' ->
                (Map.insert name epath m', ())
            return epath
    return $ either throwM return epath

-- | Load up an EnvOverride from the standard environment
getEnvOverride :: MonadIO m => Platform -> m EnvOverride
getEnvOverride platform =
    liftIO $
    getEnvironment >>=
          mkEnvOverride platform
        . Map.fromList . map (T.pack *** T.pack)

data FindExecutableException
    = NoPathFound
    | ExecutableNotFound String [FilePath]
    deriving Typeable
instance Exception FindExecutableException
instance Show FindExecutableException where
    show NoPathFound = "PATH not found in EnvOverride"
    show (ExecutableNotFound name path) = concat
        [ "Executable named "
        , name
        , " not found on path: "
        , show path
        ]
