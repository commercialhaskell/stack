{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading from external processes.

module System.Process.Read
  (callProcess
  ,readProcessStdout
  ,tryProcessStdout
  ,sinkProcessStdout
  ,runIn
  ,EnvOverride
  ,unEnvOverride
  ,mkEnvOverride
  ,envHelper
  ,doesExecutableExist
  ,findExecutable
  ,getEnvOverride)
  where

import           Control.Applicative
import           Control.Arrow ((***), first)
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Exception
import           Control.Monad (when, join, liftM)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (MonadLogger, logError)
import qualified Data.ByteString as S
import           Data.Conduit
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
import           System.Directory (createDirectoryIfMissing, doesFileExist, canonicalizePath)
import qualified System.FilePath as FP
import           System.Environment (getEnvironment)
import           System.Exit (ExitCode(ExitSuccess), exitWith)

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
                  -> Sink S.ByteString IO a
                  -> m a
sinkProcessStdout menv name args sink = do
  name' <- liftIO $ liftM toFilePath $ join $ findExecutable menv name
  liftIO (withCheckedProcess
            (proc name' args) { env = envHelper menv }
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
      -> FilePath -- ^ command to run
      -> EnvOverride
      -> [String] -- ^ command line arguments
      -> Maybe String -- ^ error message on failure, if Nothing uses a default
      -> m ()
runIn dir cmd menv args errMsg =
  do let dir' = toFilePath dir
     liftIO (createDirectoryIfMissing True dir')
     (Nothing,Nothing,Nothing,ph) <-
       liftIO (createProcess
                 (proc cmd args) {cwd = Just dir'
                                 ,env = envHelper menv
                                 })
     ec <- liftIO (waitForProcess ph)
     when (ec /= ExitSuccess)
          (do $logError (T.pack (concat ["Exit code "
                                        ,show ec
                                        ," while running "
                                        ,show (cmd : args)
                                        ," in "
                                        ,dir']))
              forM_ errMsg
                    (\e -> ($logError (T.pack e)))
              liftIO (exitWith ec))

-- | Check if the given executable exists on the given PATH
doesExecutableExist :: MonadIO m => EnvOverride -> String -> m Bool
doesExecutableExist menv name = liftM isJust $ findExecutable menv name

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
                            fp' <- canonicalizePath fp >>= parseAbsFile
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
