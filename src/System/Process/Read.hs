{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading from external processes.

module System.Process.Read
  (readProcessStdout
  ,readProcessStderrStdout
  ,tryProcessStdout
  ,tryProcessStderrStdout
  ,sinkProcessStdout
  ,sinkProcessStderrStdout
  ,sinkProcessStderrStdoutHandle
  ,logProcessStderrStdout
  ,readProcess
  ,EnvOverride(..)
  ,unEnvOverride
  ,mkEnvOverride
  ,modifyEnvOverride
  ,envHelper
  ,doesExecutableExist
  ,findExecutable
  ,getEnvOverride
  ,envSearchPath
  ,preProcess
  ,readProcessNull
  ,ReadProcessException (..)
  ,augmentPath
  ,augmentPathMap
  ,resetExeCache
  )
  where

import           Control.Applicative
import           Control.Arrow ((***), first)
import           Control.Concurrent.Async (concurrently)
import           Control.Exception hiding (try, catch)
import           Control.Monad (join, liftM, unless, void)
import           Control.Monad.Catch (MonadThrow, MonadCatch, throwM, try, catch)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith)
import qualified Data.ByteString as S
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process hiding (callProcess)
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (isJust, maybeToList, fromMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Data.Typeable (Typeable)
import           Distribution.System (OS (Windows), Platform (Platform))
import           Language.Haskell.TH as TH (location)
import           Path
import           Path.Extra
import           Path.IO hiding (findExecutable)
import           Prelude -- Fix AMP warning
import qualified System.Directory as D
import           System.Environment (getEnvironment)
import           System.Exit
import qualified System.FilePath as FP
import           System.IO (Handle, hClose)
import           System.Process.Log
import           Prelude () -- Hide post-AMP warnings

-- | Override the environment received by a child process.
data EnvOverride = EnvOverride
    { eoTextMap :: Map Text Text -- ^ Environment variables as map
    , eoStringList :: [(String, String)] -- ^ Environment variables as association list
    , eoPath :: [FilePath] -- ^ List of directories searched for executables (@PATH@)
    , eoExeCache :: IORef (Map FilePath (Either ReadProcessException (Path Abs File)))
    , eoExeExtensions :: [String] -- ^ @[""]@ on non-Windows systems, @["", ".exe", ".bat"]@ on Windows
    , eoPlatform :: Platform
    }

-- | Get the environment variables from an 'EnvOverride'.
unEnvOverride :: EnvOverride -> Map Text Text
unEnvOverride = eoTextMap

-- | Get the list of directories searched (@PATH@).
envSearchPath :: EnvOverride -> [FilePath]
envSearchPath = eoPath

-- | Modify the environment variables of an 'EnvOverride'.
modifyEnvOverride :: MonadIO m
                  => EnvOverride
                  -> (Map Text Text -> Map Text Text)
                  -> m EnvOverride
modifyEnvOverride eo f = mkEnvOverride
    (eoPlatform eo)
    (f $ eoTextMap eo)

-- | Create a new 'EnvOverride'.
mkEnvOverride :: MonadIO m
              => Platform
              -> Map Text Text
              -> m EnvOverride
mkEnvOverride platform tm' = do
    ref <- liftIO $ newIORef Map.empty
    return EnvOverride
        { eoTextMap = tm
        , eoStringList = map (T.unpack *** T.unpack) $ Map.toList tm
        , eoPath =
             (if isWindows then (".":) else id)
             (maybe [] (FP.splitSearchPath . T.unpack) (Map.lookup "PATH" tm))
        , eoExeCache = ref
        , eoExeExtensions =
            if isWindows
                then let pathext = fromMaybe
                           ".COM;.EXE;.BAT;.CMD;.VBS;.VBE;.JS;.JSE;.WSF;.WSH;.MSC"
                           (Map.lookup "PATHEXT" tm)
                      in map T.unpack $ "" : T.splitOn ";" pathext
                else [""]
        , eoPlatform = platform
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

-- | Helper conversion function.
envHelper :: EnvOverride -> Maybe [(String, String)]
envHelper = Just . eoStringList

-- | Read from the process, ignoring any output.
--
-- Throws a 'ReadProcessException' exception if the process fails.
readProcessNull :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
                => Maybe (Path Abs Dir) -- ^ Optional working directory
                -> EnvOverride
                -> String -- ^ Command
                -> [String] -- ^ Command line arguments
                -> m ()
readProcessNull wd menv name args =
    sinkProcessStdout wd menv name args CL.sinkNull

-- | Try to produce a strict 'S.ByteString' from the stdout of a
-- process.
tryProcessStdout :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
                 => Maybe (Path Abs Dir) -- ^ Optional directory to run in
                 -> EnvOverride
                 -> String -- ^ Command
                 -> [String] -- ^ Command line arguments
                 -> m (Either ReadProcessException S.ByteString)
tryProcessStdout wd menv name args =
    try (readProcessStdout wd menv name args)

-- | Try to produce strict 'S.ByteString's from the stderr and stdout of a
-- process.
tryProcessStderrStdout :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
                       => Maybe (Path Abs Dir) -- ^ Optional directory to run in
                       -> EnvOverride
                       -> String -- ^ Command
                       -> [String] -- ^ Command line arguments
                       -> m (Either ReadProcessException (S.ByteString, S.ByteString))
tryProcessStderrStdout wd menv name args =
    try (readProcessStderrStdout wd menv name args)

-- | Produce a strict 'S.ByteString' from the stdout of a process.
--
-- Throws a 'ReadProcessException' exception if the process fails.
readProcessStdout :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
                  => Maybe (Path Abs Dir) -- ^ Optional directory to run in
                  -> EnvOverride
                  -> String -- ^ Command
                  -> [String] -- ^ Command line arguments
                  -> m S.ByteString
readProcessStdout wd menv name args =
  sinkProcessStdout wd menv name args CL.consume >>=
  liftIO . evaluate . S.concat

-- | Produce strict 'S.ByteString's from the stderr and stdout of a process.
--
-- Throws a 'ReadProcessException' exception if the process fails.
readProcessStderrStdout :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
                        => Maybe (Path Abs Dir) -- ^ Optional directory to run in
                        -> EnvOverride
                        -> String -- ^ Command
                        -> [String] -- ^ Command line arguments
                        -> m (S.ByteString, S.ByteString)
readProcessStderrStdout wd menv name args = do
  (e, o) <- sinkProcessStderrStdout wd menv name args CL.consume CL.consume
  liftIO $ (,) <$> evaluate (S.concat e) <*> evaluate (S.concat o)

-- | An exception while trying to read from process.
data ReadProcessException
    = ProcessFailed CreateProcess ExitCode L.ByteString L.ByteString
    -- ^ @'ProcessFailed' createProcess exitCode stdout stderr@
    | NoPathFound
    | ExecutableNotFound String [FilePath]
    | ExecutableNotFoundAt FilePath
    deriving Typeable
instance Show ReadProcessException where
    show (ProcessFailed cp ec out err) = concat $
        [ "Running "
        , showSpec $ cmdspec cp] ++
        maybe [] (\x -> [" in directory ", x]) (cwd cp) ++
        [ " exited with "
        , show ec
        , "\n\n"
        , toStr out
        , "\n"
        , toStr err
        ]
      where
        toStr = LT.unpack . LT.decodeUtf8With lenientDecode

        showSpec (ShellCommand str) = str
        showSpec (RawCommand cmd args) =
            unwords $ cmd : map (T.unpack . showProcessArgDebug) args
    show NoPathFound = "PATH not found in EnvOverride"
    show (ExecutableNotFound name path) = concat
        [ "Executable named "
        , name
        , " not found on path: "
        , show path
        ]
    show (ExecutableNotFoundAt name) =
        "Did not find executable at specified path: " ++ name
instance Exception ReadProcessException

-- | Consume the stdout of a process feeding strict 'S.ByteString's to a consumer.
-- If the process fails, spits out stdout and stderr as error log
-- level. Should not be used for long-running processes or ones with
-- lots of output; for that use 'sinkProcessStdoutLogStderr'.
--
-- Throws a 'ReadProcessException' if unsuccessful.
sinkProcessStdout
    :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m)
    => Maybe (Path Abs Dir) -- ^ Optional directory to run in
    -> EnvOverride
    -> String -- ^ Command
    -> [String] -- ^ Command line arguments
    -> Sink S.ByteString IO a -- ^ Sink for stdout
    -> m a
sinkProcessStdout wd menv name args sinkStdout = do
  stderrBuffer <- liftIO (newIORef mempty)
  stdoutBuffer <- liftIO (newIORef mempty)
  (_,sinkRet) <-
      catch
          (sinkProcessStderrStdout
               wd
               menv
               name
               args
               (CL.mapM_ (\bytes -> liftIO (modifyIORef' stderrBuffer (<> byteString bytes))))
               (CL.iterM (\bytes -> liftIO (modifyIORef' stdoutBuffer (<> byteString bytes))) $=
                sinkStdout))
          (\(ProcessExitedUnsuccessfully cp ec) ->
               do stderrBuilder <- liftIO (readIORef stderrBuffer)
                  stdoutBuilder <- liftIO (readIORef stdoutBuffer)
                  throwM $ ProcessFailed
                    cp
                    ec
                    (toLazyByteString stdoutBuilder)
                    (toLazyByteString stderrBuilder))
  return sinkRet

logProcessStderrStdout
    :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
    => Maybe (Path Abs Dir)
    -> String
    -> EnvOverride
    -> [String]
    -> m ()
logProcessStderrStdout mdir name menv args = liftBaseWith $ \restore -> do
    let logLines = CB.lines =$ CL.mapM_ (void . restore . monadLoggerLog $(TH.location >>= liftLoc) "" LevelInfo . toLogStr)
    void $ restore $ sinkProcessStderrStdout mdir menv name args logLines logLines

-- | Consume the stdout and stderr of a process feeding strict 'S.ByteString's to the consumers.
--
-- Throws a 'ReadProcessException' if unsuccessful in launching, or 'ProcessExitedUnsuccessfully' if the process itself fails.
sinkProcessStderrStdout :: forall m e o. (MonadIO m, MonadLogger m)
                        => Maybe (Path Abs Dir) -- ^ Optional directory to run in
                        -> EnvOverride
                        -> String -- ^ Command
                        -> [String] -- ^ Command line arguments
                        -> Sink S.ByteString IO e -- ^ Sink for stderr
                        -> Sink S.ByteString IO o -- ^ Sink for stdout
                        -> m (e,o)
sinkProcessStderrStdout wd menv name args sinkStderr sinkStdout = do
  name' <- preProcess wd menv name
  $withProcessTimeLog name' args $
      liftIO $ withCheckedProcess
          (proc name' args) { env = envHelper menv, cwd = fmap toFilePath wd }
          (\ClosedStream out err -> f err out)
  where

    -- There is a bug in streaming-commons or conduit-extra which
    -- leads to a file descriptor leak. Ideally, we should be able to
    -- simply use the following code. Instead, we're using the code
    -- below it, which is explicit in closing Handles. When the
    -- upstream bug is fixed, we can consider moving back to the
    -- simpler code, though there's really no downside to the more
    -- complex version used here.
    --
    -- f :: Source IO S.ByteString -> Source IO S.ByteString -> IO (e, o)
    -- f err out = (err $$ sinkStderr) `concurrently` (out $$ sinkStdout)

    f :: Handle -> Handle -> IO (e, o)
    f err out = ((CB.sourceHandle err $$ sinkStderr) `concurrently` (CB.sourceHandle out $$ sinkStdout))
        `finally` hClose err `finally` hClose out

-- | Like sinkProcessStderrStdout, but receives Handles for stderr and stdout instead of 'Sink's.
--
-- Throws a 'ReadProcessException' if unsuccessful in launching, or 'ProcessExitedUnsuccessfully' if the process itself fails.
sinkProcessStderrStdoutHandle :: (MonadIO m, MonadLogger m)
                              => Maybe (Path Abs Dir) -- ^ Optional directory to run in
                              -> EnvOverride
                              -> String -- ^ Command
                              -> [String] -- ^ Command line arguments
                              -> Handle
                              -> Handle
                              -> m ()
sinkProcessStderrStdoutHandle wd menv name args err out = do
  name' <- preProcess wd menv name
  $withProcessTimeLog name' args $
      liftIO $ withCheckedProcess
          (proc name' args)
              { env = envHelper menv
              , cwd = fmap toFilePath wd
              , std_err = UseHandle err
              , std_out = UseHandle out
              }
          (\ClosedStream UseProvidedHandle UseProvidedHandle -> return ())

-- | Perform pre-call-process tasks.  Ensure the working directory exists and find the
-- executable path.
--
-- Throws a 'ReadProcessException' if unsuccessful.
preProcess :: (MonadIO m)
  => Maybe (Path Abs Dir) -- ^ Optional directory to create if necessary
  -> EnvOverride       -- ^ How to override environment
  -> String            -- ^ Command name
  -> m FilePath
preProcess wd menv name = do
  name' <- liftIO $ liftM toFilePath $ join $ findExecutable menv name
  maybe (return ()) ensureDir wd
  return name'

-- | Check if the given executable exists on the given PATH.
doesExecutableExist :: (MonadIO m)
  => EnvOverride       -- ^ How to override environment
  -> String            -- ^ Name of executable
  -> m Bool
doesExecutableExist menv name = liftM isJust $ findExecutable menv name

-- | Find the complete path for the executable.
--
-- Throws a 'ReadProcessException' if unsuccessful.
findExecutable :: (MonadIO m, MonadThrow n)
  => EnvOverride       -- ^ How to override environment
  -> String            -- ^ Name of executable
  -> m (n (Path Abs File)) -- ^ Full path to that executable on success
findExecutable eo name0 | any FP.isPathSeparator name0 = do
    let names0 = map (name0 ++) (eoExeExtensions eo)
        testNames [] = return $ throwM $ ExecutableNotFoundAt name0
        testNames (name:names) = do
            exists <- liftIO $ D.doesFileExist name
            if exists
                then do
                    path <- liftIO $ resolveFile' name
                    return $ return path
                else testNames names
    testNames names0
findExecutable eo name = liftIO $ do
    m <- readIORef $ eoExeCache eo
    epath <- case Map.lookup name m of
        Just epath -> return epath
        Nothing -> do
            let loop [] = return $ Left $ ExecutableNotFound name (eoPath eo)
                loop (dir:dirs) = do
                    let fp0 = dir FP.</> name
                        fps0 = map (fp0 ++) (eoExeExtensions eo)
                        testFPs [] = loop dirs
                        testFPs (fp:fps) = do
                            exists <- D.doesFileExist fp
                            existsExec <- if exists then liftM D.executable $ D.getPermissions fp else return False
                            if existsExec
                                then do
                                    fp' <- D.makeAbsolute fp >>= parseAbsFile
                                    return $ return fp'
                                else testFPs fps
                    testFPs fps0
            epath <- loop $ eoPath eo
            () <- atomicModifyIORef (eoExeCache eo) $ \m' ->
                (Map.insert name epath m', ())
            return epath
    return $ either throwM return epath

-- | Reset the executable cache.
resetExeCache :: MonadIO m => EnvOverride -> m ()
resetExeCache eo = liftIO (atomicModifyIORef (eoExeCache eo) (const mempty))

-- | Load up an 'EnvOverride' from the standard environment.
getEnvOverride :: MonadIO m => Platform -> m EnvOverride
getEnvOverride platform =
    liftIO $
    getEnvironment >>=
          mkEnvOverride platform
        . Map.fromList . map (T.pack *** T.pack)

data PathException = PathsInvalidInPath [FilePath]
    deriving Typeable

instance Exception PathException
instance Show PathException where
    show (PathsInvalidInPath paths) = unlines $
        [ "Would need to add some paths to the PATH environment variable \
          \to continue, but they would be invalid because they contain a "
          ++ show FP.searchPathSeparator ++ "."
        , "Please fix the following paths and try again:"
        ] ++ paths

-- | Augment the PATH environment variable with the given extra paths.
augmentPath :: MonadThrow m => [Path Abs Dir] -> Maybe Text -> m Text
augmentPath dirs mpath =
  do let illegal = filter (FP.searchPathSeparator `elem`) (map toFilePath dirs)
     unless (null illegal) (throwM $ PathsInvalidInPath illegal)
     return $ T.intercalate (T.singleton FP.searchPathSeparator)
            $ map (T.pack . toFilePathNoTrailingSep) dirs
            ++ maybeToList mpath

-- | Apply 'augmentPath' on the PATH value in the given Map.
augmentPathMap :: MonadThrow m => [Path Abs Dir] -> Map Text Text
                               -> m (Map Text Text)
augmentPathMap dirs origEnv =
  do path <- augmentPath dirs mpath
     return $ Map.insert "PATH" path origEnv
  where
    mpath = Map.lookup "PATH" origEnv
