{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading from external processes.

module RIO.Process
  (withProcess
  ,withProcess_
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
  ,HasEnvOverride (..)
  ,workingDirL
  ,withProc
  ,withEnvOverride
  ,withModifyEnvOverride
  ,withWorkingDir
  ,runEnvNoLogging
  ,withProcessTimeLog
  ,showProcessArgDebug
  ,exec
  ,execSpawn
  ,execObserve
  ,module System.Process.Typed
  )
  where

import           RIO.Prelude
import           RIO.Logger
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Text.Encoding.Error (lenientDecode)
import           Lens.Micro (set, to)
import qualified System.Directory as D
import           System.Environment (getEnvironment)
import           System.Exit (exitWith)
import qualified System.FilePath as FP
import qualified System.Process.Typed as P
import           System.Process.Typed hiding (withProcess, withProcess_)

#ifndef WINDOWS
import           System.Directory (setCurrentDirectory)
import           System.Posix.Process (executeFile)
#endif

class HasLogFunc env => HasEnvOverride env where
  envOverrideL :: Lens' env EnvOverride

data EnvVarFormat = EVFWindows | EVFNotWindows

currentEnvVarFormat :: EnvVarFormat
currentEnvVarFormat =
#if WINDOWS
  EVFWindows
#else
  EVFNotWindows
#endif

-- | Override the environment received by a child process.
data EnvOverride = EnvOverride
    { eoTextMap :: Map Text Text -- ^ Environment variables as map
    , eoStringList :: [(String, String)] -- ^ Environment variables as association list
    , eoPath :: [FilePath] -- ^ List of directories searched for executables (@PATH@)
    , eoExeCache :: IORef (Map FilePath (Either ReadProcessException FilePath))
    , eoExeExtensions :: [String] -- ^ @[""]@ on non-Windows systems, @["", ".exe", ".bat"]@ on Windows
    , eoWorkingDir :: !(Maybe FilePath)
    }

workingDirL :: HasEnvOverride env => Lens' env (Maybe FilePath)
workingDirL = envOverrideL.lens eoWorkingDir (\x y -> x { eoWorkingDir = y })

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
modifyEnvOverride eo f = mkEnvOverride (f $ eoTextMap eo)

-- | Create a new 'EnvOverride'.
mkEnvOverride :: MonadIO m
              => Map Text Text
              -> m EnvOverride
mkEnvOverride tm' = do
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
        , eoWorkingDir = Nothing
        }
  where
    -- Fix case insensitivity of the PATH environment variable on Windows.
    tm
        | isWindows = Map.fromList $ map (first T.toUpper) $ Map.toList tm'
        | otherwise = tm'

    -- Don't use CPP so that the Windows code path is at least type checked
    -- regularly
    isWindows =
        case currentEnvVarFormat of
            EVFWindows -> True
            EVFNotWindows -> False

-- | Helper conversion function.
envHelper :: EnvOverride -> [(String, String)]
envHelper = eoStringList

-- | Read from the process, ignoring any output.
--
-- Throws a 'ReadProcessException' exception if the process fails.
readProcessNull :: HasEnvOverride env -- FIXME remove
                => String -- ^ Command
                -> [String] -- ^ Command line arguments
                -> RIO env ()
readProcessNull name args =
  -- We want the output to appear in any exceptions, so we capture and drop it
  void $ withProc name args readProcessStdout_

-- | An exception while trying to read from process.
data ReadProcessException
    = NoPathFound
    | ExecutableNotFound String [FilePath]
    | ExecutableNotFoundAt FilePath
    deriving Typeable
instance Show ReadProcessException where
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

-- | Provide a 'ProcessConfig' based on the 'EnvOverride' in
-- scope. Deals with resolving the full path, setting the child
-- process's environment variables, setting the working directory, and
-- wrapping the call with 'withProcessTimeLog' for debugging output.
withProc
  :: HasEnvOverride env
  => FilePath -- ^ command to run
  -> [String] -- ^ command line arguments
  -> (ProcessConfig () () () -> RIO env a)
  -> RIO env a
withProc name0 args inner = do
  menv <- view envOverrideL
  name <- preProcess name0

  withProcessTimeLog (eoWorkingDir menv) name args
    $ inner
    $ setEnv (envHelper menv)
    $ maybe id setWorkingDir (eoWorkingDir menv)
    $ proc name args

-- | Apply the given function to the modified environment
-- variables. For more details, see 'withEnvOverride'.
withModifyEnvOverride :: HasEnvOverride env => (Map Text Text -> Map Text Text) -> RIO env a -> RIO env a
withModifyEnvOverride f inner = do
  menv <- view envOverrideL
  menv' <- modifyEnvOverride menv f
  withEnvOverride menv' inner

-- | Set a new 'EnvOverride' in the child reader. Note that this will
-- keep the working directory set in the parent with 'withWorkingDir'.
withEnvOverride :: HasEnvOverride env => EnvOverride -> RIO env a -> RIO env a
withEnvOverride newEnv = local $ \r ->
  let newEnv' = newEnv { eoWorkingDir = eoWorkingDir $ view envOverrideL r }
   in set envOverrideL newEnv' r

-- | Set the working directory to be used by child processes.
withWorkingDir :: HasEnvOverride env => FilePath -> RIO env a -> RIO env a
withWorkingDir = local . set workingDirL . Just

-- | Perform pre-call-process tasks.  Ensure the working directory exists and find the
-- executable path.
--
-- Throws a 'ReadProcessException' if unsuccessful.
preProcess
  :: HasEnvOverride env
  => String            -- ^ Command name
  -> RIO env  FilePath
preProcess name = do
  menv <- view envOverrideL
  let wd = eoWorkingDir menv
  name' <- liftIO $ join $ findExecutable menv name
  liftIO $ maybe (return ()) (D.createDirectoryIfMissing True) wd
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
  -> m (n FilePath) -- ^ Full path to that executable on success
findExecutable eo name0 | any FP.isPathSeparator name0 = do
    let names0 = map (name0 ++) (eoExeExtensions eo)
        testNames [] = return $ throwM $ ExecutableNotFoundAt name0
        testNames (name:names) = do
            exists <- liftIO $ D.doesFileExist name
            if exists
                then do
                    path <- liftIO $ D.canonicalizePath name
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
                                    fp' <- D.makeAbsolute fp
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
getEnvOverride :: MonadIO m => m EnvOverride
getEnvOverride =
    liftIO $
    getEnvironment >>=
          mkEnvOverride
        . Map.fromList . map (T.pack *** T.pack)

newtype InvalidPathException = PathsInvalidInPath [FilePath]
    deriving Typeable

instance Exception InvalidPathException
instance Show InvalidPathException where
    show (PathsInvalidInPath paths) = unlines $
        [ "Would need to add some paths to the PATH environment variable \
          \to continue, but they would be invalid because they contain a "
          ++ show FP.searchPathSeparator ++ "."
        , "Please fix the following paths and try again:"
        ] ++ paths

-- | Augment the PATH environment variable with the given extra paths.
augmentPath :: MonadThrow m => [FilePath] -> Maybe Text -> m Text
augmentPath dirs mpath =
  do let illegal = filter (FP.searchPathSeparator `elem`) dirs
     unless (null illegal) (throwM $ PathsInvalidInPath illegal)
     return $ T.intercalate (T.singleton FP.searchPathSeparator)
            $ map (T.pack . FP.dropTrailingPathSeparator) dirs
            ++ maybeToList mpath

-- | Apply 'augmentPath' on the PATH value in the given Map.
augmentPathMap :: MonadThrow m => [FilePath] -> Map Text Text -> m (Map Text Text)
augmentPathMap dirs origEnv =
  do path <- augmentPath dirs mpath
     return $ Map.insert "PATH" path origEnv
  where
    mpath = Map.lookup "PATH" origEnv

runEnvNoLogging :: RIO EnvNoLogging a -> IO a
runEnvNoLogging inner = do
  menv <- getEnvOverride
  runRIO (EnvNoLogging menv) inner

newtype EnvNoLogging = EnvNoLogging EnvOverride
instance HasLogFunc EnvNoLogging where
  logFuncL = to (\_ _ _ _ _ -> return ())
instance HasEnvOverride EnvNoLogging where
  envOverrideL = lens (\(EnvNoLogging x) -> x) (const EnvNoLogging)

-- | Log running a process with its arguments, for debugging (-v).
--
-- This logs one message before running the process and one message after.
withProcessTimeLog :: (MonadIO m, MonadReader env m, HasLogFunc env, HasCallStack) => Maybe FilePath -> String -> [String] -> m a -> m a
withProcessTimeLog mdir name args proc' = do
  let cmdText =
          T.intercalate
              " "
              (T.pack name : map showProcessArgDebug args)
      dirMsg =
        case mdir of
          Nothing -> ""
          Just dir -> " within " <> T.pack dir
  logDebug ("Run process" <> display dirMsg <> ": " <> display cmdText)
  start <- getMonotonicTime
  x <- proc'
  end <- getMonotonicTime
  let diff = end - start
  -- useAnsi <- asks getAnsiTerminal FIXME
  let useAnsi = True
  logDebug
      ("Process finished in " <>
      (if useAnsi then "\ESC[92m" else "") <> -- green
      timeSpecMilliSecondText diff <>
      (if useAnsi then "\ESC[0m" else "") <> -- reset
       ": " <> display cmdText)
  return x

timeSpecMilliSecondText :: Double -> DisplayBuilder
timeSpecMilliSecondText d = display (round (d * 1000) :: Int) <> "ms"

-- | Show a process arg including speechmarks when necessary. Just for
-- debugging purposes, not functionally important.
showProcessArgDebug :: String -> Text
showProcessArgDebug x
    | any special x || null x = T.pack (show x)
    | otherwise = T.pack x
  where special '"' = True
        special ' ' = True
        special _ = False

-- | Execute a process within the Stack configured environment.
--
-- Execution will not return, because either:
--
-- 1) On non-windows, execution is taken over by execv of the
-- sub-process. This allows signals to be propagated (#527)
--
-- 2) On windows, an 'ExitCode' exception will be thrown.
exec :: HasEnvOverride env => String -> [String] -> RIO env b
#ifdef WINDOWS
exec = execSpawn
#else
exec cmd0 args = do
    menv <- view envOverrideL
    cmd <- preProcess cmd0
    withProcessTimeLog Nothing cmd args $ liftIO $ do
      for_ (eoWorkingDir menv) setCurrentDirectory
      executeFile cmd True args $ Just $ envHelper menv
#endif

-- | Like 'exec', but does not use 'execv' on non-windows. This way, there
-- is a sub-process, which is helpful in some cases (#1306)
--
-- This function only exits by throwing 'ExitCode'.
execSpawn :: HasEnvOverride env => String -> [String] -> RIO env a
execSpawn cmd args = withProc cmd args (runProcess . setStdin inherit) >>= liftIO . exitWith

execObserve :: HasEnvOverride env => String -> [String] -> RIO env String
execObserve cmd0 args =
  withProc cmd0 args $ \pc -> do
    (out, _err) <- readProcess_ pc
    return
      $ TL.unpack
      $ TL.filter (/= '\r')
      $ TL.concat
      $ take 1
      $ TL.lines
      $ TLE.decodeUtf8With lenientDecode out

-- | Same as 'P.withProcess', but generalized to 'MonadUnliftIO'.
withProcess
  :: MonadUnliftIO m
  => ProcessConfig stdin stdout stderr
  -> (Process stdin stdout stderr -> m a)
  -> m a
withProcess pc f = withRunInIO $ \run -> P.withProcess pc (run . f)

-- | Same as 'P.withProcess_', but generalized to 'MonadUnliftIO'.
withProcess_
  :: MonadUnliftIO m
  => ProcessConfig stdin stdout stderr
  -> (Process stdin stdout stderr -> m a)
  -> m a
withProcess_ pc f = withRunInIO $ \run -> P.withProcess_ pc (run . f)
