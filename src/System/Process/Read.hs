{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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
  ,HasEnvOverride (..)
  ,workingDirL
  ,withProc
  ,withEnvOverride
  ,withModifyEnvOverride
  ,withWorkingDir
  ,runProcess
  ,runProcess_
  ,runEnvNoLogging
  )
  where

import           Stack.Prelude
import qualified Data.ByteString as S
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process.Typed
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Distribution.System (OS (Windows), Platform (Platform), buildPlatform)
import           Lens.Micro (set, to)
import           Path
import           Path.Extra
import           Path.IO hiding (findExecutable)
import qualified System.Directory as D
import           System.Environment (getEnvironment)
import qualified System.FilePath as FP
import           System.Process.Log

class HasLogFunc env => HasEnvOverride env where
  envOverrideL :: Lens' env EnvOverride

data EnvVarFormat = EVFWindows | EVFNotWindows

evfFromPlatform :: Platform -> EnvVarFormat
evfFromPlatform (Platform _ Windows) = EVFWindows
evfFromPlatform (Platform _ _) = EVFNotWindows

currentEnvVarFormat :: EnvVarFormat
currentEnvVarFormat = evfFromPlatform buildPlatform

-- | Override the environment received by a child process.
data EnvOverride = EnvOverride
    { eoTextMap :: Map Text Text -- ^ Environment variables as map
    , eoStringList :: [(String, String)] -- ^ Environment variables as association list
    , eoPath :: [FilePath] -- ^ List of directories searched for executables (@PATH@)
    , eoExeCache :: IORef (Map FilePath (Either ReadProcessException (Path Abs File)))
    , eoExeExtensions :: [String] -- ^ @[""]@ on non-Windows systems, @["", ".exe", ".bat"]@ on Windows
    , eoWorkingDir :: !(Maybe (Path Abs Dir))
    }

workingDirL :: HasEnvOverride env => Lens' env (Maybe (Path Abs Dir))
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
readProcessNull :: HasEnvOverride env
                => String -- ^ Command
                -> [String] -- ^ Command line arguments
                -> RIO env ()
readProcessNull name args = sinkProcessStdout name args CL.sinkNull

-- | Try to produce a strict 'S.ByteString' from the stdout of a
-- process.
tryProcessStdout :: HasEnvOverride env
                 => String -- ^ Command
                 -> [String] -- ^ Command line arguments
                 -> RIO env (Either SomeException S.ByteString)
tryProcessStdout name args = tryAny (readProcessStdout name args)

-- | Try to produce strict 'S.ByteString's from the stderr and stdout of a
-- process.
tryProcessStderrStdout
  :: HasEnvOverride env
  => String -- ^ Command
  -> [String] -- ^ Command line arguments
  -> RIO env (Either ReadProcessException (S.ByteString, S.ByteString))
tryProcessStderrStdout name args =
    try (readProcessStderrStdout name args)

-- | Produce a strict 'S.ByteString' from the stdout of a process.
--
-- Throws a 'ReadProcessException' exception if the process fails.
readProcessStdout
  :: HasEnvOverride env
  => String -- ^ Command
  -> [String] -- ^ Command line arguments
  -> RIO env S.ByteString
readProcessStdout name args =
  sinkProcessStdout name args CL.consume >>=
  liftIO . evaluate . S.concat

-- | Produce strict 'S.ByteString's from the stderr and stdout of a process.
--
-- Throws a 'ReadProcessException' exception if the process fails.
readProcessStderrStdout
  :: HasEnvOverride env
  => String -- ^ Command
  -> [String] -- ^ Command line arguments
  -> RIO env (S.ByteString, S.ByteString)
readProcessStderrStdout name args = do
  (e, o) <- sinkProcessStderrStdout name args CL.consume CL.consume
  liftIO $ (,) <$> evaluate (S.concat e) <*> evaluate (S.concat o)

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

-- | Consume the stdout of a process feeding strict 'S.ByteString's to a consumer.
-- If the process fails, spits out stdout and stderr as error log
-- level. Should not be used for long-running processes or ones with
-- lots of output; for that use 'sinkProcessStdoutLogStderr'.
--
-- Throws a 'ReadProcessException' if unsuccessful.
sinkProcessStdout
    :: HasEnvOverride env
    => String -- ^ Command
    -> [String] -- ^ Command line arguments
    -> Sink S.ByteString (RIO env) a -- ^ Sink for stdout
    -> RIO env a
sinkProcessStdout name args sinkStdout =
  withProc name args $ \pc ->
  withLoggedProcess_ (setStdin closed pc) $ \p -> runConcurrently
    $ Concurrently (runConduit $ getStderr p .| CL.sinkNull)
   *> Concurrently (runConduit $ getStdout p .| sinkStdout)

logProcessStderrStdout
    :: (HasCallStack, HasEnvOverride env)
    => String
    -> [String]
    -> RIO env ()
logProcessStderrStdout name args = do
    let logLines = CB.lines =$ CL.mapM_ (logInfo . decodeUtf8With lenientDecode)
    ((), ()) <- sinkProcessStderrStdout name args logLines logLines
    return ()

-- | Consume the stdout and stderr of a process feeding strict 'S.ByteString's to the consumers.
--
-- Throws a 'ReadProcessException' if unsuccessful in launching, or 'ProcessExitedUnsuccessfully' if the process itself fails.
sinkProcessStderrStdout
  :: forall e o env. HasEnvOverride env
  => String -- ^ Command
  -> [String] -- ^ Command line arguments
  -> Sink S.ByteString (RIO env) e -- ^ Sink for stderr
  -> Sink S.ByteString (RIO env) o -- ^ Sink for stdout
  -> RIO env (e,o)
sinkProcessStderrStdout name args sinkStderr sinkStdout =
  withProc name args $ \pc0 -> do
    let pc = setStdin closed
           $ setStdout createSource
           $ setStderr createSource
             pc0
    withProcess_ pc $ \p ->
      runConduit (getStderr p .| sinkStderr) `concurrently`
      runConduit (getStdout p .| sinkStdout)

-- | Like sinkProcessStderrStdout, but receives Handles for stderr and stdout instead of 'Sink's.
--
-- Throws a 'ReadProcessException' if unsuccessful in launching, or 'ProcessExitedUnsuccessfully' if the process itself fails.
sinkProcessStderrStdoutHandle
  :: HasEnvOverride env
  => String -- ^ Command
  -> [String] -- ^ Command line arguments
  -> Handle
  -> Handle
  -> RIO env ()
sinkProcessStderrStdoutHandle name args err out =
      withProc name args
    $ runProcess_
    . setStdin closed
    . setStdout (useHandleOpen out)
    . setStderr (useHandleOpen err)

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

  withProcessTimeLog (toFilePath <$> eoWorkingDir menv) name args
    $ inner
    $ setDelegateCtlc True
    $ setEnv (envHelper menv)
    $ maybe id (setWorkingDir . toFilePath) (eoWorkingDir menv)

    -- sensible default in Stack: we do not want subprocesses to be
    -- able to interact with the user by default. If a specific case
    -- requires interaction, we can override with `setStdin
    -- (useHandleOpen stdin)`.
    $ setStdin closed

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
withWorkingDir :: HasEnvOverride env => Path Abs Dir -> RIO env a -> RIO env a
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

runEnvNoLogging :: RIO EnvNoLogging a -> IO a
runEnvNoLogging inner = do
  menv <- getEnvOverride
  runRIO (EnvNoLogging menv) inner

newtype EnvNoLogging = EnvNoLogging EnvOverride
instance HasLogFunc EnvNoLogging where
  logFuncL = to (\_ _ _ _ _ -> return ())
instance HasEnvOverride EnvNoLogging where
  envOverrideL = lens (\(EnvNoLogging x) -> x) (const EnvNoLogging)
