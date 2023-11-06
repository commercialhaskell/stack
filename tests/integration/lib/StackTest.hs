{-# LANGUAGE ScopedTypeVariables #-}

module StackTest
  ( run'
  , run
  , runShell
  , runWithCwd
  , stackExe
  , stackSrc
  , testDir
  , stack'
  , stack
  , stackCleanFull
  , stackIgnoreException
  , stackErr
  , Repl
  , ReplConnection (..)
  , nextPrompt
  , replCommand
  , replGetChar
  , replGetLine
  , runRepl
  , repl
  , stackStderr
  , stackCheckStderr
  , stackErrStderr
  , runEx
  , runEx'
  , stackCheckStdout
  , doesNotExist
  , doesExist
  , doesFileOrDirExist
  , copy
  , fileContentsMatch
  , logInfo
  , showProcessArgDebug
  , exeExt
  , isWindows
  , isLinux
  , getIsAlpine
  , isARM
  , isAarch64
  , isMacOSX
  , defaultResolverArg
  , removeFileIgnore
  , removeDirIgnore
  , withCwd
  , withSourceDirectory
  , superslow
  ) where

import           Control.Monad ( forever, unless, void, when )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.Reader ( ReaderT, ask, runReaderT )
import           Control.Concurrent ( forkIO )
import           Control.Exception
                   ( Exception (..), IOException, bracket_, catch, throw
                   , throwIO
                   )
import           Data.Maybe ( fromMaybe )
import           GHC.Stack ( HasCallStack )
import           System.Environment ( getEnv, lookupEnv )
import           System.Directory
                   ( copyFile, doesDirectoryExist, doesFileExist
                   , getCurrentDirectory, removeDirectoryRecursive, removeFile
                   , setCurrentDirectory
                   )
import           System.IO
                   ( BufferMode (..), Handle, IOMode (..), hGetChar, hGetLine
                   , hPutChar, hPutStr, hPutStrLn, hSetBuffering, stderr
                   , withFile
                   )
import           System.IO.Error
                   ( isDoesNotExistError, isEOFError )
import           System.Process
                   ( CreateProcess (..), StdStream (..), createProcess, proc
                   , readCreateProcessWithExitCode, readProcessWithExitCode
                   , shell, waitForProcess
                   )
import           System.Exit ( ExitCode (..) )
import           System.Info ( arch, os )

run' :: HasCallStack => FilePath -> [String] -> IO ExitCode
run' cmd args = do
  logInfo $ "Running: " ++ cmd ++ " " ++ unwords (map showProcessArgDebug args)
  (Nothing, Nothing, Nothing, ph) <- createProcess (proc cmd args)
  waitForProcess ph

run :: HasCallStack => FilePath -> [String] -> IO ()
run cmd args = do
  ec <- run' cmd args
  unless (ec == ExitSuccess) $
    error $ "Exited with exit code: " ++ displayException ec

runShell :: HasCallStack => String -> IO ()
runShell cmd = do
  logInfo $ "Running: " ++ cmd
  (Nothing, Nothing, Nothing, ph) <- createProcess (shell cmd)
  ec <- waitForProcess ph
  unless (ec == ExitSuccess) $
    error $ "Exited with exit code: " ++ displayException ec

runWithCwd :: HasCallStack => FilePath -> String -> [String] -> IO String
runWithCwd cwdPath cmd args = do
  logInfo $ "Running: " ++ cmd
  let cp = proc cmd args
  (ec, stdoutStr, _) <- readCreateProcessWithExitCode (cp { cwd = Just cwdPath }) ""
  unless (ec == ExitSuccess) $
    error $ "Exited with exit code: " ++ displayException ec
  pure stdoutStr

stackExe :: IO String
stackExe = getEnv "STACK_EXE"

stackSrc :: IO String
stackSrc = getEnv "SRC_DIR"

testDir :: IO String
testDir = getEnv "TEST_DIR"

stack' :: HasCallStack => [String] -> IO ExitCode
stack' args = do
  stackEnv <- stackExe
  run' stackEnv args

stack :: HasCallStack => [String] -> IO ()
stack args = do
  ec <- stack' args
  unless (ec == ExitSuccess) $
    error $ "Exited with exit code: " ++ displayException ec

-- Temporary workaround for Windows to ignore exceptions arising out of Windows
-- when we do stack clean. More info here:
-- https://github.com/commercialhaskell/stack/issues/4936
stackCleanFull :: HasCallStack => IO ()
stackCleanFull = stackIgnoreException ["clean", "--full"]

-- Temporary workaround for Windows to ignore exceptions arising out of Windows
-- when we do stack clean. More info here:
-- https://github.com/commercialhaskell/stack/issues/4936
stackIgnoreException :: HasCallStack => [String] -> IO ()
stackIgnoreException args =
  if isWindows
    then void (stack' args) `catch` (\(_e :: IOException) -> pure ())
    else stack args

stackErr :: HasCallStack => [String] -> IO ()
stackErr args = do
  ec <- stack' args
  when (ec == ExitSuccess) $ error "stack was supposed to fail, but didn't"

type Repl = ReaderT ReplConnection IO

data ReplConnection = ReplConnection
  { replStdin  :: Handle
  , replStdout :: Handle
  }

nextPrompt :: Repl ()
nextPrompt = do
  (ReplConnection _ replStdoutHandle) <- ask
  c <- liftIO $ hGetChar replStdoutHandle
  if c == '>'
    then do
      -- Skip next character
      _ <- liftIO $ hGetChar replStdoutHandle
      pure ()
    else nextPrompt

replCommand :: String -> Repl ()
replCommand cmd = do
  (ReplConnection replStdinHandle _) <- ask
  liftIO $ hPutStrLn replStdinHandle cmd

replGetLine :: Repl String
replGetLine = ask >>= liftIO . hGetLine . replStdout

replGetChar :: Repl Char
replGetChar = ask >>= liftIO . hGetChar . replStdout

runRepl ::
     HasCallStack
  => FilePath
  -> [String]
  -> ReaderT ReplConnection IO ()
  -> IO ExitCode
runRepl cmd args actions = do
  logInfo $ "Running: " ++ cmd ++ " " ++ unwords (map showProcessArgDebug args)
  (Just rStdin, Just rStdout, Just rStderr, ph) <-
    createProcess (proc cmd args)
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }
  hSetBuffering rStdin NoBuffering
  hSetBuffering rStdout NoBuffering
  hSetBuffering rStderr NoBuffering
  -- Log stack repl's standard error output
  tempDir <- if isWindows
                then fromMaybe "" <$> lookupEnv "TEMP"
                else pure "/tmp"
  let tempLogFile = tempDir ++ "/stderr"
  _ <- forkIO $ withFile tempLogFile WriteMode $ \logFileHandle -> do
    hSetBuffering logFileHandle NoBuffering
    forever $
      catch
        (hGetChar rStderr >>= hPutChar logFileHandle)
        (\e -> unless (isEOFError e) $ throw e)
  runReaderT actions (ReplConnection rStdin rStdout)
  waitForProcess ph

repl :: HasCallStack => [String] -> Repl () -> IO ()
repl args action = do
  stackExe' <- stackExe
  ec <- runRepl stackExe' ("repl":args) action
  unless (ec == ExitSuccess) $ pure ()
  -- TODO: Understand why the exit code is 1 despite running GHCi tests
  -- successfully.
  -- else error $ "Exited with exit code: " ++ show ec

stackStderr :: HasCallStack => [String] -> IO (ExitCode, String)
stackStderr args = do
  stackExe' <- stackExe
  logInfo $
       "Running: "
    ++ stackExe'
    ++ " "
    ++ unwords (map showProcessArgDebug args)
  (ec, _, err) <- readProcessWithExitCode stackExe' args ""
  hPutStr stderr err
  pure (ec, err)

-- | Run stack with arguments and apply a check to the resulting stderr output
-- if the process succeeded.
stackCheckStderr :: HasCallStack => [String] -> (String -> IO ()) -> IO ()
stackCheckStderr args check = do
  (ec, err) <- stackStderr args
  if ec /= ExitSuccess
    then error $ "Exited with exit code: " ++ displayException ec
    else check err

-- | Same as 'stackCheckStderr', but ensures that the Stack process
-- fails.
stackErrStderr :: HasCallStack => [String] -> (String -> IO ()) -> IO ()
stackErrStderr args check = do
  (ec, err) <- stackStderr args
  if ec == ExitSuccess
    then error "Stack process succeeded, but it shouldn't"
    else check err

runEx :: HasCallStack => FilePath -> String -> IO (ExitCode, String, String)
runEx cmd args = runEx' cmd $ words args

runEx' :: HasCallStack => FilePath -> [String] -> IO (ExitCode, String, String)
runEx' cmd args = do
  logInfo $ "Running: " ++ cmd ++ " " ++ unwords (map showProcessArgDebug args)
  (ec, out, err) <- readProcessWithExitCode cmd args ""
  putStr out
  hPutStr stderr err
  pure (ec, out, err)

-- | Run stack with arguments and apply a check to the resulting stdout output
-- if the process succeeded.
--
-- Take care with newlines; if the output includes a newline character that
-- should not be there, use 'Data.List.Extra.trimEnd' to remove it.
stackCheckStdout :: HasCallStack => [String] -> (String -> IO ()) -> IO ()
stackCheckStdout args check = do
  stackExe' <- stackExe
  (ec, out, _) <- runEx' stackExe' args
  if ec /= ExitSuccess
    then error $ "Exited with exit code: " ++ displayException ec
    else check out

doesNotExist :: HasCallStack => FilePath -> IO ()
doesNotExist fp = do
  logInfo $ "doesNotExist " ++ fp
  exists <- doesFileOrDirExist fp
  case exists of
    (Right msg) -> error msg
    (Left _) -> pure ()

doesExist :: HasCallStack => FilePath -> IO ()
doesExist fp = do
  logInfo $ "doesExist " ++ fp
  exists <- doesFileOrDirExist fp
  case exists of
    (Right _) -> pure ()
    (Left _) -> error "No file or directory exists"

doesFileOrDirExist :: HasCallStack => FilePath -> IO (Either () String)
doesFileOrDirExist fp = do
  isFile <- doesFileExist fp
  if isFile
    then pure (Right ("File exists: " ++ fp))
    else do
      isDir <- doesDirectoryExist fp
      if isDir
        then pure (Right ("Directory exists: " ++ fp))
        else pure (Left ())

copy :: HasCallStack => FilePath -> FilePath -> IO ()
copy src dest = do
  logInfo ("Copy " ++ show src ++ " to " ++ show dest)
  System.Directory.copyFile src dest

fileContentsMatch :: HasCallStack => FilePath -> FilePath -> IO ()
fileContentsMatch f1 f2 = do
  doesExist f1
  doesExist f2
  f1Contents <- readFile f1
  f2Contents <- readFile f2
  unless (f1Contents == f2Contents) $
    error ("contents do not match for " ++ show f1 ++ " " ++ show f2)

logInfo :: String -> IO ()
logInfo = hPutStrLn stderr

-- TODO: use Stack's process running utilities?  (better logging)
-- for now just copy+modifying this one from System.Process.Log

-- | Show a process arg including speechmarks when necessary. Just for
-- debugging purposes, not functionally important.
showProcessArgDebug :: String -> String
showProcessArgDebug x
  | any special x = show x
  | otherwise = x
 where
  special '"' = True
  special ' ' = True
  special _ = False

-- | Extension of executables
exeExt :: String
exeExt = if isWindows then ".exe" else ""

-- | Is the OS Windows?
isWindows :: Bool
isWindows = os == "mingw32"

isLinux :: Bool
isLinux = os == "linux"

-- | Is the OS Alpine Linux?
getIsAlpine :: IO Bool
getIsAlpine = doesFileExist "/etc/alpine-release"

-- | Is the architecture ARM?
isARM :: Bool
isARM = arch == "arm"

-- | Is the architecture Aarch64?
isAarch64 :: Bool
isAarch64 = arch == "aarch64"

-- | Is the OS Mac OS X?
isMacOSX :: Bool
isMacOSX = os == "darwin"

-- | To avoid problems with GHC version mismatch when a new LTS major
-- version is released, pass this argument to @stack@ when running in
-- a global context. The LTS major version here should match that of
-- the main @stack.yaml@.
--
defaultResolverArg :: String
defaultResolverArg = "--resolver=lts-21.16"

-- | Remove a file and ignore any warnings about missing files.
removeFileIgnore :: HasCallStack => FilePath -> IO ()
removeFileIgnore fp = removeFile fp `catch` \e ->
  if isDoesNotExistError e
    then pure ()
    else throwIO e

-- | Remove a directory and ignore any warnings about missing files.
removeDirIgnore :: HasCallStack => FilePath -> IO ()
removeDirIgnore fp = removeDirectoryRecursive fp `catch` \e ->
  if isDoesNotExistError e
    then pure ()
    else throwIO e

-- | Changes to the specified working directory.
withCwd :: HasCallStack => FilePath -> IO () -> IO ()
withCwd dir action = do
  currentDirectory <- getCurrentDirectory
  let enterDir = setCurrentDirectory dir
      exitDir = setCurrentDirectory currentDirectory
  bracket_ enterDir exitDir action

-- | Changes working directory to Stack source directory.
withSourceDirectory :: HasCallStack => IO () -> IO ()
withSourceDirectory action = do
  dir <- stackSrc
  withCwd dir action

-- | Mark a test as superslow, only to be run when explicitly requested.
superslow :: HasCallStack => IO () -> IO ()
superslow inner = do
  mres <- lookupEnv "STACK_TEST_SPEED"
  case mres of
    Just "NORMAL" -> logInfo "Skipping superslow test"
    Just "SUPERSLOW" -> do
      logInfo "Running superslow test, hold on to your butts"
      inner
    Nothing -> do
      logInfo "No STACK_TEST_SPEED specified. Executing superslow test, hold \
              \on to your butts"
      inner
    Just x -> error $ "Invalid value for STACK_TEST_SPEED env var: " ++ show x
