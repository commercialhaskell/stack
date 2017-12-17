module StackTest where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Concurrent
import Control.Exception
import System.Environment
import System.Directory
import System.IO
import System.IO.Error
import System.Process
import System.Exit
import System.Info (arch, os)

run' :: FilePath -> [String] -> IO ExitCode
run' cmd args = do
    logInfo $ "Running: " ++ cmd ++ " " ++ unwords (map showProcessArgDebug args)
    (Nothing, Nothing, Nothing, ph) <- createProcess (proc cmd args)
    waitForProcess ph

run :: FilePath -> [String] -> IO ()
run cmd args = do
    ec <- run' cmd args
    unless (ec == ExitSuccess) $ error $ "Exited with exit code: " ++ show ec

stackExe :: IO String
stackExe = getEnv "STACK_EXE"

stack' :: [String] -> IO ExitCode
stack' args = do
    stackEnv <- stackExe
    run' stackEnv args

stack :: [String] -> IO ()
stack args = do
    ec <- stack' args
    unless (ec == ExitSuccess) $ error $ "Exited with exit code: " ++ show ec

stackErr :: [String] -> IO ()
stackErr args = do
    ec <- stack' args
    when (ec == ExitSuccess) $ error "stack was supposed to fail, but didn't"

type Repl = ReaderT ReplConnection IO

data ReplConnection
  = ReplConnection
    { replStdin  :: Handle
    , replStdout :: Handle
    }

nextPrompt :: Repl ()
nextPrompt = do
    (ReplConnection _ inputHandle) <- ask
    c <- liftIO $ hGetChar inputHandle
    if c == '>'
      then do _ <- liftIO $ hGetChar inputHandle
              return ()
      else nextPrompt

replCommand :: String -> Repl ()
replCommand cmd = do
    (ReplConnection input _) <- ask
    liftIO $ hPutStrLn input cmd

replGetLine :: Repl String
replGetLine = fmap replStdout ask >>= liftIO . hGetLine

replGetChar :: Repl Char
replGetChar = fmap replStdout ask >>= liftIO . hGetChar

runRepl :: FilePath -> [String] -> ReaderT ReplConnection IO () -> IO ExitCode
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

    _ <- forkIO $ withFile "/tmp/stderr" WriteMode
        $ \err -> forever $ catch (hGetChar rStderr >>= hPutChar err)
                  $ \e -> unless (isEOFError e) $ throw e

    runReaderT (nextPrompt >> actions) (ReplConnection rStdin rStdout)
    waitForProcess ph

repl :: [String] -> Repl () -> IO ()
repl args action = do
    stackExe' <- stackExe
    ec <- runRepl stackExe' ("repl":args) action
    unless (ec == ExitSuccess) $ return ()
        -- TODO: Understand why the exit code is 1 despite running GHCi tests
        -- successfully.
        -- else error $ "Exited with exit code: " ++ show ec

stackStderr :: [String] -> IO (ExitCode, String)
stackStderr args = do
    stackExe' <- stackExe
    logInfo $ "Running: " ++ stackExe' ++ " " ++ unwords (map showProcessArgDebug args)
    (ec, _, err) <- readProcessWithExitCode stackExe' args ""
    hPutStr stderr err
    return (ec, err)

-- | Run stack with arguments and apply a check to the resulting
-- stderr output if the process succeeded.
stackCheckStderr :: [String] -> (String -> IO ()) -> IO ()
stackCheckStderr args check = do
    (ec, err) <- stackStderr args
    if ec /= ExitSuccess
        then error $ "Exited with exit code: " ++ show ec
        else check err

-- | Same as 'stackCheckStderr', but ensures that the Stack process
-- fails.
stackErrStderr :: [String] -> (String -> IO ()) -> IO ()
stackErrStderr args check = do
    (ec, err) <- stackStderr args
    if ec == ExitSuccess
        then error "Stack process succeeded, but it shouldn't"
        else check err

doesNotExist :: FilePath -> IO ()
doesNotExist fp = do
    logInfo $ "doesNotExist " ++ fp
    exists <- doesFileOrDirExist fp
    case exists of
      (Right msg) -> error msg
      (Left _) -> return ()

doesExist :: FilePath -> IO ()
doesExist fp = do
    logInfo $ "doesExist " ++ fp
    exists <- doesFileOrDirExist fp
    case exists of
      (Right _) -> return ()
      (Left _) -> error "No file or directory exists"

doesFileOrDirExist :: FilePath -> IO (Either () String)
doesFileOrDirExist fp = do
    isFile <- doesFileExist fp
    if isFile
        then return (Right ("File exists: " ++ fp))
        else do
            isDir <- doesDirectoryExist fp
            if isDir
                then return (Right ("Directory exists: " ++ fp))
                else return (Left ())

copy :: FilePath -> FilePath -> IO ()
copy src dest = do
    logInfo ("Copy " ++ show src ++ " to " ++ show dest)
    System.Directory.copyFile src dest

fileContentsMatch :: FilePath -> FilePath -> IO ()
fileContentsMatch f1 f2 = do
    doesExist f1
    doesExist f2
    f1Contents <- readFile f1
    f2Contents <- readFile f2
    unless (f1Contents == f2Contents) $
        error ("contents do not match for " ++ show f1 ++ " " ++ show f2)

logInfo :: String -> IO ()
logInfo = hPutStrLn stderr

-- TODO: use stack's process running utilties?  (better logging)
-- for now just copy+modifying this one from System.Process.Log

-- | Show a process arg including speechmarks when necessary. Just for
-- debugging purposes, not functionally important.
showProcessArgDebug :: String -> String
showProcessArgDebug x
    | any special x = show x
    | otherwise = x
  where special '"' = True
        special ' ' = True
        special _ = False

-- | Extension of executables
exeExt :: String
exeExt = if isWindows then ".exe" else ""

-- | Is the OS Windows?
isWindows :: Bool
isWindows = os == "mingw32"

-- | Is the OS Alpine Linux?
getIsAlpine :: IO Bool
getIsAlpine = doesFileExist "/etc/alpine-release"

-- | Is the architecture ARM?
isARM :: Bool
isARM = arch == "arm"

-- | Is the OS Mac OS X?
isMacOSX :: Bool
isMacOSX = os == "darwin"

-- | To avoid problems with GHC version mismatch when a new LTS major
-- version is released, pass this argument to @stack@ when running in
-- a global context. The LTS major version here should match that of
-- the main @stack.yaml@ (and ordinarily be the `.0` minor version).
--
-- NOTE: currently using lts-8.22 instead of lts-8.0 because the `cyclic-test-deps` integration test is broken with lts-8.0 because a hackage metadata revision invalidated the snapshot (snapshot has `test-framework-quickcheck2-0.3.0.3` and `QuickCheck-2.9.2`, which used to be fine, but now test-framework-quickcheck2 was revised to have a `QuickCheck < 2.8` constraint).
defaultResolverArg :: String
defaultResolverArg = "--resolver=lts-8.22"

-- | Remove a file and ignore any warnings about missing files.
removeFileIgnore :: FilePath -> IO ()
removeFileIgnore fp = removeFile fp `catch` \e ->
  if isDoesNotExistError e
    then return ()
    else throwIO e
