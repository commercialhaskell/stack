module StackTest where

import Control.Exception
import Data.List (intercalate)
import System.Environment
import System.FilePath
import System.Directory
import System.IO
import System.Process
import System.Exit
import System.Info (os)

run' :: FilePath -> [String] -> IO ExitCode
run' cmd args = do
    logInfo $ "Running: " ++ cmd ++ " " ++ intercalate " " (map showProcessArgDebug args)
    (Nothing, Nothing, Nothing, ph) <- createProcess (proc cmd args)
    waitForProcess ph

run :: FilePath -> [String] -> IO ()
run cmd args = do
    ec <- run' cmd args
    if ec == ExitSuccess
        then return ()
        else error $ "Exited with exit code: " ++ show ec

stack' :: [String] -> IO ExitCode
stack' args = do
    stack <- getEnv "STACK_EXE"
    run' stack args

stack :: [String] -> IO ()
stack args = do
    ec <- stack' args
    if ec == ExitSuccess
        then return ()
        else error $ "Exited with exit code: " ++ show ec

stackErr :: [String] -> IO ()
stackErr args = do
    ec <- stack' args
    if ec == ExitSuccess
        then error "stack was supposed to fail, but didn't"
        else return ()

-- | Run stack with arguments and apply a check to the resulting
-- stderr output if the process succeeded.
stackCheckStderr :: [String] -> (String -> IO ()) -> IO ()
stackCheckStderr args check = do
    stack <- getEnv "STACK_EXE"
    logInfo $ "Running: " ++ stack ++ " " ++ intercalate " " (map showProcessArgDebug args)
    (ec, _, err) <- readProcessWithExitCode stack args ""
    hPutStr stderr err
    if ec /= ExitSuccess
        then error $ "Exited with exit code: " ++ show ec
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
      (Right msg) -> return ()
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
    if f1Contents == f2Contents
          then return ()
          else error
                   ("contents do not match for " ++
                    show f1 ++
                    " " ++
                    show f2)

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
exeExt = if isWindows then ".exe" else ""

-- | Is the OS Windows?
isWindows = os == "mingw32"

-- | To avoid problems with GHC version mismatch when a new LTS major
-- version is released, pass this argument to @stack@ when running in
-- a global context.  The LTS major version here should match that of
-- the main @stack.yaml@.
defaultResolverArg = "--resolver=lts-4"
