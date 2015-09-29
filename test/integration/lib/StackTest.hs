module StackTest where

import Control.Exception
import System.Environment
import System.FilePath
import System.Directory
import System.IO
import System.Process
import System.Exit

run' :: FilePath -> [String] -> IO ExitCode
run' cmd args = do
    putStrLn $ "Running " ++ cmd ++ " with args " ++ show args
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

doesNotExist :: FilePath -> IO ()
doesNotExist fp = do
    putStrLn $ "doesNotExist " ++ fp
    exists <- doesFileOrDirExist fp
    case exists of
      (Right msg) -> error msg
      (Left _) -> return ()

doesExist :: FilePath -> IO ()
doesExist fp = do
    putStrLn $ "doesExist " ++ fp
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
    putStrLn ("Copy " ++ show src ++ " to " ++ show dest)
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
