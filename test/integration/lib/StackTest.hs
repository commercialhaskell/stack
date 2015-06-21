module StackTest where

import Control.Exception
import System.Environment
import System.FilePath
import System.Directory
import System.IO
import System.Process
import System.Exit
import System.Environment

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
    isFile <- doesFileExist fp
    if isFile
        then error $ "File exists: " ++ fp
        else do
            isDir <- doesDirectoryExist fp
            if isDir
                then error $ "Directory exists: " ++ fp
                else return ()
