module StackTest where

import Control.Exception
import System.Environment
import System.FilePath
import System.Directory
import System.IO
import System.Process
import System.Exit
import System.Environment

stack' :: [String] -> IO ExitCode
stack' args = do
    stack <- getEnv "STACK_EXE"
    putStrLn $ "Running stack with args " ++ show args
    (Nothing, Nothing, Nothing, ph) <- createProcess (proc stack args)
    waitForProcess ph

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
