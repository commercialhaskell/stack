module Main where

import Lib (greeting)
import System.Exit (exitSuccess)

main :: IO ()
main = do
  putStrLn ("test: " ++ greeting)
  exitSuccess
