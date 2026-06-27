module Main (main) where

import Lib (answer)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main
  | answer == 42 = exitSuccess
  | otherwise    = exitFailure
