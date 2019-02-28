module Main where

import Files

main = do
  cFiles <- allCFiles
  putStrLn $ "C files:" ++ show cFiles
