module Main where

import Files
import Foo

main :: IO ()
main = do
  putStrLn "files:"
  print files
  putStrLn "foo"
  foo >>= print
