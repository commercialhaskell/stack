module Baz where

import Files
import Foo

baz :: IO ()
baz = do
  putStrLn "files:"
  print files
  putStrLn "foo"
  foo >>= print
