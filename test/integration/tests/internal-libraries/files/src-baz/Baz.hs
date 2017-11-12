module Baz where

import Files
import Foo
import Control.Monad.Reader

baz :: IO ()
baz = flip runReaderT () $ lift $ do
  putStrLn "files:"
  print files
  putStrLn "foo"
  foo >>= print
