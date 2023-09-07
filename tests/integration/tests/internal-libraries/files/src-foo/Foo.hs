module Foo where

import Control.Monad.STM
import Files

foo :: IO String
foo = atomically $ pure $ "foo using " ++ files
