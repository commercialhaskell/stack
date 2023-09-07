module Lib
    ( someFunc
    ) where

import Control.Monad.Reader ()

someFunc :: IO ()
someFunc = putStrLn "someFunc"
