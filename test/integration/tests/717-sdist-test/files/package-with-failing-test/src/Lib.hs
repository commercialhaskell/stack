module Lib
    ( someFunc
    ) where

import Language.Haskell.TH

someFunc :: IO ()
someFunc = putStrLn "aaa"
