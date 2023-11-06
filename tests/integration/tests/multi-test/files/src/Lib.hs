module Lib
    ( someFunc
    , someOtherFunc
    , cyclicOutput
    ) where

import Cyclic

someFunc :: IO ()
someFunc = putStrLn "someFunc"

someOtherFunc :: IO ()
someOtherFunc = putStrLn "someOtherFunc"

cyclicOutput :: IO ()
cyclicOutput = putStrLn cyclic
