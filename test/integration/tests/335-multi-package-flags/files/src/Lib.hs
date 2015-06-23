{-# LANGUAGE CPP #-}
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

#if !WORK
#error Not going to work, sorry
#endif
