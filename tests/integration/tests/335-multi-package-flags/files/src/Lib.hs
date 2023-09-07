{-# LANGUAGE CPP #-}
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Avoid problems with CPP and HLint
#ifndef __HLINT__

#if !WORK
#error Not going to work, sorry
#endif

#endif
