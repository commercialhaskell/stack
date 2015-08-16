{-# LANGUAGE CPP #-}
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

#ifndef FOO
#error FOO isn't defined
#endif

#ifndef BAR
#error BAR isn't defined
#endif

#ifdef BAZ
#error BAZ is defined
#endif
