{-# LANGUAGE CPP #-}
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Avoid problems with CPP and HLint
#ifndef __HLINT__

#ifndef FOO
#error FOO isn't defined
#endif

#ifndef BAR
#error BAR isn't defined
#endif

#ifdef BAZ
#error BAZ is defined
#endif

#endif
