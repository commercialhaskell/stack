{-# LANGUAGE CPP #-}
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

#if defined(FOO) && defined(BAR)
#error FOO and BAR is defined
#endif
