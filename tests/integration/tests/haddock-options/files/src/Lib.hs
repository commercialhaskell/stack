{-# LANGUAGE CPP #-}

module Lib
  ( someFunc
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

#if defined(VARIABLE_A) && defined(VARIABLE_B)

#error VARIABLE_A and VARIABLE_B is defined

#endif
