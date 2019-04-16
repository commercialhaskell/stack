{-# LANGUAGE CPP #-}

module Main where

import X
import GHC.Types
import Control.Monad
import Language.Haskell.TH

#include "Test.h"

main :: IO ()
main = putStrLn "Hello, World!"

curryN :: Int -> Q Exp
curryN n = do
  f  <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f:xs)
      ntup = TupE (map VarE xs)
  return $ LamE args (AppE (VarE f) ntup)

data List a = Nil | Cons a (List a)

x = Cons 4 (Cons 2 Nil)

y = TRUE
