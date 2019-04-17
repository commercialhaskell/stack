{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           GHC.Types
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax
import           X

#include "Test.h"

main :: IO ()
main = putStrLn "Hello, World!"

f :: String
f = $(let readme = "README.md"
      in qAddDependentFile readme *> (stringE =<< qRunIO (readFile readme)))
