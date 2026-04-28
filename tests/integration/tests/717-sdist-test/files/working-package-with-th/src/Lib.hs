{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( someFunc
  ) where

import           TH ( thFunc )

someFunc :: IO ()
someFunc = print $(thFunc)
