{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( someFunc
    ) where

import           TH ( thFunc )
import           Language.Haskell.TH ()

someFunc :: IO ()
someFunc = print $(thFunc)
