{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( someFunc
    ) where

import TH
import Language.Haskell.TH

someFunc :: IO ()
someFunc = print $(thFunc)
