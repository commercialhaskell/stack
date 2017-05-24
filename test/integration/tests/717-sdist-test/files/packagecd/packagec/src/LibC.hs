{-# LANGUAGE TemplateHaskell #-}
module LibC
    ( someFuncC
    ) where

import THC
import Language.Haskell.TH

someFuncC :: IO ()
someFuncC = print $(thFuncC)
