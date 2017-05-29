{-# LANGUAGE TemplateHaskell #-}
module LibC
    ( someFuncC
    ) where

import THInSubdir
import Language.Haskell.TH

someFuncC :: IO ()
someFuncC = print $(thFuncC)
