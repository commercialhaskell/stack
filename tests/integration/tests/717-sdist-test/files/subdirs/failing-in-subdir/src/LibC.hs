{-# LANGUAGE TemplateHaskell #-}

module LibC
    ( someFuncC
    ) where

import           THInSubdir ( thFuncC )

someFuncC :: IO ()
someFuncC = print $(thFuncC)
