module LibD
    ( someFunc
    ) where

import Lib
import LibC

someFuncD :: IO ()
someFuncD = do
  someFunc
  someFuncC
