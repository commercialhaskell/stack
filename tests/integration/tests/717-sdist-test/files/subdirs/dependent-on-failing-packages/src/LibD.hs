module LibD
  ( someFuncD
  ) where

import           Lib ( someFunc )
import           LibC ( someFuncC )

someFuncD :: IO ()
someFuncD = do
  someFunc
  someFuncC
