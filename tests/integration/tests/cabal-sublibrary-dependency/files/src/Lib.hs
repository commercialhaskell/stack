module Lib
  ( someFunc
  ) where

import Subproject.Lib ( libFunc )
import Subproject.SubLib ( subLibFunc )

someFunc :: IO ()
someFunc = do
  libFunc
  subLibFunc
