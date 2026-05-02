module Str (Str, empty, append) where

import Data.Text (Text)
import qualified Data.Text as T

type Str = Text

empty :: Str
empty = T.empty

append :: Str -> Str -> Str
append = T.append
