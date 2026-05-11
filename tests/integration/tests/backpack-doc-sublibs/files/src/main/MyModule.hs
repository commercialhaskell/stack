module MyModule (greeting) where

import Str (Str, empty, append)

greeting :: Str
greeting = empty `append` empty
