module Str (Str, empty, append, toString, fromString) where

-- Concrete implementation of the Str signature using plain String.
type Str = String

empty :: Str
empty = ""

append :: Str -> Str -> Str
append = (++)

toString :: Str -> String
toString = id

fromString :: String -> Str
fromString = id
