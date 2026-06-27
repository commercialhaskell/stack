module PrivateBackpack (greetString) where

import Str (Str, fromString, append, toString)

-- Public API: no Backpack types leak. Str is resolved internally
-- by mix-in linking str-sig's requirement with str-impl's provision.
greetString :: String -> String
greetString name = toString (fromString "Hello, " `append` fromString name `append` fromString "!")
