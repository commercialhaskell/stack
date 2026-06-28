module SublibDeps.Extended (greetLoud) where

import SublibDeps.Core (greet)

greetLoud :: String -> String
greetLoud name = map toUpper (greet name)
  where
    toUpper c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise = c
