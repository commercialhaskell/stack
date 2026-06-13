module Consumer (shout) where

import Provider.Utils (capitalize)

shout :: String -> String
shout = map toUpper . capitalize
  where
    toUpper c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise = c
