module Provider.Utils (capitalize) where

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs)
  | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32) : cs
  | otherwise = c : cs
