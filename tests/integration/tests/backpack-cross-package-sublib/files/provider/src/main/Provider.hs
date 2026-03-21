module Provider (capitalGreet) where

import Provider.Utils (capitalize)

capitalGreet :: String -> String
capitalGreet name = capitalize ("hello, " ++ name ++ "!")
