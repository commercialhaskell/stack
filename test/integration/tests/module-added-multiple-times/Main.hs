

import Control.Monad
import Data.List
import StackTest

main :: IO ()
main = repl [] $ do
    replCommand ":main"
    line <- replGetLine
    when (line /= "Hello World!")
        $ error "Main module didn't load correctly."
