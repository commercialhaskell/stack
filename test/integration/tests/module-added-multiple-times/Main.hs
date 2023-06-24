import Control.Monad
import Data.List
import StackTest

main :: IO ()
main = repl [] $ do
    replCommand ":main"
    line <- replGetLine
    let expected = "Hello World!"
    when (line /= expected) $
      error $
           "Main module didn't load correctly.\n"
        <> "Expected: " <> expected <> "\n"
        <> "Actual  : " <> line <> "\n"
