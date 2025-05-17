import Control.Monad
import StackTest.Repl

main :: IO ()
main = stackRepl [] $ do
  nextPrompt
  replCommand ":main"
  line <- replGetLine
  let expected = "Hello World!"
  when (line /= expected) $
    error $
         "Main module didn't load correctly.\n"
      <> "Expected: " <> expected <> "\n"
      <> "Actual  : " <> line <> "\n"
