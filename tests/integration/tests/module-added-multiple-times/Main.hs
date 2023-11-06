import Control.Monad
import Data.List
import StackTest

main :: IO ()
main = repl [] $ do
  -- The command must be issued before searching the output for the next prompt,
  -- otherwise, on Windows from msys2-20230526, `stack repl` encounters a EOF
  -- and terminates gracefully.
  replCommand ":main"
  nextPrompt
  line <- replGetLine
  let expected = "Hello World!"
  when (line /= expected) $
    error $
         "Main module didn't load correctly.\n"
      <> "Expected: " <> expected <> "\n"
      <> "Actual  : " <> line <> "\n"
