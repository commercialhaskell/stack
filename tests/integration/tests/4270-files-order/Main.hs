import Control.Monad
import StackTest
import StackTest.Repl

main :: IO ()
main = do
  stack ["build"]
  stackRepl [] $ do
    nextPrompt
    replCommand "putStrLn greeting"
    line <- replGetLine
    let expected = "Hello, world!"
    when (line /= expected) $
      error $
           "Didn't load correctly.\n"
        <> "Expected: " <> expected <> "\n"
        <> "Actual  : " <> line <> "\n"
