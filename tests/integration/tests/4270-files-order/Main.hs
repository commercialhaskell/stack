import Control.Monad
import StackTest
import StackTest.Repl

main :: IO ()
main = do
  stack ["build"]
  repl ["--ghci-options=-ignore-dot-ghci"] $ do
    -- The command must be issued before searching the output for the next
    -- prompt, otherwise, on Windows from msys2-20230526, `stack repl`
    -- encounters a EOF and terminates gracefully.
    replCommand "putStrLn greeting"
    nextPrompt
    line <- replGetLine
    let expected = "Hello, world!"
    when (line /= expected) $
      error $
           "Didn't load correctly.\n"
        <> "Expected: " <> expected <> "\n"
        <> "Actual  : " <> line <> "\n"
