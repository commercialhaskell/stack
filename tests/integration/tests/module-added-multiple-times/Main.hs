-- | Stack can load a package into GHC's repl.

import           Control.Monad ( when )
import           StackTest.Repl

main :: IO ()
main = stackRepl [] $ do
  nextPrompt
  replCommand ":main"
  line <- replGetLine
  let expected = "OK"
  when (line /= expected) $
    error $
         "Main module didn't load correctly.\n"
      <> "Expected: " <> expected <> "\n"
      <> "Actual  : " <> line <> "\n"
