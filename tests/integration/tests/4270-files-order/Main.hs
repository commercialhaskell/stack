-- Stack's ghci command can load a project with c-sources, if the package
-- description lists the C source files in dependency order.
--
-- See: https://github.com/commercialhaskell/stack/issues/4270

import           Control.Monad ( when )
import           StackTest
import           StackTest.Repl

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
