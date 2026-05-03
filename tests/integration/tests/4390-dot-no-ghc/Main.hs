-- Stack's dot command does not require the presence of the specified GHC.
--
-- See: https://github.com/commercialhaskell/stack/issues/4390

import           StackTest

main :: IO ()
main = do
  stack ["ls", "dependencies", "--global-hints"]
  stack ["dot", "--global-hints"]
