-- |
-- The integration tests have no ghc present, initially. Stack should not
-- require ghc present to run the `clean` command.

import StackTest

main :: IO ()
main = do
  -- `stack clean` should succeed even though there is no ghc available.
  -- See the stack.yaml file for how this works.
  stackIgnoreException ["clean"]
  stackCleanFull
