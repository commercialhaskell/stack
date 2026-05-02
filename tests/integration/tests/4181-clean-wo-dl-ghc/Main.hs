-- Stack's clean command should not require the presence of the specified
-- version of GHC.
--
-- See: https://github.com/commercialhaskell/stack/issues/4181

import StackTest

main :: IO ()
main = do
  -- `stack clean` should succeed even though there is no ghc available.
  -- See the stack.yaml file for how this works.
  stackIgnoreException ["clean"]
  stackCleanFull
