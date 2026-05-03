-- Stack builds a package where the main library depends on a private named
-- lirary (an internal library).
--
-- See: https://github.com/commercialhaskell/stack/issues/6046

import StackTest

-- This tests building a package with a library and an internal sub library,
-- where the library depends on the sub library, first version 0.1.0.0 and then
-- version 0.2.0.0.
main :: IO ()
main = do
  copy "package1.yaml" "package.yaml"
  stack ["build"]
  copy "package2.yaml" "package.yaml"
  stack ["build"]
