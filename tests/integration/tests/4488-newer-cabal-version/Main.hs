-- Stack can build with a version of Cabal (the library) other than that which
-- is a boot package of the specified GHC version.
--
-- See: https://github.com/commercialhaskell/stack/issues/4488

import           StackTest

main :: IO ()
main = do
  stackErr ["--stack-yaml", "stack-bad.yaml", "build", "--dry-run"]
  stack ["--stack-yaml", "stack-good.yaml", "build", "--dry-run"]
