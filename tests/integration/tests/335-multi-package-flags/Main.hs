-- Stack should be able to specify Cabal flags for all packages that have a
-- Cabal flag of the same name.
--
-- See: https://github.com/commercialhaskell/stack/issues/335

import           StackTest

main :: IO ()
main = do
  stackErr ["build"]
  stack ["build", "--flag", "myPackage:necessary"]
  stackErr ["build"]
  stack ["build", "--flag", "*:necessary"]
