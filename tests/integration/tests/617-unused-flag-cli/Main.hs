-- Stack reports an error when a Cabal flag is specified on the command line for
-- a specific non-existent package or a non-existent Cabal flag is specified for
-- a specific project package.
--
-- See: https://github.com/commercialhaskell/stack/issues/617

import StackTest

main :: IO ()
main = do
  stackErr ["build", "--flag", "noSuchPackage:my-flag"]
  stackErr ["build", "--flag", "myPackage:no-such-flag"]
  stack ["build", "--flag", "*:no-such-flag"]
