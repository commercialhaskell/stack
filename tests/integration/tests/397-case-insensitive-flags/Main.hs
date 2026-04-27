-- Cabal flags are case-insensitive and Stack treats them as such.
--
-- See: https://github.com/commercialhaskell/stack/issues/397

import StackTest

main :: IO ()
main = do
  stackErr ["build"]
  stack ["build", "--flag", "myPackage:nEcEssAry"]
  stack ["build", "--flag", "*:nEcEssAry"]
