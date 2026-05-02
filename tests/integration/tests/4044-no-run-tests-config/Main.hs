-- Stack can be configured not to run test suites.
--
-- See: https://github.com/commercialhaskell/stack/issues/4044

import           StackTest

main :: IO ()
main = do
  stack ["test"]
  stack ["build", "myPackage:test:test"]
