-- Stack reports an error if commanded to build a specific component of a
-- package that is not buildable.
--
-- https://github.com/commercialhaskell/stack/issues/763

import StackTest

main :: IO ()
main = do
  stack ["build"]
  stack ["build", ":myPackage", "--flag", "myPackage:buildable"]
  stackErr ["build", ":myPackage"]
