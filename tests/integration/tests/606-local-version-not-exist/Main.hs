-- Stack reports an error when the target is a project package version,
-- whether the version is correct or not.
--
-- See: https://github.com/commercialhaskell/stack/issues/606

import StackTest

main :: IO ()
main = do
  stackErr ["build", "myPackage-1"]
  stackErr ["build", "myPackage-0.1.0.0"]
  stack ["build", "myPackage"]
