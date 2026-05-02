--
-- See: https://github.com/commercialhaskell/stack/issues/4783

import           StackTest

main :: IO ()
main = do
  stack ["build", "myPackageB"]
  stack ["test"]
