-- Stack supports project packages located outside of the project directory.
--
-- See: https://github.com/commercialhaskell/stack/issues/5680

import           StackTest

main :: IO ()
main = do
  withCwd "myPackageA" $ stack ["build"]
  withCwd "myPackageB" $ stack ["build"]
  withCwd "myPackageA" $ stack ["build"]
