-- | Stack builds a package which depends directly on a package with the same
-- name as a sublibrary or foreign library of that package.
--
-- See: https://github.com/commercialhaskell/stack/issues/6920

import           StackTest

main :: IO ()
main = do
  stack ["build", "myPackageA"]
  stack ["build", "myPackageB"]
