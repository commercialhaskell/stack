-- | When building, Stack ignores the components of packages that are
-- not-buildable.

import           StackTest

main :: IO ()
main = do
  stack ["build", "--dry-run"]
  stack ["build"]
