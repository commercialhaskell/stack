-- Stack can be commanded to skip the building of specific package components.
--
-- See: https://github.com/commercialhaskell/stack/issues/1659

import           StackTest

main :: IO ()
main = do
  stack ["build", "--test", "--bench", "--skip", "failing-test", "--skip", "failing-bench", "--skip", "myPackage-failing"]
  stack ["build", ":failing-test", ":failing-bench", ":myPackage", ":myPackage-failing", "--skip", "failing-test", "--skip", "failing-bench", "--skip", "myPackage-failing"]
