-- Stack can be commanded to skip the building of specific package components.
--
-- See: https://github.com/commercialhaskell/stack/issues/1659

import           StackTest

main :: IO ()
main = do
  stack ["build", "--test", "--bench", "--skip", "test-failing", "--skip", "bench-failing", "--skip", "myExe-failing"]
  stack ["build", ":test-failing", ":bench-failing", ":myExe", ":myExe-failing", "--skip", "test-failing", "--skip", "bench-failing", "--skip", "myExe-failing"]
