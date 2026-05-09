-- | The test's project has project packages A and B.
--
-- In terms of main libraries, the dependencies are (->- is 'depends on'):
--
--     A ->- B
--
-- In terms of executables (a test suite):
--
--     B ->- A
--
-- As, overall, A ->- B and B ->- A, packages A and B cannot be built
-- 'all-in-one'.
--
-- This integration test tests:
--
-- * when A is named myPackageA and B is named myPackageB; and
--
-- * when A is named myPackageD and B is named myPackageC.
--
-- See: https://github.com/commercialhaskell/stack/issues/6905

import           StackTest

main :: IO ()
main = do
  stack ["--stack-yaml", "stack1.yaml", "test"]
  stack ["--stack-yaml", "stack2.yaml", "test"]
