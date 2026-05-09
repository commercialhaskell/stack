-- | The test's project has project packages A, B and C (which has no library).
--
-- In terms of main libraries, the dependencies are (->- is 'depends on'):
--
--     A ->- B
--
-- In terms of executables (including test suites):
--
--     B ->- A and C ->- A
--
-- As, overall, A ->- B and B ->- A, packages A and B cannot be built
-- 'all-in-one'.
--
-- A, B and C are named myPackageA, myPackageB and myPackageC respectively.
--
-- See:  https://github.com/commercialhaskell/stack/issues/6905

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  stackCheckStderr ["test", "--coverage"] $ \out -> do
    unless ("The coverage report for myPackageA's test-suite test1 is available at" `isInfixOf` out) $
      fail "Didn't get expected report for test1"
    unless ("[S-6829]" `isInfixOf` out) $
      fail "Didn't get expected empty report for test2"
