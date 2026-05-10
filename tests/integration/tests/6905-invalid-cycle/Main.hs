-- | The test's project has project packages A, B, C and D.
--
-- In terms of main libraries, the dependencies are (->- is 'depends on'):
--
--     A ->- B and C ->- D, D ->- C (a cycle)
--
-- In terms of executables (a test suite):
--
--     B ->- A, B ->- C
--
-- As, overall, A ->- B and B ->- A, packages A and B cannot be built
-- 'all-in-one'. However, if the test suite of B is not being built, A and B can
-- be built.

-- The test suite of B cannot be built, because C ->- D and D ->- C.
--
-- See: https://github.com/commercialhaskell/stack/issues/6905

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  stack ["build", "myPackageA", "myPackageB"]
  stackErrStderr ["test", "myPackageB"] (expectMessage dependencyCycleDetected)

dependencyCycleDetected :: String
dependencyCycleDetected =
  "myPackageC dependency cycle detected: myPackageC, myPackageD, myPackageC"

expectMessage :: String -> String -> IO ()
expectMessage msg stderr =
  unless (words msg `isInfixOf` words stderr)
    (error $ "Expected a warning: \n" ++ show msg)
