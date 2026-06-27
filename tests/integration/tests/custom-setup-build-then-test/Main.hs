import StackTest

-- Regression test for the build-then-test workflow on Custom-setup packages.
-- `stack build` configures without --enable-tests; a subsequent `stack test`
-- must then reconfigure with --enable-tests and still build the test suite
-- correctly. Before PR #6865's TestSuiteExeMissing fix, the second step
-- failed with `TestSuiteExeMissing` because the final task shared a
-- ComponentKey with the primary and was dropped.
main :: IO ()
main = do
  stack ["build"]
  stack ["test"]
