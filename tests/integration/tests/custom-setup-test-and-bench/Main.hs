import StackTest

-- Regression test for PR #6865. A Custom-setup package with both a test
-- suite and a benchmark must have both components built when
-- `stack build --test --bench` is commanded.
main :: IO ()
main = do
  -- Drop the no-run flags: with the bug, the run step surfaces the missing
  -- test/bench exes. Without the no-run flags, Stack will build the lib,
  -- skip the test/bench builds (bug), and then fail when it tries to run
  -- non-existent exes.
  stack ["build", "--test", "--bench"]
