import StackTest

-- Regression test for PR #6865. A package with `build-type: Custom`
-- (triggered by a `custom-setup` stanza) and a test suite should build the
-- test suite when `stack test` is commanded. The bug: the test suite is
-- never built, and `stack test` fails with TestSuiteExeMissing.
main :: IO ()
main = do
  stack ["test"]
