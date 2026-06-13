import StackTest

-- Regression test for PR #6865. Mirrors the `stack new` default project
-- layout (library + executable + test) with a `custom-setup` stanza added.
-- `stack test` must build and run the test suite.
main :: IO ()
main = do
  stack ["test"]
