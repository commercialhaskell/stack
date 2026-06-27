import StackTest

-- Regression test for PR #6865. A package with `build-type: Custom` and a
-- benchmark should have its benchmark component built when `stack bench`
-- (or `stack build --bench`) is commanded. Same root cause as the test-suite
-- bug: the non-split final key collapses with the primary build's CLib key.
main :: IO ()
main = do
  -- `stack bench` both builds and runs the benchmark; if the non-split final
  -- action drops the bench target (the PR bug), Stack reports
  -- BenchmarkExeMissing instead of finding a freshly built exe.
  stack ["bench"]
