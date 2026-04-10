import StackTest

-- Test that a package with chained sub-library dependencies builds correctly.
-- lib:core -> lib:extended -> lib (main) -> exe:sublib-deps-demo
-- This already works in Stack; this test prevents regressions.
main :: IO ()
main = stack ["build"]
