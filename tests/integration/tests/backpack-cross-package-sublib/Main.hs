import StackTest

-- Test that a package can depend on another package's public sub-library.
-- consumer depends on provider:utils (not provider's main library).
-- This already works in Stack; this test prevents regressions.
main :: IO ()
main = stack ["build"]
