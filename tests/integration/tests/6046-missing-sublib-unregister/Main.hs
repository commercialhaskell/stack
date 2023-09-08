import StackTest

-- This tests building a package with a library and an internal sub library,
-- where the library depends on the sub library, first version 0.1.0.0 (the
-- Cabal file is @foo.cabal1@) and then version 0.2.0.0 (the Cabal file is
-- @foo.cabal2@).
main :: IO ()
main = do
  copy "foo.cabal1" "foo.cabal"
  stack ["build"]
  copy "foo.cabal2" "foo.cabal"
  stack ["build"]
