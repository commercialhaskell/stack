import StackTest

main :: IO ()
main = do
  -- Newer Cabal: dry run and building should succeed, because they'll
  -- both ignore the do-not-build
  writeFile "stack.yaml" "resolver: ghc-9.2.2"
  stack ["build", "--dry-run"]
  stack ["build"]

  -- Older Cabal: both should fail, because they'll both try to
  -- include the non-buildable component. If there's a regression, the
  -- dry run will succeed (because Stack will use the proper logic)
  -- and build will fail (because Cabal will be using its broken
  -- logic).
  writeFile "stack.yaml" "resolver: ghc-7.10.3"
  stackErr ["build"]
  stackErr ["build", "--dry-run"]
