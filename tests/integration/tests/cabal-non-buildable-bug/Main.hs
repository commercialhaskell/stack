import StackTest

main :: IO ()
main = do
  -- Newer Cabal: dry run and building should succeed, because they'll
  -- both ignore the do-not-build
  writeFile "stack.yaml" "resolver: lts-22.0"
  stack ["build", "--dry-run"]
  stack ["build"]

  -- Older Cabal: both should fail, because they'll both try to
  -- include the non-buildable component. If there's a regression, the
  -- dry run will succeed (because Stack will use the proper logic)
  -- and build will fail (because Cabal will be using its broken
  -- logic).
  writeFile "stack.yaml" "resolver: ghc-7.10.3"
  -- The '--install-ghc' flag is passed here, because IntegrationSpec.runApp
  -- sets up `config.yaml` with `system-ghc: true` and `install-ghc: false`.
  stackErr ["--install-ghc", "build"]
  stackErr ["--install-ghc", "build", "--dry-run"]
