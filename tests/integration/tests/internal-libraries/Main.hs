import StackTest

main :: IO ()
main = do
  -- The '--install-ghc' flag is passed here, because IntegrationSpec.runApp
  -- sets up `config.yaml` with `system-ghc: true` and `install-ghc: false`.
  stack ["--install-ghc", "setup"] -- See stack.yaml; using GHC 9.4.7
  stack ["build"]
