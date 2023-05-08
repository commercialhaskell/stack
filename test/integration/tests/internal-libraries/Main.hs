import StackTest

main :: IO ()
main = do
  -- The '--install-ghc' flag is passed here, because etc/scripts/release.hs
  -- passes `--no-install-ghc` when `--alpine` is passed to its 'check'
  -- command.
  stack ["--install-ghc", "setup"] -- See stack.yaml; using GHC 9.4.5
  stack ["build"]
