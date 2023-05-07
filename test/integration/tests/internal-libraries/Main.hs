import Control.Monad ( unless )
import StackTest

main :: IO ()
-- See https://github.com/haskell/cabal/issues/7763. On macOS (Apple silicon),
-- fails with Cabal error:
-- "Cabal-simple_SvXsv1f__3.2.1.0_ghc-8.10.7: Cannot build some foreign libraries:
-- Building foreign libraries is currently only supported on OSX, Linux and
-- Windows"
main = unless ( isAarch64 && isMacOSX ) $ do
  -- The '--install-ghc' flag is passed here, because etc/scripts/release.hs
  -- passes `--no-install-ghc` when `--alpine` is passed to its 'check'
  -- command.
  stack ["--install-ghc", "setup"] -- See stack.yaml; using GHC 8.10.7
  stack ["build"]
