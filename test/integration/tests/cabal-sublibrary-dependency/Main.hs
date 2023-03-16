import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

-- This tests building two local packages, one of which depends on the other
-- (subproject). The dependency has a library and a visible sub-library named
-- sub, each of which exposes a module that exports a function.

main :: IO ()
-- The '--install-ghc' flag is passed here, because etc/scripts/release.hs
-- passes `--no-install-ghc` when `--alpine` is passed to its 'check' command.
-- (See stack.yaml; using GHC 9.4.4.)
main = stackErrStderr ["build", "--install-ghc"] $ \str ->
  let msg = "Sublibrary dependency is not supported, this will almost \
            \certainly fail."
  in  unless (msg `isInfixOf` str) $
        error $ "Expected a warning: \n" ++ show msg
