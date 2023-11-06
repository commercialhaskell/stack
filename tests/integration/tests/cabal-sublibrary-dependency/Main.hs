import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

-- This tests building two local packages, one of which depends on the other
-- (subproject). The dependency has a library and a visible sub-library named
-- sub, each of which exposes a module that exports a function.

main :: IO ()
-- The '--install-ghc' flag is passed here, because IntegrationSpec.runApp sets
-- up `config.yaml` with `system-ghc: true` and `install-ghc: false`.
-- (See stack.yaml; using GHC 9.4.7.)
main = stackErrStderr ["--install-ghc", "build"] $ \str ->
  let msg = "Sublibrary dependency is not supported, this will almost \
            \certainly fail."
  in  unless (msg `isInfixOf` str) $
        error $ "Expected a warning: \n" ++ show msg
