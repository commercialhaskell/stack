import Control.Monad (when)
import Data.List (isInfixOf)
import StackTest

-- Regression guard for the no-finals path of PR #6865's fix. A Custom-setup
-- package with only a library (no test suites, no benchmarks) should take
-- the plain non-split primary-only path: configure+build+copy+register,
-- exactly as before the fix. Second `stack build` must be a no-op.
main :: IO ()
main = do
  stack ["build"]
  stackCheckStderr ["build"] $ \err ->
    let signals =
          [ "] Compiling"
          , "Installing library"
          , "Registering library"
          ]
    in  when (any (`isInfixOf` err) signals) $
          error $
            "Second `stack build` was not a no-op. Stderr:\n" ++ err
