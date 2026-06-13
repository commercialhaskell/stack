import Control.Monad (when)
import Data.List (isInfixOf)
import StackTest

-- Regression test for PR #6865's third-run observation: after a successful
-- `stack test`, a repeat `stack test` with no source changes should be a
-- no-op (no re-compilation). The reporter observed Stack rebuilding the
-- library on every other run because the enable-tests configure flag
-- flip-flops between primary and final tasks keyed at the same CLib.
main :: IO ()
main = do
  stack ["test"]
  -- Second run must not redo actual work. We don't assert on
  -- "Preprocessing"/"Building library" — Cabal prints those even when
  -- GHC skips compilation — but we do assert no real compilation
  -- (`[N of M] Compiling`) and no reinstall (`Installing library`,
  -- `Registering library`).
  stackCheckStderr ["test"] $ \err ->
    let signals =
          [ "] Compiling"
          , "Installing library"
          , "Registering library"
          ]
    in  when (any (`isInfixOf` err) signals) $
          error $
            "Second `stack test` was not a no-op. Stderr:\n" ++ err
