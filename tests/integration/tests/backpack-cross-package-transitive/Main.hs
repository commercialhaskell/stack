import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

-- Test transitive Backpack chains: logger-sig (indefinite, sig: Logger) depends
-- on str-sig (indefinite, sig: Str). When consumer mixes in logger-sig, both
-- Logger and Str holes must be filled transitively.
main :: IO ()
main = do
  -- Build all four packages. This exercises:
  -- 1. str-sig CLib (indefinite, typecheck-only)
  -- 2. impl-pkg CLib (concrete Str + Logger)
  -- 3. str-sig CInst (instantiation with impl-pkg's Str)
  -- 4. logger-sig CLib (indefinite, typecheck-only, inherits Str hole)
  -- 5. logger-sig CInst (fills BOTH Logger and Str holes)
  -- 6. consumer-pkg CLib + CExe
  stack ["build"]

  -- Verify the consumer executable calls through the transitive chain
  stackCheckStdout ["exec", "consumer-demo"] $ \out ->
    unless ("[LOG] Hello from transitive chain" `isInfixOf` out) $
      error $ "Expected '[LOG] Hello from transitive chain' in output, got: "
            ++ show out

  -- Rebuild should succeed (no stale CInst state)
  stack ["build"]

  -- Verify output still correct after rebuild
  stackCheckStdout ["exec", "consumer-demo"] $ \out ->
    unless ("[LOG] Hello from transitive chain" `isInfixOf` out) $
      error $ "Expected '[LOG] Hello from transitive chain' after rebuild, got: "
            ++ show out
