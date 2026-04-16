import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

-- Test cross-package Backpack: sig-pkg (indefinite, has Str signature) is
-- instantiated with impl-pkg's Str module via consumer-pkg's mixin declaration.
main :: IO ()
main = do
  -- Build all three packages. This exercises:
  -- 1. sig-pkg CLib (indefinite, typecheck-only)
  -- 2. impl-pkg CLib (concrete Str implementation)
  -- 3. sig-pkg CInst (instantiation with impl-pkg's Str)
  -- 4. consumer-pkg CLib + CExe
  stack ["build"]

  -- Verify the consumer executable calls through the instantiated signature
  stackCheckStdout ["exec", "consumer-demo"] $ \out ->
    unless ("Hello from impl-pkg" `isInfixOf` out) $
      error $ "Expected 'Hello from impl-pkg' in output, got: " ++ show out

  -- Rebuild should succeed (no stale CInst state or dist-dir conflicts)
  stack ["build"]

  -- Verify output still correct after rebuild
  stackCheckStdout ["exec", "consumer-demo"] $ \out ->
    unless ("Hello from impl-pkg" `isInfixOf` out) $
      error $ "Expected 'Hello from impl-pkg' after rebuild, got: " ++ show out
