import Control.Monad ( unless )
import Data.List ( isInfixOf )
import StackTest

-- Test cross-package Backpack: sig-pkg (indefinite, has Str signature) is
-- instantiated with impl-pkg's Str module via consumer-pkg's mixin declaration.
main :: IO ()
-- On Windows, this test fails because Cabal-3.12.1.0's copy command cannot cope
-- with long paths. It appears it will be fixed in Cabal >= 3.18. We disable the
-- test on Windows for now.
main = unless isWindows $ do
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
