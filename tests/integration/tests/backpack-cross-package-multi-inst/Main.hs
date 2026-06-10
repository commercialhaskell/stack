import Control.Monad ( unless )
import Data.List ( isInfixOf )
import StackTest

-- Test multiple instantiations: two consumers fill the same sig-pkg with
-- different implementations. Each consumer should get its own CInst task.
main :: IO ()
-- On Windows, this test fails because Cabal-3.12.1.0's copy command cannot cope
-- with long paths. It appears it will be fixed in Cabal >= 3.18. We disable the
-- test on Windows for now.
main = unless isWindows $ do
  stack ["build"]

  stackCheckStdout ["exec", "multi-inst-demo"] $ \out -> do
    unless ("A says: Hello from impl-a" `isInfixOf` out) $
      error $ "Expected 'A says: Hello from impl-a' in output, got: " ++ show out
    unless ("B says: Hello from impl-b" `isInfixOf` out) $
      error $ "Expected 'B says: Hello from impl-b' in output, got: " ++ show out

  -- Rebuild should succeed
  stack ["build"]

  stackCheckStdout ["exec", "multi-inst-demo"] $ \out -> do
    unless ("A says: Hello from impl-a" `isInfixOf` out) $
      error $ "Expected 'A says: Hello from impl-a' after rebuild, got: " ++ show out
    unless ("B says: Hello from impl-b" `isInfixOf` out) $
      error $ "Expected 'B says: Hello from impl-b' after rebuild, got: " ++ show out
