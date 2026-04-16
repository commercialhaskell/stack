import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = do
  -- Clean to ensure a full rebuild so we can inspect build output.
  stack ["clean"]

  -- Build all targets and verify per-component build messages appear in
  -- stderr (Stack prints build progress there).
  stackCheckStderr ["build"] $ \err -> do
    -- With per-component builds, each executable should be built separately
    -- and announced with its component name.
    let expect tag =
          unless (tag `isInfixOf` err) $
            error $ "Expected " ++ show tag ++ " in build output:\n" ++ err
    expect "exe:app1"
    expect "exe:app2"

  -- Verify both executables produce correct output.
  stackCheckStdout ["exec", "app1"] $ \out ->
    unless ("app1: Hello from Lib" `isInfixOf` out) $
      error $ "Unexpected app1 output: " ++ out

  stackCheckStdout ["exec", "app2"] $ \out ->
    unless ("app2: Hello from Lib" `isInfixOf` out) $
      error $ "Unexpected app2 output: " ++ out

  -- Run the test suite and verify it passes.
  stackCheckStderr ["test"] $ \err ->
    unless ("per-component-build" `isInfixOf` err) $
      error $ "Expected package name in test output:\n" ++ err
