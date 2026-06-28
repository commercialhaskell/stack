import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = do
  stack ["clean"]

  stackCheckStderr ["build"] $ \err -> do
    let expect tag =
          unless (tag `isInfixOf` err) $
            error $ "Expected " ++ show tag ++ " in build output:\n" ++ err
    expect "Building executable 'app1'"
    expect "Building executable 'app2'"

  stackCheckStdout ["exec", "app1"] $ \out ->
    unless ("app1: Hello from Lib" `isInfixOf` out) $
      error $ "Unexpected app1 output: " ++ out

  stackCheckStdout ["exec", "app2"] $ \out ->
    unless ("app2: Hello from Lib" `isInfixOf` out) $
      error $ "Unexpected app2 output: " ++ out

  stackCheckStderr ["test"] $ \err ->
    unless ("per-component-build" `isInfixOf` err) $
      error $ "Expected package name in test output:\n" ++ err
