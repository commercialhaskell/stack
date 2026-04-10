import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

-- Test cross-package Backpack with module renaming: sig-pkg has a "Sig"
-- signature, impl-pkg exposes "Impl", and consumer-pkg uses
-- mixins: sig-pkg requires (Sig as Impl) to wire them together.
main :: IO ()
main = do
  stack ["build"]

  stackCheckStdout ["exec", "consumer-demo"] $ \out ->
    unless ("Renamed module works" `isInfixOf` out) $
      error $ "Expected 'Renamed module works' in output, got: " ++ show out
