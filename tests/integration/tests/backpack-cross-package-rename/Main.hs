import Control.Monad ( unless )
import Data.List ( isInfixOf )
import StackTest

-- Test cross-package Backpack with module renaming: sig-pkg has a "Sig"
-- signature, impl-pkg exposes "Impl", and consumer-pkg uses
-- mixins: sig-pkg requires (Sig as Impl) to wire them together.
main :: IO ()
-- On Windows, this test fails because Cabal-3.12.1.0's copy command cannot cope
-- with long paths. It appears it will be fixed in Cabal >= 3.18. We disable the
-- test on Windows for now.
main = unless isWindows $ do
  stack ["build"]

  stackCheckStdout ["exec", "consumer-demo"] $ \out ->
    unless ("Renamed module works" `isInfixOf` out) $
      error $ "Expected 'Renamed module works' in output, got: " ++ show out
