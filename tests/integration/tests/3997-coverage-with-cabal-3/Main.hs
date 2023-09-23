import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = do
    stack ["setup"]
    stackCheckStderr ["test", "--coverage"] $ \out -> do
        unless ("The coverage report for foo's test-suite foo-test is available at" `isInfixOf` out) $
            fail "Coverage report didn't build"
