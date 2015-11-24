import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = do
    stack ["build"]
    stack ["test"]
    -- FIXME: Make fforce-recomp unnecessary (see #1411)
    stackCheckStderr ["test", "--coverage", "--ghc-options", "-fforce-recomp"] $ \out -> do
        unless ("The coverage report for multi-test-suite's test-suite \"multi-test-suite-test\" is available at" `isInfixOf` out) $
            fail "Didn't get expected report for multi-test-suite-test"
        unless ("Error: The coverage report for multi-test-suite's test-suite \"multi-test-suite-test-2\" did not consider any code." `isInfixOf` out) $
            fail "Didn't get expected empty report for multi-test-suite-test-2"
    -- Test then build works too.
    stack ["clean"]
    stack ["test"]
    stack ["build"]
