import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = do
    -- FIXME: Make 'clean' unnecessary (see #1411)
    stack ["clean"]
    stackCheckStderr ["test", "--coverage"] $ \out -> do
        unless ("The coverage report for multi-test-suite's test-suite multi-test-suite-test is available at" `isInfixOf` out) $
            fail "Didn't get expected report for multi-test-suite-test"
        unless ("[S-6829]" `isInfixOf` out) $
            fail "Didn't get expected empty report for multi-test-suite-test-2"
    -- Test then build works too.
    stack ["clean"]
    stack ["test"]
    stack ["build"]
