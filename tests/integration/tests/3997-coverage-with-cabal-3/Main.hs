-- Stack can create a coverage report for a test suite.
--
-- See: https://github.com/commercialhaskell/stack/issues/3997

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  stackCheckStderr ["test", "--coverage"] $ \out -> do
    unless ("The coverage report for myPackage's test-suite test is available at" `isInfixOf` out) $
      fail "Coverage report didn't build"
