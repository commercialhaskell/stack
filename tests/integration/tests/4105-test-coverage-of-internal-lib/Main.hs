-- Stack generates coverage reports for main libraries and private named
-- libraries (internal libraries).
--
-- See: https://github.com/commercialhaskell/stack/issues/4105

import           Control.Monad ( unless )
import           Data.List ( isInfixOf, isPrefixOf )
import           StackTest

main :: IO ()
main = do
  stack ["clean"]
  stackCheckStdout ["test", "--coverage", "--color", "never"] check

check :: String -> IO ()
check output = case getCoverageLines output of
  _:exprs:_ -> unless ("2/2" `isInfixOf` exprs) testFail
  _ -> testFail
 where
  testFail = fail "Stack didn't generate coverage from both libraries"

getCoverageLines :: String -> [String]
getCoverageLines = dropWhile (not . isCoverageHeader) . lines
 where
  isCoverageHeader = isPrefixOf "Summary coverage report for "
