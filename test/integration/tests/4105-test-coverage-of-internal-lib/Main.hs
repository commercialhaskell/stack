import Control.Monad (unless)
import Data.List (isInfixOf, isPrefixOf)
import StackTest

main :: IO ()
main = do
  stack ["clean"]
  stack ["build"]
  res <- getCoverageLines . snd <$> stackStderr ["test", "--coverage", "--color", "never"]
  case res of
    _:exprs:_ -> unless ("2/2" `isInfixOf` exprs) testFail
    _ -> testFail
  where
    testFail = fail "Stack didn't generate coverage from both libraries"

getCoverageLines :: String -> [String]
getCoverageLines = dropWhile (not . isCoverageHeader) . lines
  where
    isCoverageHeader = isPrefixOf "Generating coverage report for "
