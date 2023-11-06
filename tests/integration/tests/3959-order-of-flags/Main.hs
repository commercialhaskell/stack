import StackTest

import Control.Monad (unless)
import Data.List (isInfixOf)

-- Integration test for https://github.com/commercialhaskell/stack/issues/3959
main :: IO ()
main = do
  checkFlagsBeforeCommand
  checkFlagsAfterCommand

checkFlagsBeforeCommand :: IO ()
checkFlagsBeforeCommand = stackCheckStderr ["--test", "--no-run-tests", "build"] checker

checkFlagsAfterCommand :: IO ()
checkFlagsAfterCommand = stackCheckStderr ["build", "--test", "--no-run-tests"] checker

checker :: String -> IO ()
checker output = do
  let testsAreDisabled = any (\ln -> "Test running disabled by" `isInfixOf` ln) (lines output)
  unless testsAreDisabled $ fail "Tests should not be run"
