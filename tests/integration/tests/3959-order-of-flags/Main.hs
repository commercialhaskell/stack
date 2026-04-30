-- Stack allows build flags or options to be specified before or after the build
-- command.
--
-- See: https://github.com/commercialhaskell/stack/issues/3959

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  checkFlagsBeforeCommand
  checkFlagsAfterCommand

checkFlagsBeforeCommand :: IO ()
checkFlagsBeforeCommand =
  stackCheckStderr ["--test", "--no-run-tests", "build"] checker

checkFlagsAfterCommand :: IO ()
checkFlagsAfterCommand =
  stackCheckStderr ["build", "--test", "--no-run-tests"] checker

checker :: String -> IO ()
checker output = do
  let testsAreDisabled =
        any (\ln -> "All test running disabled by" `isInfixOf` ln) (lines output)
  unless testsAreDisabled $
    fail "Tests should not be run"
