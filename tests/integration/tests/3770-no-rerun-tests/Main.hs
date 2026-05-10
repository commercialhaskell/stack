-- | Stack can avoid re-running successful test suites.
--
-- See: https://github.com/commercialhaskell/stack/pull/3770

import          Control.Monad ( unless )
import          Data.List ( isInfixOf )
import          StackTest

main :: IO ()
main = do
  stackCheckStderr ["test"] (expectMessage testSuitePassed)
  stackCheckStderr
    ["test", "--no-rerun-tests"]
    (expectMessage skippedAlreadyPassedTest)

testSuitePassed :: String
testSuitePassed = "Test suite test passed"

skippedAlreadyPassedTest :: String
skippedAlreadyPassedTest = "skipping already passed test"

expectMessage :: String -> String -> IO ()
expectMessage msg stderr = do
  unless (words msg `isInfixOf` words stderr) $
    error $ "Expected output: \n" ++ show msg
