import Control.Monad (unless,when)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = do
    stack [defaultSnapshotArg, "clean"]
    stack [defaultSnapshotArg, "init", "--force"]
    stackCheckStderr
        ["build", "also-has-exe-foo", "has-exe-foo"]
        (expectMessage buildMessage1)
    stackCheckStderr
        ["build", "has-exe-foo-too"]
        (expectMessage buildMessage2)

expectMessage :: String -> String -> IO ()
expectMessage msg stderr =
    unless (msg `isInfixOf` stderr)
        (error $ "Expected a warning: \n" ++ show msg)

-- Use short message fragment because prettyWarn formatting and colour
buildMessage1 =
    "Building several executables with the same name:"

-- Use short message fragment because prettyWarn formatting and colour
buildMessage2 =
    "Other executables with the same name"
