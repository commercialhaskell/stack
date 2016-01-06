import Control.Monad (unless,when)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = do
    stack [defaultResolverArg, "clean"]
    stack [defaultResolverArg, "init", "--force"]
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

buildMessage1 =
    unlines
        [ "Building several executables with the same name: 'also-has-exe-foo:foo', 'has-exe-foo:foo'."
        , "Only one of them will be available via 'stack exec' or locally installed."
        , "Other executables with the same name might be overwritten: 'has-exe-foo-too:foo'."
        ]

buildMessage2 =
    unlines
        [ "Building executable 'has-exe-foo-too:foo'."
        , "Other executables with the same name might be overwritten: 'also-has-exe-foo:foo', 'has-exe-foo:foo'."
        ]
