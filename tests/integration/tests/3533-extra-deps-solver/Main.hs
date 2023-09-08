{--

import StackTest
import System.Directory

main :: IO ()
main = do
    copyFile "orig-stack.yaml" "stack.yaml"
    stack [defaultResolverArg, "solver", "--update-config"]
    stack ["build"]

// --}

main :: IO ()
main = putStrLn "This test is disabled (see https://github.com/commercialhaskell/stack/issues/4410)."
