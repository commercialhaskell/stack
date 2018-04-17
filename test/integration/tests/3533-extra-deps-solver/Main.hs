import StackTest
import System.Directory

main :: IO ()
main = do
    copyFile "orig-stack.yaml" "stack.yaml"
    stack [defaultResolverArg, "solver", "--update-config"]
    stack ["build"]
