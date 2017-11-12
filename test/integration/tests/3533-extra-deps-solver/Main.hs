import StackTest
import System.Directory

main :: IO ()
main = do
    copyFile "orig-stack.yaml" "stack.yaml"
    stack ["--resolver", "lts-9.11", "solver", "--update-config"]
    stack ["build"]
