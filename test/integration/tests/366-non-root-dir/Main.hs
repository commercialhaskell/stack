import StackTest
import System.Directory

main :: IO ()
main = do
    removeDirIgnore ".stack-work"
    stackErr ["exec", "hello-world"]
    setCurrentDirectory "app"
    stack ["build"]
    stack ["exec", "hello-world"]
