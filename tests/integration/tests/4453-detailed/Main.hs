import StackTest
import System.Directory

main :: IO ()
main = do
    removeDirIgnore ".stack-work"
    stack [defaultResolverArg, "test"]
