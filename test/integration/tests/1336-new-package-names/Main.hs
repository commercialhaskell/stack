import StackTest
import System.Directory
import System.FilePath

main :: IO ()
main = do
    stack ["new", "1234a-4b-b4-abc-12b34"]
    doesExist "./1234a-4b-b4-abc-12b34/stack.yaml"
    stackErr ["new", "1234-abc"]
    doesNotExist "./1234-abc/stack.yaml"
    doesNotExist "./1234-abc"
    stackErr ["new", "1-abc"]
    stackErr ["new", "44444444444444"]
    stackErr ["new", "abc-1"]
