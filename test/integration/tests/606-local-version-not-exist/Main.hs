import StackTest

main :: IO ()
main = do
    stackErr ["build", "files-3"]
    stackErr ["build", "files-0.1.0.0"]
    stack ["build", "files"]
    stack ["build", "."]
