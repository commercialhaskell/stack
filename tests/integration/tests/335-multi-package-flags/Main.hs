import StackTest

main :: IO ()
main = do
    stackErr ["build"]
    stack ["build", "--flag", "new-template:necessary"]
    stackErr ["build"]
    stack ["build", "--flag", "*:necessary"]
