import StackTest

main :: IO ()
main = do
    stackErr ["build"]
    stack ["build", "--flag", "new-template:fixIt"]
    stack ["build", "--flag", "new-template:fixit"]
    stack ["build", "--flag", "new-template:fiXit"]
    stack ["build", "--flag", "*:fiXit"]
    stackErr ["build", "--flag", "*:fiXit-else"]
