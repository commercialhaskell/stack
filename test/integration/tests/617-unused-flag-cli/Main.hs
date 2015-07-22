import StackTest

main :: IO ()
main = do
    stack ["build"]
    stackErr ["build", "--flag", "foo:bar"]
    stackErr ["build", "--flag", "files:bar"]
    stack ["build", "--flag", "*:bar"]
