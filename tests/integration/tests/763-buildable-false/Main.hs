import StackTest

main :: IO ()
main = do
    stack ["build"]
    stack ["build", "--flag", "*:force-enable"]
    stack ["build", ":enabled"]
    stackErr ["build", ":disabled"]
    stack ["build", ":disabled", "--flag", "files:force-enable"]
