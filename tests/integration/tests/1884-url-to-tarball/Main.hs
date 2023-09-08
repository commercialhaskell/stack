import StackTest

main :: IO ()
main = do
    stackCleanFull
    stack ["build", "--dry-run"]
