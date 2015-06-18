import StackTest

main :: IO ()
main = do
    stack ["build", "--dry-run"] -- for useful error output
    stack ["build"]
