import StackTest

main :: IO ()
main = do
    stack ["init", "--use-solver"]
    stack ["build"]
