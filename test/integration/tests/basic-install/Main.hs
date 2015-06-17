import StackTest

main :: IO ()
main = do
    stack ["install", "acme-missiles-0.3"]
    doesNotExist "stack.yaml"
