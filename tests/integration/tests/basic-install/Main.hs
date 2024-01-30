import StackTest

main :: IO ()
main = do
    stack [defaultSnapshotArg, "install", "acme-missiles-0.3"]
    doesNotExist "stack.yaml"
