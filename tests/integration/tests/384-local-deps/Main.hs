import StackTest

main :: IO ()
main = do
    removeFileIgnore "stack.yaml"
    stack ["init", defaultSnapshotArg]
    stack ["test"]
