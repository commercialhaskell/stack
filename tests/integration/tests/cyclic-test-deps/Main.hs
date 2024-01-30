import StackTest

main :: IO ()
main = do
    removeDirIgnore "text-2.0.2"
    stack ["unpack", "text-2.0.2"]
    stack ["unpack", "QuickCheck-2.14.3"]
    removeFileIgnore "stack.yaml"
    stack ["init", defaultSnapshotArg]
    stack ["test", "--dry-run"]
