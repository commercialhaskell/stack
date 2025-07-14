import StackTest

main :: IO ()
main = do
    removeDirIgnore "text-2.1.2"
    stack ["unpack", "text-2.1.2"]
    stack ["unpack", "QuickCheck-2.15.0.1"]
    removeFileIgnore "stack.yaml"
    stack ["init", defaultSnapshotArg]
    stack ["test", "--dry-run"]
