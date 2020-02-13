import StackTest

main :: IO ()
main = do
    removeDirIgnore "text-1.2.3.1"
    stack ["unpack", "text-1.2.3.1"]
    stack ["unpack", "QuickCheck-2.10.1"]
    removeFileIgnore "stack.yaml"
    stack ["init", defaultResolverArg]
    stack ["test", "--dry-run"]
