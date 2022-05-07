import StackTest

main :: IO ()
main = do
    removeDirIgnore "text-2.0"
    stack ["unpack", "text-2.0"]
    stack ["unpack", "QuickCheck-2.14.2"]
    removeFileIgnore "stack.yaml"
    stack ["init", defaultResolverArg]
    stack ["test", "--dry-run"]
