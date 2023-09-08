import StackTest

main :: IO ()
main = do
    removeFileIgnore "stack.yaml"
    stack ["init", defaultResolverArg]
    stack ["test"]
