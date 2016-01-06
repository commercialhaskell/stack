import StackTest

main :: IO ()
main = do
    stack ["init", defaultResolverArg]
    stack ["test"]
