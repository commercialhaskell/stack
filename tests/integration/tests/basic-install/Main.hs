import StackTest

main :: IO ()
main = do
    stack [defaultResolverArg, "install", "acme-missiles-0.3"]
    doesNotExist "stack.yaml"
