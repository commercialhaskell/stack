import StackTest

main :: IO ()
main = do
    stack ["unpack", "text-1.2.2.1"]
    stack ["init", defaultResolverArg]
    appendFile "stack.yaml" "\n\nextra-deps:\n- test-framework-quickcheck2-0.3.0.3@sha256:989f988d0c4356d7fc1d87c062904d02eba0637c5adba428b349aeb709d81bc0"
    readFile "stack.yaml" >>= putStrLn
    stack ["test", "--dry-run"]
