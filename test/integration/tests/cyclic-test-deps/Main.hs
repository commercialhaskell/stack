import StackTest

main :: IO ()
main = do
    stack ["unpack", "text-1.2.2.1"]
    stack ["init", defaultResolverArg]
    stack ["test", "--dry-run"]
