import StackTest

main :: IO ()
main = do
    stack ["--version"]
    stack ["--help"]
    stack ["unpack", "acme-missiles-0.2"]
    stack ["unpack", "acme-missiles"]
    stackErr ["command-does-not-exist"]
    stackErr ["unpack", "invalid-package-name-"]
    stackErr ["build"]
    doesNotExist "stack.yaml"

    if isWindows
        then stack [defaultResolverArg, "exec", "./foo.bat"]
        else stack [defaultResolverArg, "exec", "./foo.sh"]
