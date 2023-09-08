import StackTest

main :: IO ()
main = do
    stackEnv <- stackExe
    withCwd "package-a" $ stack ["build"]
    withCwd "package-b" $ stack ["build"]
    withCwd "package-a" $ stack ["build"]
