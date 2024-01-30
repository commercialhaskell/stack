import StackTest

main :: IO ()
main = stack ["init", "--snapshot", "ghc-9.2.4", "--force"]
