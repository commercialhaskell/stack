import StackTest

main :: IO ()
main = stack ["init", "--resolver", "ghc-8.10.4", "--force"]
