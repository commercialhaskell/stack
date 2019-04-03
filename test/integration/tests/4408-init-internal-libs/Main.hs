import StackTest

main :: IO ()
main = stack ["init", "--resolver", "ghc-8.2.2", "--force"]
