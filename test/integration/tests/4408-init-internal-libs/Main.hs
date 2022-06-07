import StackTest

main :: IO ()
main = stack ["init", "--resolver", "ghc-9.2.2", "--force"]
