import StackTest

main :: IO ()
main = stack ["init", "--resolver", "ghc-9.0.2", "--force"]
