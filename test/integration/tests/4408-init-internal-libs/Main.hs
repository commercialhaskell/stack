import StackTest

main :: IO ()
main = stack ["init", "--resolver", "ghc-8.6.5", "--force"]
