import StackTest

main :: IO ()
main = do
  stack ["init", "--solver", "--resolver", "ghc-8.2.1"]
  stack ["solver", "--update-config"]
