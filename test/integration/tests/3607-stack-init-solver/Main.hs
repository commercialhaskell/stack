import StackTest

main :: IO ()
main = do
  stack ["init", "--resolver", "nightly-2018-03-12", "--solver", "--force"]
  stack ["build"]
