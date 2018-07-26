import StackTest

main :: IO ()
main = do
  stack ["init", "--resolver", "nightly-2018-07-31", "--solver", "--force"]
  stack ["build"]
