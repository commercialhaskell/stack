import StackTest

main :: IO ()
main = do
  stack ["init", "--resolver", "nightly-2017-07-25", "--solver", "--force"]
  stack ["build"]
