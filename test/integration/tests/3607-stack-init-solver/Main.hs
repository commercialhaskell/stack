import StackTest

main :: IO ()
main = do
  -- The resolver specified should be a nightly with a GHC version that matches
  -- the LTS resolver used in Stack's top-level `stack.yaml`.
  stack ["init", "--resolver", "nightly-2018-03-12", "--solver", "--force"]
  stack ["build"]
