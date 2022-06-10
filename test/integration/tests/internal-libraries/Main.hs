import StackTest

main :: IO ()
main = do
  stack ["setup"] -- See stack.yaml; using GHC 8.10.7
  stack ["build"]
