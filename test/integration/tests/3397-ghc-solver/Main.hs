import StackTest

main :: IO ()
main = do
  removeFileIgnore "stack.yaml"
  removeFileIgnore "issue3397.cabal"
  stack ["init", "--solver", "--resolver", "ghc-8.0.2"]
  stack ["solver", "--update-config"]
