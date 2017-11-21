import StackTest

main :: IO ()
main = do
  stack ["build", "--no-nix"]
  stack ["build", "--nix"]
