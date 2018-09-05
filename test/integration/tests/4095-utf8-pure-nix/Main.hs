import StackTest

main :: IO ()
main = do
  stack ["build", "--nix-pure"]
  stack ["exec", "--nix-pure", "ShowUnicode"]
