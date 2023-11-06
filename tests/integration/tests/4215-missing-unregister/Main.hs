import StackTest

main :: IO ()
main = do
  stack ["build", "--stack-yaml", "stack1.yaml"]
  stack ["build", "--stack-yaml", "stack2.yaml"]
