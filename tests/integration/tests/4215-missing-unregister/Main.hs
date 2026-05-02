import           StackTest

main :: IO ()
main = do
  stack ["--stack-yaml", "stack1.yaml", "build"]
  stack [ "--stack-yaml", "stack2.yaml", "build"]
