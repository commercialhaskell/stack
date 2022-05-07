import StackTest

main :: IO ()
main = do
  stack ["--stack-yaml", "stack-bad.yaml", "setup"]
  stackErr ["--stack-yaml", "stack-bad.yaml", "build", "--dry-run"]
  stack ["--stack-yaml", "stack-good.yaml", "setup"]
  stack ["--stack-yaml", "stack-good.yaml", "build"]
