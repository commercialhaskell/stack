import StackTest

main :: IO ()
main = do
  stackErr ["--stack-yaml", "stack-bad.yaml", "build", "--dry-run"]
  stack ["--stack-yaml", "stack-good.yaml", "build"]
