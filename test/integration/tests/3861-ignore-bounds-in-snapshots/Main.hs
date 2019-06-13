import StackTest

main :: IO ()
main = do
  stack ["clean", "--stack-yaml", "stack-good.yaml", "--full"]
  stackErr ["build", "--stack-yaml", "stack-bad.yaml"]
  stack ["build", "--stack-yaml", "stack-good.yaml"]
