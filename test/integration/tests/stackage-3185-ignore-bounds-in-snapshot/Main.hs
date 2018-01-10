import StackTest

main :: IO ()
main = do
  stack ["build", "--dry-run"]
  stackErr ["build", "--stack-yaml", "with-rev.yaml", "--dry-run"]
