import StackTest

main :: IO ()
main = do
  stackErr ["build", "--stack-yaml", "as-extra-dep.yaml", "--dry-run"]
  stack ["build", "--stack-yaml", "as-snapshot.yaml", "--dry-run"]
