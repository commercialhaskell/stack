import StackTest

main :: IO ()
main = do
  removeDirIgnore "primitive-0.6.2.0"
  removeFileIgnore "primitive-0.6.2.0.tar.gz"
  stack ["unpack", "primitive-0.6.2.0@rev:0"]
  run "tar" ["cf", "primitive-0.6.2.0.tar.gz", "primitive-0.6.2.0"]
  stack ["clean", "--stack-yaml", "stack-good.yaml", "--full"]
  stackErr ["build", "--stack-yaml", "stack-bad.yaml"]
  stack ["build", "--stack-yaml", "stack-good.yaml"]
