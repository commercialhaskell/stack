import StackTest

main :: IO ()
main = do
  stack ["build", defaultSnapshotArg, "--dry-run", "http2"]
  stack ["build", defaultSnapshotArg, "http2"]
