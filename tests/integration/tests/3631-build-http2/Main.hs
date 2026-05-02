-- Stack can build the http2 package.
--
--
-- https://github.com/commercialhaskell/stack/issues/3631

import StackTest

main :: IO ()
main = do
  stack ["build", defaultSnapshotArg, "--dry-run", "http2"]
  stack ["build", defaultSnapshotArg, "http2"]
