-- | Stack detects when snapshot locations form a cycle and reports an error.
--
-- See: https://github.com/commercialhaskell/stack/issues/6927

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  -- `stack build` makes use of pantry's loadAndCompleteSnapshotRaw':
  stackErrStderr
    ["--stack-yaml", "stack1.yaml", "build"]
    (expectMessage cycleDetected)
  stackErrStderr
    ["--stack-yaml", "stack2.yaml", "build"]
    (expectMessage cycleDetected)
  -- `stack --snapshot <snapshot> list` makes use of pantry's loadSnapshot:
  stackErrStderr
    ["--snapshot", "my-snapshot.yaml", "list", "base"]
    (expectMessage cycleDetected)
  stackErrStderr
    ["--snapshot", "my-snapshot1.yaml", "list", "base"]
    (expectMessage cycleDetected)

cycleDetected :: String
cycleDetected = "Cycle detected while reading snapshot."

expectMessage :: String -> String -> IO ()
expectMessage msg stderr =
 unless (words msg `isInfixOf` words stderr) $
   error $ "Expected:\n\n" <> msg <> "\n\nin stderr, got:\n\n" <> stderr
