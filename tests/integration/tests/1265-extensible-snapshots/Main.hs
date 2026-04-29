-- Stack supports extensible snapshots.
--
-- See: https://github.com/commercialhaskell/stack/issues/1198

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  stack ["build", "myPackageA", "--dry-run"]
  -- Should fail because myPackageB depends on acme-box, which has been dropped:
  stackErrStderr ["build", "myPackageB", "--dry-run"] (expectMessage acmeBoxNeeded)
  stack ["build", "--stack-yaml", "stack-modify-lts.yaml", "myPackageA", "--dry-run"]
  stack ["build", "--stack-yaml", "stack-local-snapshot.yaml", "myPackageC", "--dry-run"]
  stack ["build", "--stack-yaml", "stack-remote-snapshot.yaml", "myPackageA", "--dry-run"]
  -- Should fail because myPackageD depends on zlib, which has been dropped:
  stackErrStderr
    ["build", "--stack-yaml", "stack-modify-lts.yaml", "myPackageD", "--dry-run"]
    (expectMessage zlibNeeded)
 where
  acmeBoxNeeded = "acme-box needed, but no version"
  zlibNeeded = "zlib needed, but no version"

expectMessage :: String -> String -> IO ()
expectMessage msg stderr = do
  unless (words msg `isInfixOf` words stderr)
         (error $ "Expected a warning: \n" ++ show msg)
