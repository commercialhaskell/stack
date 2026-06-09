-- Stack's config set commands should work as expected.

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  stackCheckStderr
    ["config", "set", "snapshot", "ghc-9.10.2"]
    (expectMessage hasBeenUpdated)
  stackCheckStderr
    ["config", "set", "snapshot", "ghc-9.10.2"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["--stack-yaml", "stack-alt.yaml", "config", "set", "snapshot", "ghc-9.10.3"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["config", "set", "system-ghc", "false"]
    (expectMessage hasBeenExtended)
  stackCheckStderr
    ["config", "set", "system-ghc", "false"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["config", "set", "system-ghc", "true"]
    (expectMessage hasBeenUpdated)
  stackCheckStderr
    ["config", "set", "system-ghc", "true"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["--stack-yaml", "stack-alt.yaml", "config", "set", "system-ghc", "true"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["config", "set", "install-ghc", "true"]
    (expectMessage hasBeenExtended)
  stackCheckStderr
    ["config", "set", "install-ghc", "true"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["config", "set", "install-ghc", "false"]
    (expectMessage hasBeenUpdated)
  stackCheckStderr
    ["config", "set", "install-ghc", "false"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["--stack-yaml", "stack-alt.yaml", "config", "set", "install-ghc", "false"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["config", "set", "install-msys", "true"]
    (expectMessage hasBeenExtended)
  stackCheckStderr
    ["config", "set", "install-msys", "true"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["config", "set", "install-msys", "false"]
    (expectMessage hasBeenUpdated)
  stackCheckStderr
    ["config", "set", "install-msys", "false"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["--stack-yaml", "stack-alt.yaml", "config", "set", "install-msys", "false"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["config", "set", "recommend-stack-upgrade", "--project", "true"]
    (expectMessage hasBeenExtended)
  stackCheckStderr
    ["config", "set", "recommend-stack-upgrade", "--project", "true"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["config", "set", "recommend-stack-upgrade", "--project", "false"]
    (expectMessage hasBeenUpdated)
  stackCheckStderr
    ["config", "set", "recommend-stack-upgrade", "--project", "false"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["--stack-yaml", "stack-alt.yaml", "config", "set", "recommend-stack-upgrade", "--project", "false"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["config", "set", "package-index", "download-prefix", "https://hackage.haskell.org/"]
    (expectMessage hasBeenExtended)
  stackCheckStderr
    ["config", "set", "package-index", "download-prefix", "https://hackage.haskell.org/"]
    (expectMessage alreadyContained)
  stackCheckStderr
    ["--stack-yaml", "stack-alt.yaml", "config", "set", "package-index", "download-prefix", "https://hackage.haskell.org/"]
    (expectMessage alreadyContained)

hasBeenUpdated :: String
hasBeenUpdated =
  "has been updated."

alreadyContained :: String
alreadyContained =
  "already contained the intended configuration and remains unchanged."

hasBeenExtended :: String
hasBeenExtended =
  "has been extended."

expectMessage :: String -> String -> IO ()
expectMessage msg stderr = do
  unless (words msg `isInfixOf` words stderr) $
    error $ "Expected output: \n" ++ show msg
