-- Stack's config set commands should recreate the global-project directory, if
-- Stack needs to consult its project-level configuration file and there is no
-- file.
--
-- See: https://github.com/commercialhaskell/stack/issues/6915

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  -- In a clean Stack root, there is no global-projects directory.
  stackCheckStderr
    ["config", "set", "install-ghc", "false"]
    (expectMessage writingConfigFile)

writingConfigFile :: String
writingConfigFile =
  "Writing the configuration file for the implicit global project to:"

expectMessage :: String -> String -> IO ()
expectMessage msg stderr = do
  unless (words msg `isInfixOf` words stderr) $
    error $ "Expected output: \n" ++ show msg
