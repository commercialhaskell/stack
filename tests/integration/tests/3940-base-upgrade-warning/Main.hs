import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

-- Use short message fragment because prettyWarn formatting and colour
unattainableBaseWarning :: String
unattainableBaseWarning =
  "Build requires unattainable version of"

-- Use short message fragment because prettyWarn formatting and colour
noBaseUpgradeWarning :: String
noBaseUpgradeWarning =
  "Before GHC 9.12.1, the base package is"

main :: IO ()
main = do
  stackErrStderr ["build", "--stack-yaml", "unattainable-base.yaml"] (expectMessage unattainableBaseWarning)
  stackErrStderr ["build", "--stack-yaml", "no-base-upgrade.yaml"] (expectMessage noBaseUpgradeWarning)

expectMessage :: String -> String -> IO ()
expectMessage msg stderr =
  unless (words msg `isInfixOf` words stderr)
         (error $ "Expected a warning: \n" ++ show msg)
