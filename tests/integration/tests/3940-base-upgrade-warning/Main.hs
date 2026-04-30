-- Stack provides appropriate advice if the version of base required is not that
-- specified by the snapshot. Stack warns that base is a wired-in package before
-- GHC 9.12.1.
--
-- See: https://github.com/commercialhaskell/stack/issues/3940

import Control.Monad ( unless )
import Data.List ( isInfixOf )
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
  unless (words msg `isInfixOf` words stderr) $
    error $ "Expected a warning: \n" ++ show msg
