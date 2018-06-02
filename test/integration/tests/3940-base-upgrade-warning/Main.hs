import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

unattainableBaseWarning :: String
unattainableBaseWarning =
  "Build requires unattainable version of base. Since base is a part of GHC, \
  \you most likely need to use a different GHC version with the matching base."

noBaseUpgradeWarning :: String
noBaseUpgradeWarning =
  "You are trying to upgrade/downgrade base, which is almost certainly \
  \not what you really want. Please, consider using another GHC version \
  \if you need a certain version of base, or removing base from extra-deps. \
  \See more at https://github.com/commercialhaskell/stack/issues/3940."

main :: IO ()
main = do
  stackErrStderr ["build", "--stack-yaml", "unattainable-base.yaml"] (expectMessage unattainableBaseWarning)
  stackErrStderr ["build", "--stack-yaml", "no-base-upgrade.yaml"] (expectMessage noBaseUpgradeWarning)

expectMessage :: String -> String -> IO ()
expectMessage msg stderr =
  unless (words msg `isInfixOf` words stderr)
         (error $ "Expected a warning: \n" ++ show msg)
