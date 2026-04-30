-- Stack trusts package versions in a snapshot over Cabal file dependency
-- information.
--
-- See: https://github.com/commercialhaskell/stack/issues/3861

import StackTest

main :: IO ()
main = do
  stackErr ["build", "--stack-yaml", "stack-bad.yaml"]
  stack ["build", "--stack-yaml", "stack-good.yaml"]
