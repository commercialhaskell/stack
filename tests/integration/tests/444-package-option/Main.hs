-- Stack's runghc command allows packages to be identified for use with scripts.
--
-- See: https://github.com/commercialhaskell/stack/issues/444

import           StackTest

main :: IO ()
main = stack
  ["--snapshot", "mySnapshot.yaml", "runghc", "--package", "acme-missiles", "Script.hs"]
