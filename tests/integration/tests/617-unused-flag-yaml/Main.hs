-- Stack reports an error when a Cabal flag is specified for a specific
-- non-existent package.
--
-- See: https://github.com/commercialhaskell/stack/issues/617

import StackTest

main :: IO ()
main = stackErr ["build"]
