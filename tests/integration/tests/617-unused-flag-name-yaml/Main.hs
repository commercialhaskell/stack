-- Stack reports an error when a non-existent Cabal flag is specified for
-- a project package.
--
-- See: https://github.com/commercialhaskell/stack/issues/617

import StackTest

main :: IO ()
main = stackErr ["build"]
