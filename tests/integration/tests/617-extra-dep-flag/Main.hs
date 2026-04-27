-- Stack can specify a Cabal flag for an extra-dep.
--
-- See: https://github.com/commercialhaskell/stack/issues/617

import StackTest

main :: IO ()
main = stack ["build", "acme-missiles-0.3"]
