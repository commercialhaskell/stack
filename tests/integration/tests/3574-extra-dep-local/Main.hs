-- Stack can target a local extra-dep and distinguishes local extra-deps from
-- local packages, when applying GHC options to local packages.
--
-- See: https://github.com/commercialhaskell/stack/issues/3574

import StackTest

main :: IO ()
main = stack ["build", "myPackage"]
