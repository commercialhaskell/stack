-- Stack ignores a package's Setup.hs file when the Cabal build type is Simple.
--
-- See: https://github.com/commercialhaskell/stack/issues/370

import           StackTest

main :: IO ()
main = stack ["build"]
