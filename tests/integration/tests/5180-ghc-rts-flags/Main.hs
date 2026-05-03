-- Stack supports GHC RTS options.
--
-- See: https://github.com/commercialhaskell/stack/issues/5180

import           StackTest

main :: IO ()
main = stack ["build"]
