-- Stack builds a package with a private named sublibrary (an internal library)
-- but no main library.
--
-- See: https://github.com/commercialhaskell/stack/issues/3787

import           StackTest

main :: IO ()
main = stack ["build"]
