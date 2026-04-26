-- Stack should build a package with a main library and an internal library (a
-- private named sublibrary) even if the main library does not depend on the
-- sublibrary.
--
-- See: https://github.com/commercialhaskell/stack/issues/3996

import           StackTest

main :: IO ()
main = stack ["build"]
