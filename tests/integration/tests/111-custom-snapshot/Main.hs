-- Stack should build a package when a custom snapshot is specified in the
-- project-level configuration file.
--
-- See: https://github.com/commercialhaskell/stack/issues/111

import StackTest

main :: IO ()
main = stack ["build"]
