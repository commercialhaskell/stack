-- Stack allows an extra-dep to be specified as a URL for an archive file.
--
-- See: https://github.com/commercialhaskell/stack/issues/1884

import StackTest

main :: IO ()
main = stack ["build", "--dry-run"]
