-- Stack ignores test suites that are not buildable.
--
-- See: https://github.com/commercialhaskell/stack/issues/3390

import StackTest

main :: IO ()
main = stack ["test"]
