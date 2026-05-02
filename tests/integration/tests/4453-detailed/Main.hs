-- Stack supports tests of type detailed-0.9.
--
-- See: https://github.com/commercialhaskell/stack/issues/4453

import           StackTest

main :: IO ()
main = stack ["test"]
