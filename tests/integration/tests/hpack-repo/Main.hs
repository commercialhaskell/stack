-- Stack supports immutable dependency packages that are described only by a
-- package.yaml file. However, this work flow is deprecated.
--
-- See: https://github.com/commercialhaskell/stack/issues/5210

import           StackTest

main :: IO ()
main = stack ["build"]
