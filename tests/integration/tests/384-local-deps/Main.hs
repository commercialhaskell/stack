-- Stack can initialise a multi-project package where one project package
-- depends on another project package.
--
-- See: https://github.com/commercialhaskell/stack/issues/384

import StackTest

main :: IO ()
main = do
  stack ["init"]
  stack ["build"]
