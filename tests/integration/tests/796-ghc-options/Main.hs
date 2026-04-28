-- Stack can set environment variables in a project-level configuration file or
-- at the command line and cause the package to be rebuilt.
--
-- See: https://github.com/commercialhaskell/stack/issues/796

import StackTest

main :: IO ()
main = do
  stack ["build"]
  stackErr ["build", "--ghc-options=-DVARIABLE_C"]
  stack ["build", "--ghc-options=-DVARIABLE_D"]
