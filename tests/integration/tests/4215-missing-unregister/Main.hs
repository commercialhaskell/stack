-- Even if the project directory is unchanged and the project package name and
-- version is the same, Stack recognises when a project package is different to
-- a project package that has been built previously.
--
-- See: https://github.com/commercialhaskell/stack/issues/4215

import           StackTest

main :: IO ()
main = do
  stack ["--stack-yaml", "stack1.yaml", "build"]
  stack ["--stack-yaml", "stack2.yaml", "build"]
