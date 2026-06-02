-- | Stack can support a dependency package that has one or more public
-- sublibraries but no unnamed main library.

import           StackTest

main :: IO ()
main = stack ["build"]
