-- | Stack can build a project package that depends on the public named library
-- (a sublibrary) of another project package. However, the latter package must
-- also have a main library (which may be a Stack bug).
--
-- See: https://github.com/commercialhaskell/stack/issues/6896

import          StackTest

main :: IO ()
main = stack ["build", ":myExe"]
