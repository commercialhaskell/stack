-- An extra-dep in a project-level configuration file should be able to shadow a
-- GHC boot package such as bytestring.
--
-- See: https://github.com/commercialhaskell/stack/issues/345

import           StackTest

main :: IO ()
main = stack ["build"]
