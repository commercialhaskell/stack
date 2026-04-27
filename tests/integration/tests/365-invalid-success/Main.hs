-- If Stack fails to build a package once, it should fail to build it
-- (unchanged) a second time.
--
-- See: https://github.com/commercialhaskell/stack/issues/365

import           StackTest

main :: IO ()
main = do
  copy "src/Lib_FAIL.hs" "src/Lib.hs"
  stackErr ["build"]
  stackErr ["build"]
  copy "src/Lib_OK.hs" "src/Lib.hs"
  stack ["build"]
  copy "src/Lib_FAIL.hs" "src/Lib.hs"
  stackErr ["build"]
  stackErr ["build"]
