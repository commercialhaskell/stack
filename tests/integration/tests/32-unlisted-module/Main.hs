-- Stack should rebuild a package when files required for building are dirty,
-- even if the files are not listed in the package's Cabal file.
--
-- See: https://github.com/commercialhaskell/stack/issues/32

import           Control.Concurrent ( threadDelay )
import           StackTest

main :: IO ()
main = do
  copy "app/Unlisted_OK.hs" "app/Unlisted.hs"
  copy "embed_OK.txt" "embed.txt"
  stack ["build"]
  pause
  copy "app/Unlisted_FAIL.hs" "app/Unlisted.hs"
  stackErr ["build"]
  pause
  copy "app/Unlisted_OK.hs" "app/Unlisted.hs"
  stack ["build"]
  stack ["exec", "fail-if-fail"]
  pause
  copy "embed_FAIL.txt" "embed.txt"
  stack ["build"]
  stackErr ["exec", "fail-if-fail"]
  pause
  copy "embed_OK.txt" "embed.txt"
  stack ["build"]
  stack ["exec", "fail-if-fail"]
 where
  pause = threadDelay 1000000
