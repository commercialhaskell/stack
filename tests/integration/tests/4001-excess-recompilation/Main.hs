-- Stack does not recompile a package when a test suite or benchmark is dirty
-- but the test suite or benchmark is not a build target.
--
-- See: https://github.com/commercialhaskell/stack/issues/4001

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  copy "test/Main.v1" "test/Main.hs"
  copy "bench/Main.v1" "bench/Main.hs"
  stack ["build"]

  copy "test/Main.v2" "test/Main.hs"
  copy "bench/Main.v2" "bench/Main.hs"
  res <- unregisteringLines . snd <$> stackStderr ["build"]
  unless (null res) $
    fail "Stack recompiled when a test or benchmark file was changed, but only \
         \the library was targeted."

unregisteringLines :: String -> [String]
unregisteringLines = filter (isInfixOf " unregistering ") . lines
