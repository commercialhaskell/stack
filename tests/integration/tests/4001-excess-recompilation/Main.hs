import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = do
  copy "test/Main1.hs" "test/Main.hs"
  copy "bench/Main1.hs" "bench/Main.hs"
  stack ["build"]

  copy "test/Main2.hs" "test/Main.hs"
  copy "bench/Main2.hs" "bench/Main.hs"
  res <- unregisteringLines . snd <$> stackStderr ["build"]
  removeFileIgnore "test/Main.hs"
  removeFileIgnore "bench/Main.hs"
  unless (null res) $ fail "Stack recompiled when a test or benchmark file was changed, but only the library was targeted"

unregisteringLines :: String -> [String]
unregisteringLines = filter (isInfixOf " unregistering ") . lines
