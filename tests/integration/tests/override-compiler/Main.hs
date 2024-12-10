import StackTest
import Control.Monad (unless)

main :: IO ()
main = stackCheckStdout ["exec", "--", "ghc", "--numeric-version"] $ \ver ->
  -- get rid of the newline character
  unless (concat (lines ver) == "9.8.4") $ error $ "Invalid version: " ++ show ver
