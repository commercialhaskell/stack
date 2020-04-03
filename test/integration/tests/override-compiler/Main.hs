import StackTest
import Control.Monad (unless)

main :: IO ()
main = stackCheckStdout ["exec", "--", "ghc", "--numeric-version"] $ \ver ->
  -- get rid of the newline character
  unless (concat (lines ver) == "8.6.5") $ error $ "Invalid version: " ++ show ver
