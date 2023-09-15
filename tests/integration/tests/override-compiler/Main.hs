import StackTest
import Control.Monad (unless)

main :: IO ()
main = stackCheckStdout ["exec", "--", "ghc", "--numeric-version"] $ \ver ->
  -- get rid of the newline character
  unless (concat (lines ver) == "9.4.7") $ error $ "Invalid version: " ++ show ver
