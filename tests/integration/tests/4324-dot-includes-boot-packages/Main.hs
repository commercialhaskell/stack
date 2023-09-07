import StackTest
import Control.Monad (unless)
import Data.List (isInfixOf)

main :: IO ()
main = do
  stackCheckStdout ["dot", "--external"] $ \str ->
    unless ("\n\"process\" ->" `isInfixOf` str) $
    error "Not showing dependencies of process"
