import StackTest
import Data.List (isInfixOf)

main :: IO ()
main = do
  stack ["clean", "--full"]
  stackCheckStderr ["build", "--terminal", "--color=always"] $ \str ->
    if "no type signature" `isInfixOf` str
      then pure ()
      else error "Warnings are not being shown"
