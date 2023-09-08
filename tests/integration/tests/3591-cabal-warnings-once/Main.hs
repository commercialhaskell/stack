import StackTest
import Data.List (isInfixOf)

main :: IO ()
main = do
  stackCheckStderr ["build", "--dry-run"] $ \str ->
    case filter ("unknown-field-name" `isInfixOf`) (lines str) of
      [] -> error "unknown-field-name didn't appear once"
      [_] -> pure ()
      _:_:_ -> error "unknown-field-name appeared multiple times"
