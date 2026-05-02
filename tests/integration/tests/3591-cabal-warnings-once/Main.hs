-- Stack warns about unknown fields in Cabal files, but only once.
--
-- https://github.com/commercialhaskell/stack/issues/3591

import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  stackCheckStderr ["build", "--dry-run"] $ \str ->
    case filter ("unknown-cabal-field-name" `isInfixOf`) (lines str) of
      [] -> error "unknown-Cabal-field-name didn't appear once"
      [_] -> pure ()
      _:_:_ -> error "unknown-Cabal-field-name appeared multiple times"
