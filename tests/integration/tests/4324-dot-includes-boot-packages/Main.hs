-- Stack's dot command includes the dependencies of GHC's boot packages.
--
-- See: https://github.com/commercialhaskell/stack/issues/4324

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  stackCheckStdout ["dot", "--external"] $ \str ->
    unless ("\n\"process\" ->" `isInfixOf` str) $
      error "Not showing dependencies of process"
