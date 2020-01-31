import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = do
  stackErrStderr ["build"] $ \str ->
    let msg = "SubLibrary dependency is not supported, this will almost certainly fail" in

    unless (msg `isInfixOf` str) $
    error $ "Expected a warning: \n" ++ show msg
