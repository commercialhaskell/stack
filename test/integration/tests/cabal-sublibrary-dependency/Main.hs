import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = do
  putStrLn "Disabled: CI doesn't have GHC 8.8.1"
  {-
  stackErrStderr ["build"] $ \str ->
    let msg = "SubLibrary dependency is not supported, this will almost certainly fail" in

    unless (msg `isInfixOf` str) $
    error $ "Expected a warning: \n" ++ show msg
  -}
