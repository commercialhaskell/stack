import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

planFailure :: String
planFailure = "but this GHC boot package has been pruned"

main :: IO ()
main = do
  stackErrStderr ["build"] (expectMessage planFailure)

expectMessage :: String -> String -> IO ()
expectMessage msg stderr = do
  unless (words msg `isInfixOf` words stderr)
         (error $ "Expected an error: \n" ++ show msg)
