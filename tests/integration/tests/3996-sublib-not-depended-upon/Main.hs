import Control.Monad (unless)
import Data.List (isInfixOf)
import StackTest

main :: IO ()
main = do
  stack ["clean"]
  stack ["build"]
