import StackTest
import Control.Monad
import Data.List

main :: IO ()
main = do
    stack ["build", "random", "--stack-yaml", "custom1/stack.yaml"]
    stackCheckStderr ["build", "random", "--stack-yaml", "custom2/stack.yaml"] $ \out -> do
      print out
      unless ("precompiled" `isInfixOf` out) $ error "Didn't use precompiled!"
