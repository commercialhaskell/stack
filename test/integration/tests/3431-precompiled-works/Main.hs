import StackTest
import Control.Monad
import Data.List

main :: IO ()
main = do
    stack ["build", "stm", "--stack-yaml", "custom1/stack.yaml"]
    stackCheckStderr ["build", "stm", "--stack-yaml", "custom2/stack.yaml"] $ \out ->
      unless ("precompiled" `isInfixOf` out) $ error "Didn't use precompiled!"
