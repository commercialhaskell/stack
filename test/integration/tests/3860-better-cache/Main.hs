import StackTest
import Control.Monad
import Data.List
import Stack.Types.Build

cache = PrecompiledCache Nothing []

main :: IO ()
main = do
    stack ["build", "stm", "--stack-yaml", "stack.yaml"]
    stackCheckStderr ["build", "stm", "--stack-yaml", "stack.yaml"] $ \out ->
      unless ("precompiled" `isInfixOf` out) $ 
      error "Didn't use precompiled!"
