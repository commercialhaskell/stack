-- Stack uses pre-compiled immutable packages where it can.
--
-- See: https://github.com/commercialhaskell/stack/issues/3431

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  stack ["build", "random-1.1", "--stack-yaml", "custom1/stack.yaml"]
  stackCheckStderr ["build", "random-1.1", "--stack-yaml", "custom2/stack.yaml"] $ \out -> do
    print out
    unless ("precompiled" `isInfixOf` out) $ error "Didn't use precompiled!"
