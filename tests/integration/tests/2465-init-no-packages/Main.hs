-- Stack can initialise a project directory that contains no project packages.
--
-- See: https://github.com/commercialhaskell/stack/issues/2465

import           Control.Monad ( unless )
import           StackTest
import           System.Directory ( doesFileExist )

main :: IO ()
main = do
  stack ["--snapshot", "ghc-9.10.3", "init"]
  exists <- doesFileExist "stack.yaml"
  unless exists $ error "stack.yaml not created!"
