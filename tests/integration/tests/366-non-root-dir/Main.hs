-- Stack builds when commanded from a subdirectory of the project directory.
--
-- See: https://github.com/commercialhaskell/stack/issues/366

import           StackTest
import           System.Directory ( setCurrentDirectory )

main :: IO ()
main = do
  setCurrentDirectory "app"
  stack ["build"]
  stack ["exec", "myPackage"]
