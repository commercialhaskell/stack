import System.Process (rawSystem)
import Control.Exception (throwIO)
import StackTest
import Control.Monad (unless)

main :: IO ()
main = rawSystem "sh" ["run.sh"] >>= throwIO
