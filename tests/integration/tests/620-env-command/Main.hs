import StackTest
import System.Process
import Control.Exception (throwIO)
import Control.Monad (unless)

main :: IO ()
main = unless isWindows $ rawSystem "bash" ["run.sh"] >>= throwIO
