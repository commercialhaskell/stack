import StackTest
import System.Process
import Control.Exception (throwIO)

main :: IO ()
main = rawSystem "bash" ["run.sh"] >>= throwIO
