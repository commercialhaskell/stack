import System.Process (rawSystem)
import Control.Exception (throwIO)

main :: IO ()
main = rawSystem "bash" ["run.sh"] >>= throwIO
