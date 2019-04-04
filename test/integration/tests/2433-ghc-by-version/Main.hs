import System.Process (rawSystem)
import Control.Exception (throwIO)

main :: IO ()
main = rawSystem "sh" ["run.sh"] >>= throwIO
