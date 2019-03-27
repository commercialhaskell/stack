import System.Process (rawSystem)
import Control.Exception (throwIO)

main :: IO ()
main = rawSystem "./run.sh" [] >>= throwIO
