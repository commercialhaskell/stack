import Stackage.Path (getBinPaths)
import Control.Monad.Logger (runNoLoggingT)

main :: IO ()
main = do
  paths <- runNoLoggingT getBinPaths
  putStrLn paths
