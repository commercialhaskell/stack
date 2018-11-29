import StackTest
import System.Directory

main :: IO ()
main = do
  stack ["build"]
  stack ["purge"]
