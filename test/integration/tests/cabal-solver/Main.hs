import StackTest
import System.Directory

main :: IO ()
main = do
    run "cabal" ["sandbox", "init"]
    stack ["unpack", "acme-dont-1.1"]
    run "cabal" ["install", "./acme-dont-1.1"]
    removeDirectoryRecursive "acme-dont-1.1"
    stack ["init", "--solver"]
    stack ["build"]
