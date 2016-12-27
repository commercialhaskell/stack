import StackTest
import System.Directory

main :: IO ()
main = do
  isAlpine <- getIsAlpine
  if isAlpine || isARM
    then logInfo "Disabled on Alpine Linux and ARM since it cannot yet install its own GHC."
    else do
      run "cabal" ["sandbox", "init"]
      stack ["unpack", "acme-dont-1.1"]
      run "cabal" ["install", "./acme-dont-1.1"]
      removeDirectoryRecursive "acme-dont-1.1"
      stack ["--install-ghc", "init", "--solver"]
      stack ["build"]
