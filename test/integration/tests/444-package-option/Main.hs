import StackTest

main :: IO ()
main = do
  isAlpine <- getIsAlpine
  if isAlpine
    then logInfo "Disabled on Alpine Linux since it cannot yet install its own GHC."
    else stack ["--install-ghc", "runghc", "--package", "safe", "Test.hs"]
