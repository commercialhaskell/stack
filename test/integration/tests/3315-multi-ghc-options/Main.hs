import StackTest

main :: IO ()
main = do
  stack ["build", "--ghc-options=-DBAR -DBAZ"]
  stack ["setup", "--ghcjs-boot-options=-DBAR -DBAZ"]
