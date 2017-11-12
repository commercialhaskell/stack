import StackTest

main :: IO ()
main = do
  return ()
  {- FIXME: Temporarily disabled.  See #3353 / #3315
  stack ["build", "--ghc-options=-DBAR -DBAZ"]
  stack ["setup", "--ghcjs-boot-options=-DBAR -DBAZ"]
  -}
