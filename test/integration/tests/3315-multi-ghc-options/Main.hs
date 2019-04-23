import StackTest

main :: IO ()
main = do
  stack ["build", "--ghc-options=-ddump-simpl -ddump-asm -DBAR -DBAZ"]
  repl ["--ghc-options=-ddump-simpl -ddump-asm"] (pure ())
