import StackTest.Repl

main :: IO ()
main = do
  stack ["build", "--ghc-options=-ddump-simpl -ddump-asm -DBAR -DBAZ"]
  stackRepl ["--ghc-options=-ddump-simpl -ddump-asm"] (pure ())
