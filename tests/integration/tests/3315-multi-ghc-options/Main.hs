-- Stack allows one or more GHC options to be specified on the command line.
--
-- See: https://github.com/commercialhaskell/stack/issues/3315

import StackTest.Repl

main :: IO ()
main = do
  stack ["build", "--ghc-options=-ddump-simpl -ddump-asm -DVARIABLE_A -DVARIABLE_B"]
  stackRepl ["--ghc-options=-ddump-simpl -ddump-asm"] (pure ())
