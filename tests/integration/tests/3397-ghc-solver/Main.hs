{--

import StackTest

main :: IO ()
main = do
  removeFileIgnore "stack.yaml"
  removeFileIgnore "issue3397.cabal"
  stack ["init", "--solver", "--snapshot", "ghc-8.2.2"]
  stack ["solver", "--update-config"]

// --}

main :: IO ()
main = putStrLn "This test is disabled (see https://github.com/commercialhaskell/stack/issues/4410)."
