import StackTest

main :: IO ()
main = do
  stackCleanFull
  stackErr ["haddock"]
  stackCleanFull
  stackErr ["haddock", "--no-haddock-deps"]
  stack ["build"]
  stackErr ["haddock", "--no-haddock-deps"]
