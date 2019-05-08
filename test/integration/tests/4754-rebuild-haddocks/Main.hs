import StackTest

main :: IO ()
main = do
  stack ["clean", "--full"]
  stackErr ["haddock"]
  stack ["clean", "--full"]
  stackErr ["haddock", "--no-haddock-deps"]
  stack ["build"]
  stackErr ["haddock", "--no-haddock-deps"]
