import StackTest

main :: IO ()
main = do
  stackCleanFull
  stack ["build", "acme-dont-copy"]
  stack ["test"]
