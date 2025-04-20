import StackTest

main :: IO ()
main = do
  stackCleanFull
  stack ["--verbose", "build", "acme-dont-copy"]
  stack ["--verbose", "test"]
