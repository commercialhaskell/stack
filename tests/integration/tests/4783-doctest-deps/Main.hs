import StackTest

main :: IO ()
main = do
  stackCleanFull
  stack ["exec", "--", "which", "ld"]
  stack ["exec", "--", "ld", "-v"]
  stack ["--verbose", "build", "acme-dont-copy"]
  stack ["--verbose", "test"]
