import StackTest

main :: IO ()
main = do
  stack ["clean", "--full"]
  stack ["build", "acme-dont-copy"]
  stack ["test"]
