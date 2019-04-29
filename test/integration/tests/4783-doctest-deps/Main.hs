import StackTest

main :: IO ()
main = do
  stack ["clean", "--full"]
  stack ["test"]
