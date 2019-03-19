import StackTest

main :: IO ()
main = do
  stack ["ls", "dependencies", "--global-hints"]
  stack ["dot", "--global-hints"]
