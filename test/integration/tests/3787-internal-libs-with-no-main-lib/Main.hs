import StackTest

main :: IO ()
main = do
  stack ["clean"]
  stack ["build"]
