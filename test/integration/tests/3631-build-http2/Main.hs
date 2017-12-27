import StackTest

main :: IO ()
main = do
  stack ["build", "--resolver=lts-9.14", "--dry-run", "http2"]
  stack ["build", "--resolver=lts-9.14", "http2"]
