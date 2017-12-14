import StackTest

main :: IO ()
main = do
  stack ["build", "--resolver=lts-6.35", "--dry-run", "http2"]
  stack ["build", "--resolver=lts-6.35", "http2"]
