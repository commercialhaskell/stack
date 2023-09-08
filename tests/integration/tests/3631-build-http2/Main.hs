import StackTest

main :: IO ()
main = do
  stack ["build", defaultResolverArg, "--dry-run", "http2"]
  stack ["build", defaultResolverArg, "http2"]
