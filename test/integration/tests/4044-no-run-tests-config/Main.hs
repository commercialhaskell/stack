import StackTest

main :: IO ()
main = do
  stack ["test"]
  stack ["build", "foo:test:foo"]
