import StackTest
import Control.Monad (void)

main :: IO ()
main = do
  void $ stack' ["exec", "ghc-pkg", "unregister", "unliftio-core"]
  stack ["clean"]

  stackErr ["build", "--only-locals"]
  stack ["build", "--only-snapshot"]
  stack ["build", "--only-locals"]
