import StackTest

main :: IO ()
main = do
    stack ["config", "set", "resolver", "ghc-8.0.1"]
    stack ["config", "set", "resolver", "ghc-8.0"]
    stack ["config", "set", "resolver", "ghc-8"]
    stack ["config", "set", "resolver", "nightly"]
    stack ["config", "set", "resolver", "lts-7"]

    stackErr ["config", "set", "resolver", "lts-9999"]
    stackErr ["config", "set", "resolver", "lts-foo"]
    stackErr ["config", "set", "resolver", "ghc-foo"]
    stackErr ["config", "set", "resolver", "foo"]
