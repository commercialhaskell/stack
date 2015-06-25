import StackTest

main :: IO ()
main = do
    stackErr ["build"]
    stackErr ["build"]
    writeFile "app/Foo.hs" "module Foo where"
    stack ["build"]
    writeFile "app/Foo.hs" "module Foo wher e"
    stackErr ["build"]
    stackErr ["build"]
