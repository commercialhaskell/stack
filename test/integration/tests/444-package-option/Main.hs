import StackTest

main :: IO ()
main = stack ["--install-ghc", "runghc", "--package", "safe", "Test.hs"]
