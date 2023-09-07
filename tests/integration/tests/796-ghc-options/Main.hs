import StackTest

main :: IO ()
main = do
    stack ["build"]
    stackErr ["build", "--ghc-options=-DBAZ"]
    stack ["build", "--ghc-options=-DQUX"]
