import StackTest

main :: IO ()
main = do
    stack ["init"]
    stack ["test"]
