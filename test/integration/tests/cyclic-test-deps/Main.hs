import StackTest

main :: IO ()
main = do
    stack ["unpack", "text-1.2.1.1"]
    stack ["init", "--resolver", "lts-3.0"]
    stack ["test", "--dry-run"]
