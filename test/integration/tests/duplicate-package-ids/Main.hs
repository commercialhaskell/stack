import StackTest

main :: IO ()
main = do
    stack ["setup"]
    stack ["build", "auto-update"]
    readFile "stack2.yaml" >>= writeFile "stack.yaml"
    stack ["unpack", "auto-update-0.1.2.1"]
    stack ["build"]
