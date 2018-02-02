import           StackTest

main :: IO ()
main = do
    stack ["build", "async"]
    stackErr ["build", "zlib-bindings"]
    stack ["build", "--stack-yaml", "stack-modify-lts.yaml", "async"]
    stack ["build", "--stack-yaml", "stack-local-snapshot.yaml", "async"]
    stack ["build", "--stack-yaml", "stack-remote-snapshot.yaml", "async"]
    stackErr ["build", "--stack-yaml", "stack-modify-lts.yaml", "zlib-bindings"]
