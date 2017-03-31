import StackTest

main :: IO ()
main = do
    -- Fails to work because BAR is defined here and FOO in stack file
    stackErr ["haddock", "--haddock-arguments", "--optghc=-DBAR"]
    stack ["clean"]
    -- Works just fine
    stack ["haddock"]
    stack ["clean"]
    -- Fails to work because we have bad argument
    stackErr ["haddock", "--haddock-arguments", "--stack_it_badhaddockargument"]
