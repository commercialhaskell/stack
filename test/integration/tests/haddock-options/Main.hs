import StackTest

main :: IO ()
main = do
    removeDirIgnore ".stack-work"

    -- Fails to work because BAR is defined here and FOO in stack file
    stackErr ["haddock", "--haddock-arguments", "--optghc=-DBAR"]
    stack ["clean"]
    -- Works just fine, test #3099 while at it.
    stack ["haddock", "--no-haddock-hyperlink-source"]
    stack ["clean"]
    -- Fails to work because we have bad argument
    stackErr ["haddock", "--haddock-arguments", "--stack_it_badhaddockargument"]
