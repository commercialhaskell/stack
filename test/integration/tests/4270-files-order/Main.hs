import Control.Monad
import StackTest

main :: IO ()
main = do
    stack ["build"]
    repl [] $ do
        replCommand "putStrLn greeting"
        line <- replGetLine
        when (line /= "Hello, world!") $ error "Didn't load correctly."
