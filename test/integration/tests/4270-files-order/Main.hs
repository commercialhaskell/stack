import Control.Monad
import StackTest

main :: IO ()
main = do
    stack ["build"]
    repl [] $ do
        replCommand "putStrLn greeting"
        line <- replGetLine
        let expected = "Hello, world!"
        when (line /= expected) $
          error $
               "Didn't load correctly.\n"
            <> "Expected: " <> expected <> "\n"
            <> "Actual  : " <> line <> "\n"
