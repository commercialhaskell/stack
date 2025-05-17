import Control.Monad.IO.Class
import Control.Monad
import Data.List

import StackTest.Repl

main :: IO ()
main = do
  stack ["clean"] -- to make sure we can load the code even after a clean
  copy "src/Lib.v1" "src/Lib.hs"
  copy "src-internal/Internal.v1" "src-internal/Internal.hs"
  stack ["build"] -- need a build before ghci at the moment, see #4148
  stackRepl [] $ do
    -- The command must be issued before searching the output for the next prompt,
    -- otherwise, on Windows from msys2-20230526, `stack repl` encounters a EOF
    -- and terminates gracefully.
    replCommand ":main"
    liftIO $ putStrLn "Awaiting prompt..."
    nextPrompt
    liftIO $ putStrLn "Initial prompt received"
    line <- replGetLine
    let expected = "hello world"
    when (line /= expected) $
      error $
          "Main module didn't load correctly.\n"
        <> "Expected: " <> expected <> "\n"
        <> "Actual  : " <> line <> "\n"
    liftIO $ copy "src-internal/Internal.v2" "src-internal/Internal.hs"
    reloadAndTest "testInt" "42" "Internal library didn't reload."
    liftIO $ copy "src/Lib.v2" "src/Lib.hs"
    reloadAndTest "testStr" "\"OK\"" "Main library didn't reload."

reloadAndTest :: String -> String -> String -> Repl ()
reloadAndTest cmd exp err = do
  reload
  replCommand cmd
  line <- replGetLine
  liftIO . putStrLn $ line
  unless (exp `isSuffixOf` line) $ error err

reload :: Repl ()
reload = replCommand ":reload" >> loop
  where
    loop = replGetLine >>= \line -> unless ("Ok" `isInfixOf` line) loop
