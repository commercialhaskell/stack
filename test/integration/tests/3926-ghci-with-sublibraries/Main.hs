import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad
import Data.List
import StackTest

main :: IO ()
main = do
  stack ["clean"] -- to make sure we can load the code even after a clean
  copy "src/Lib.v1" "src/Lib.hs"
  copy "src-internal/Internal.v1" "src-internal/Internal.hs"
  stack ["build"] -- need a build before ghci at the moment, see #4148
  forkIO fileEditingThread
  replThread

replThread :: IO ()
replThread = repl [] $ do
  replCommand ":main"
  line <- replGetLine
  when (line /= "hello world") $ error "Main module didn't load correctly."
  liftIO $ threadDelay 1000000 -- wait for an edit of the internal library
  reloadAndTest "testInt" "42" "Internal library didn't reload."
  liftIO $ threadDelay 1000000 -- wait for an edit of the internal library
  reloadAndTest "testStr" "\"OK\"" "Main library didn't reload."

fileEditingThread :: IO ()
fileEditingThread = do
  threadDelay 1000000
  -- edit the internal library and pure to ghci
  copy "src-internal/Internal.v2" "src-internal/Internal.hs"
  threadDelay 1000000
  -- edit the internal library and end thread, returning to ghci
  copy "src/Lib.v2" "src/Lib.hs"

reloadAndTest :: String -> String -> String -> Repl ()
reloadAndTest cmd exp err = do
  reload
  replCommand cmd
  line <- replGetLine
  unless (exp `isSuffixOf` line) $ error err

reload :: Repl ()
reload = replCommand ":reload" >> loop
  where
    loop = replGetLine >>= \line -> unless ("Ok" `isInfixOf` line) loop
