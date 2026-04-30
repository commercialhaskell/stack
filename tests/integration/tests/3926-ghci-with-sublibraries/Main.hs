-- Stack does not recompile a package with a private named sublibrary (an
-- internal library) on a second build.
--
-- See: https://github.com/commercialhaskell/stack/issues/3926

import Control.Monad ( unless, when )
import Control.Monad.IO.Class ( liftIO )
import Data.List ( isInfixOf, isSuffixOf )
import StackTest.Repl

main :: IO ()
main = do
  copy "src/Lib.v1" "src/Lib.hs"
  copy "int/Internal.v1" "int/Internal.hs"
  stack ["build"] -- need a build before ghci at the moment, see #4148
  stackRepl [] $ do
    nextPrompt
    replCommand ":main"
    line <- replGetLine
    let expected = "Successful!"
    when (line /= expected) $
      error $
          "Main module didn't load correctly.\n"
        <> "Expected: " <> expected <> "\n"
        <> "Actual  : " <> line <> "\n"
    liftIO $ copy "int/Internal.v2" "int/Internal.hs"
    reloadAndTest "checkInternal" "\"OK\"" "Internal library didn't reload."
    liftIO $ copy "src/Lib.v2" "src/Lib.hs"
    reloadAndTest "checkLib" "\"OK\"" "Main library didn't reload."

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
