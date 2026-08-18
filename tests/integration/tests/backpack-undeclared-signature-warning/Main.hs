-- | Stack explains likely Backpack signature mistakes with a useful warning,
-- and not an inappropriate warning about custom preprocessors.
--
-- See: https://github.com/commercialhaskell/stack/issues/6937

import           Control.Monad ( unless, when )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main =
  stackErrStderr ["build"] $ \err -> do
    expectMessage "Found Backpack signature file for Cabal entry" err
    expectMessage "Logger" err
    expectMessage "not listed in the component's" err
    expectMessage "signatures" err
    when ("custom-preprocessor-extensions" `isInfixOf` err) $
      error $
           "Expected no custom-preprocessor warning for Logger.hsig, got:\n\n"
        ++ err

expectMessage :: String -> String -> IO ()
expectMessage msg stderr = do
  unless (words msg `isInfixOf` words stderr) $
    error $ "Expected " ++ show msg ++ " in stderr, got:\n\n" ++ stderr
