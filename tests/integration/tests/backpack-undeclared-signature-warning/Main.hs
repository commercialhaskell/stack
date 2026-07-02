-- Stack should explain likely Backpack signature mistakes with a Backpack
-- warning, not the custom-preprocessor warning.

import Control.Monad ( unless, when )
import Data.List ( isInfixOf )
import StackTest

main :: IO ()
main =
  stackErrStderr ["build"] $ \err -> do
    expect err "Found Backpack signature file for Cabal entry"
    expect err "Logger"
    expect err "not listed in the component's"
    expect err "signatures"
    when ("custom-preprocessor-extensions" `isInfixOf` err) $
      error $
           "Expected no custom-preprocessor warning for Logger.hsig, got: "
        ++ show err

expect :: String -> String -> IO ()
expect err msg =
  unless (msg `isInfixOf` err) $
    error $ "Expected " ++ show msg ++ " in stderr, got: " ++ show err
