-- Stack should still suggest custom-preprocessor-extensions for unknown
-- non-Haskell module file extensions.

import Control.Monad ( unless )
import Data.List ( isInfixOf )
import StackTest

main :: IO ()
main =
  stackErrStderr ["build"] $ \err -> do
    expect err "Unable to find a known candidate for the Cabal entry"
    expect err "Generated"
    expect err "Generated.foo"
    expect err "custom-preprocessor-extensions"

expect :: String -> String -> IO ()
expect err msg =
  unless (msg `isInfixOf` err) $
    error $ "Expected " ++ show msg ++ " in stderr, got: " ++ show err
