-- Stack does not recompile a package with a private named sublibrary (an
-- internal library) on a second build.
--
-- See: https://github.com/commercialhaskell/stack/issues/3899

import           Control.Monad ( unless )
import           Data.List ( isInfixOf )
import           StackTest

main :: IO ()
main = do
  stack ["build"]
  res <- compilingModulesLines . snd <$> stackStderr ["build"]
  unless (null res) $ fail "Stack recompiled code"

-- Returns the lines where a module is compiled
compilingModulesLines :: String -> [String]
compilingModulesLines = filter (isInfixOf " Compiling ") . lines
