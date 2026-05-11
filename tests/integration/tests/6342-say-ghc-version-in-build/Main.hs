-- Stack outputs the GHC version that it is using during a build.
--
-- See: https://github.com/commercialhaskell/stack/issues/6342

import           Control.Monad ( unless )
import           Data.Char ( isSpace )
import           Data.List ( dropWhileEnd, isInfixOf )
import           StackTest

main :: IO ()
main =
  -- Query the actual compiler
  stackCheckStdout ["query", "compiler", "actual"] $ \compiler -> do
    stackCheckStderr ["build"] (expectMessage $ buildWith (trimEnd compiler))

buildWith :: String -> String
buildWith compiler = "build (lib) with " <> compiler

expectMessage :: String -> String -> IO ()
expectMessage msg stderr = do
  unless (words msg `isInfixOf` words stderr)
         (error $ "Expected message: \n" ++ show msg)

trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace
