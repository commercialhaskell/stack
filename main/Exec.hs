{-# LANGUAGE LambdaCase #-}

import Control.Monad.Logger (runNoLoggingT)
import Data.Monoid
import Stackage.Path (getBinPaths)
import System.Environment (getEnv, setEnv, getArgs)
import System.FilePath (searchPathSeparator)
import System.Process (callProcess)

exec :: String -> [String] -> IO ()
exec cmd args = do
  paths <- runNoLoggingT getBinPaths
  oldPath <- getEnv "PATH"
  setEnv "PATH" (paths <> [searchPathSeparator] <> oldPath)
  callProcess cmd args

execHelp :: IO ()
execHelp = putStrLn "usage: stackage-exec CMD [ARGS]"

execVersion :: IO ()
execVersion = putStrLn "stackage-exec-0.0.0"

execSummary :: IO ()
execSummary = putStrLn ""

-- TODO: fancier usage, version, help, etc.
main :: IO ()
main = getArgs >>= \case
  [] -> execHelp
  ["--help"] -> execHelp
  ["--"] -> execHelp
  ["--version"] -> execVersion
  ["--summary"] -> execSummary
  ("--":cmd:args) -> exec cmd args
  (cmd:args) -> exec cmd args
