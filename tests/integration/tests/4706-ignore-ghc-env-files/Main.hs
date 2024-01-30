import StackTest
import Control.Exception (bracket_)
import Control.Monad (when)
import System.Environment
import System.Directory
import System.Info (arch, os)

main :: IO ()
main = when False $ do -- skip this test until we start using GHC 8.4.4 or later for integration tests
  let ghcVer = "8.4.4"
      fp = concat
        [ ".ghc.environment."
        , arch
        , "-"
        , os
        , "-"
        , ghcVer
        ]
  writeFile "stack.yaml" $ "snapshot: ghc-" ++ ghcVer
  bracket_
    (writeFile fp "This is an invalid GHC environment file")
    (removeFile fp) $ do
      envFile <- canonicalizePath fp
      setEnv "GHC_ENVIRONMENT" envFile
      stack ["clean"]
      stack ["build"]
      stack ["runghc", "Main.hs"]
