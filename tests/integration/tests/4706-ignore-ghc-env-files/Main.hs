-- Stack ignores GHC_ENVIRONMENT.
--
-- See: https://github.com/commercialhaskell/stack/issues/4706

import           Control.Exception ( bracket_ )
import           StackTest
import           System.Directory ( canonicalizePath, removeFile )
import           System.Environment ( setEnv )
import           System.Info (arch, os)

main :: IO ()
main = do
  let ghcVer = "9.10.3"
      fp = concat
        [ ".ghc.environment."
        , arch
        , "-"
        , os
        , "-"
        , ghcVer
        ]
  bracket_
    (writeFile fp "This is an invalid GHC environment file")
    (removeFile fp)
    ( do
        envFile <- canonicalizePath fp
        setEnv "GHC_ENVIRONMENT" envFile
        stack ["build"]
        stack ["runghc", "Main.hs"]
    )
