-- Regression test for stale Backpack instantiation state when an intermediate
-- indefinite package's signature changes. The source touch keeps this focused
-- on stale CInst planning even before .hsig file tracking is fixed:
-- logger-sig is dirty, but its indefinite dependency str-sig is still clean
-- and already installed.

import Control.Monad ( unless )
import Data.List ( isInfixOf )
import System.Directory ( removeFile )
import StackTest

main :: IO ()
main = do
  -- Build all four packages. This exercises:
  -- 1. str-sig CLib (indefinite, typecheck-only)
  -- 2. impl-pkg CLib (concrete Str + Logger)
  -- 3. str-sig CInst (instantiation with impl-pkg's Str)
  -- 4. logger-sig CLib (indefinite, typecheck-only, inherits Str hole)
  -- 5. logger-sig CInst (fills BOTH Logger and Str holes)
  -- 6. consumer-pkg CLib + CExe
  stack ["build"]

  -- Verify the consumer executable calls through the transitive chain
  stackCheckStdout ["exec", "consumer-demo"] $ \out ->
    unless ("[LOG] Hello from transitive chain" `isInfixOf` out) $
      error $ "Expected '[LOG] Hello from transitive chain' in output, got: "
            ++ show out

  replaceFile "logger-sig/src/Logger.hsig" $ unlines
    [ "signature Logger where"
    , ""
    , "logMessage :: String -> String"
    , "loggerName :: String"
    ]
  replaceFile "logger-sig/src/LogHelper.hs" $ unlines
    [ "module LogHelper where"
    , ""
    , "import Str (greeting)"
    , "import Logger (logMessage)"
    , ""
    , "-- Force logger-sig dirty independently of .hsig tracking."
    , "greetWithLog :: String"
    , "greetWithLog = logMessage greeting"
    ]

  -- Rebuild should succeed because impl-pkg already exports loggerName.
  stackCheckStderr ["build"] $ \err ->
    expectBuildLine "logger-sig" "Compiling Logger[sig]" err

  -- Verify output still correct after rebuild
  stackCheckStdout ["exec", "consumer-demo"] $ \out ->
    unless ("[LOG] Hello from transitive chain" `isInfixOf` out) $
      error $ "Expected '[LOG] Hello from transitive chain' after rebuild, got: "
            ++ show out

replaceFile :: FilePath -> String -> IO ()
replaceFile file contents = do
  removeFile file
  writeFile file contents

expectBuildLine :: String -> String -> String -> IO ()
expectBuildLine package marker err =
  unless (any matches $ lines err) $
    error $
         "Expected build output line containing "
      ++ show package
      ++ " and "
      ++ show marker
      ++ ", got stderr: "
      ++ show err
 where
  matches line = package `isInfixOf` line && marker `isInfixOf` line
