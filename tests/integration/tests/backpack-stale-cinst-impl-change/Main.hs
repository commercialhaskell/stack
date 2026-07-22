-- Regression test for stale Backpack instantiation state when the concrete
-- implementation package changes. The signature packages are clean, but their
-- concrete instantiations depend on impl-pkg's modules.

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

  replaceFile "impl-pkg/src/Str.hs" $ unlines
    [ "module Str where"
    , ""
    , "greeting :: String"
    , "greeting = \"Hello from changed implementation\""
    ]

  -- Rebuild should succeed because only the concrete implementation changed.
  stackCheckStderr ["build"] $ \err ->
    expectBuildLine "impl-pkg" "Compiling Str" err

  -- Verify output reflects the changed implementation after rebuild
  stackCheckStdout ["exec", "consumer-demo"] $ \out ->
    unless ("[LOG] Hello from changed implementation" `isInfixOf` out) $
      error $
           "Expected changed impl-pkg output after rebuild, got: "
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
