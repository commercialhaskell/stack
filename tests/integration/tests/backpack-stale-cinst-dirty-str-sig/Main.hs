-- Regression test for stale Backpack instantiation state when the deepest
-- indefinite signature package is dirty. str-sig must rebuild its indefinite
-- CLib, and the str-sig CInst must use that rebuilt unit.

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

  replaceFile "str-sig/package.yaml" $ unlines
    [ "spec-version: 0.36.0"
    , ""
    , "name: str-sig"
    , ""
    , "dependencies:"
    , "- base"
    , ""
    , "library:"
    , "  source-dirs: src"
    , "  ghc-options:"
    , "  - -Wall"
    , "  signatures:"
    , "  - Str"
    ]

  -- Rebuild should succeed because the CInst waits for the rebuilt indefinite
  -- str-sig CLib instead of trying to use the stale installed one.
  stackCheckStderr ["build"] $ \err -> do
    expectBuildLine "str-sig" "Compiling Str[sig]" err
    expectBuildLine "str-sig" "Str = impl-pkg" err

  -- Verify output still correct after rebuilding str-sig and its CInst.
  stackCheckStdout ["exec", "consumer-demo"] $ \out ->
    unless ("[LOG] Hello from transitive chain" `isInfixOf` out) $
      error $
           "Expected original output after rebuilding str-sig, got: "
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
