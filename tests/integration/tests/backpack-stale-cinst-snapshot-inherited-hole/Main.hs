-- Regression test for stale Backpack instantiation state when the final
-- Backpack consumer changes and one required hole is inherited from a snapshot
-- package. consumer-pkg mixes in only logger-sig; logger-sig depends on
-- str-sig, which is installed from a local snapshot archive. Stack must use the
-- source map, not just CombinedMap, to load str-sig's signature metadata.

import Control.Monad ( unless )
import Data.List ( isInfixOf )
import System.Directory ( createDirectoryIfMissing, removeFile )
import StackTest

main :: IO ()
main = do
  createDirectoryIfMissing True "snapshots"
  stack
    [ "--stack-yaml", "stack-bootstrap.yaml"
    , "sdist", "str-sig"
    , "--ignore-check"
    , "--tar-dir", "snapshots"
    ]

  -- Build all four packages. This exercises:
  -- 1. str-sig CLib (indefinite, typecheck-only)
  -- 2. impl-pkg CLib (concrete Str + Logger)
  -- 3. logger-sig CLib (indefinite, typecheck-only, inherits Str hole)
  -- 4. logger-sig CInst (fills BOTH Logger and Str holes)
  -- 5. consumer-pkg CLib + CExe
  stack ["build"]

  -- Verify the consumer executable calls through the transitive chain
  stackCheckStdout ["exec", "consumer-demo"] $ \out ->
    unless ("[LOG] Hello from transitive chain" `isInfixOf` out) $
      error $ "Expected '[LOG] Hello from transitive chain' in output, got: "
            ++ show out

  replaceFile "consumer-pkg/src/Consumer.hs" $ unlines
    [ "module Consumer where"
    , ""
    , "import LogHelper (greetWithLog)"
    , ""
    , "hello :: String"
    , "hello = greetWithLog ++ \" after consumer edit\""
    ]

  -- Rebuild should succeed because Stack loads logger-sig and its transitive
  -- snapshot-installed indefinite dependency str-sig as templates for the
  -- CInst task.
  stackCheckStderr ["build"] $ \err -> do
    expectBuildLine "logger-sig" "Str = impl-pkg" err
    expectBuildLine "consumer-pkg" "Compiling Consumer" err

  -- Verify output still correct after rebuild
  stackCheckStdout ["exec", "consumer-demo"] $ \out ->
    unless ("[LOG] Hello from transitive chain after consumer edit" `isInfixOf` out) $
      error $
           "Expected edited consumer-pkg output after rebuild, got: "
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
