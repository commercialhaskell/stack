-- Test transitive cross-package Backpack chains: logger-sig (indefinite,
-- sig: Logger) depends on str-sig (indefinite, sig: Str). When consumer mixes
-- in logger-sig, both Logger and Str holes must be filled transitively.

import Control.Monad ( unless, when )
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
  stackCheckStderr ["build"] expectNoCandidateWarning

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

  -- Rebuild should succeed (no stale CInst state)
  stackCheckStderr ["build"] $ \err -> do
    expectNoCandidateWarning err
    unless (any (`isInfixOf` err) ["logger-sig", "Compiling Logger"]) $
      error $
           "Expected Logger.hsig change to rebuild logger-sig, got stderr: "
        ++ show err

  -- Verify output still correct after rebuild
  stackCheckStdout ["exec", "consumer-demo"] $ \out ->
    unless ("[LOG] Hello from transitive chain" `isInfixOf` out) $
      error $ "Expected '[LOG] Hello from transitive chain' after rebuild, got: "
            ++ show out

expectNoCandidateWarning :: String -> IO ()
expectNoCandidateWarning err =
  when ("Unable to find a known candidate for the Cabal entry" `isInfixOf` err) $
    error $ "Unexpected known candidate warning in stderr: " ++ show err

replaceFile :: FilePath -> String -> IO ()
replaceFile file contents = do
  -- The integration harness may symlink fixture files on Unix, so remove the
  -- temporary path before writing to avoid mutating the source fixture.
  removeFile file
  writeFile file contents
