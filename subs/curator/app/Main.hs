{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import Curator
import Data.Yaml (encodeFile)
import Path.IO (resolveFile')

main :: IO ()
main = runPantryApp $ do
  -- each of these should be separate commands

  -- write constraints
  constraints <- loadStackageConstraints "build-constraints.yaml"
  liftIO $ encodeFile "constraints.yaml" constraints

  -- create snapshot
  makeSnapshot constraints "my-test-snapshot" >>=
    liftIO . encodeFile "snapshot-incomplete.yaml"

  -- complete snapshot
  let raw = "snapshot-incomplete.yaml"
  abs' <- resolveFile' raw
  let resolved = ResolvedPath (RelFilePath (fromString raw)) abs'
  loadPantrySnapshot (SLFilePath resolved Nothing) >>=
    either (\x -> error $ "should not happen: " ++ show x) (\(x, _, _) -> pure x) >>=
    completeSnapshot >>=
      liftIO . encodeFile "snapshot.yaml"