{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import Curator
import Data.Yaml (encodeFile, decodeFileThrow)
import Path.IO (resolveFile', resolveDir')
import RIO.Process

main :: IO ()
main = runPantryApp $ do
  -- each of these should be separate commands

  -- update Hackage index
  do
    updateHackageIndex $ Just "Running snapshot curator tool"

  -- write constraints
  do
    logInfo "Writing constraints.yaml"
    loadStackageConstraints "build-constraints.yaml" >>= liftIO . encodeFile "constraints.yaml"

  -- create snapshot
  do
    logInfo "Writing snapshot-incomplete.yaml"
    decodeFileThrow "constraints.yaml" >>= \constraints ->
      makeSnapshot constraints "my-test-snapshot" >>=
      liftIO . encodeFile "snapshot-incomplete.yaml"

  -- complete snapshot
  do
    logInfo "Writing snapshot.yaml"
    incomplete <- loadPantrySnapshotFile "snapshot-incomplete.yaml"
    complete <- completeSnapshot incomplete
    liftIO $ encodeFile "snapshot.yaml" complete

  do
    logInfo "Unpacking files"
    snapshot <- loadPantrySnapshotFile "snapshot.yaml"
    constraints <- decodeFileThrow "constraints.yaml"
    dest <- resolveDir' "unpack-dir"
    unpackSnapshot constraints snapshot dest

  do
    logInfo "Building"
    withWorkingDir "unpack-dir" $ proc 
      "stack"
      (words "build --test --bench --no-rerun-tests --no-run-benchmarks --haddock")
      runProcess_

loadPantrySnapshotFile fp = do
  abs' <- resolveFile' fp
  eres <- loadPantrySnapshot $ SLFilePath (ResolvedPath (RelFilePath (fromString fp)) abs') Nothing
  case eres of
    Left x -> error $ "should not happen: " ++ show (fp, x)
    Right (x, _, _) -> pure x