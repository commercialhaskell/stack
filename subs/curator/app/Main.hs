{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import Curator hiding (Snapshot)
import Data.Yaml (encodeFile, decodeFileThrow)
import Options.Generic (ParseRecord, getRecord)
import Path.IO (resolveFile', resolveDir')
import RIO.Process

data CuratorOptions
  = Update
  | Constraints
  | SnapshotIncomplete
  | Snapshot
  | Unpack
  | Build
  deriving (Eq, Show, Generic)

instance ParseRecord CuratorOptions

main :: IO ()
main = runPantryApp $
  getRecord "curator" >>= \case
    Update ->
      update
    Constraints ->
      constraints
    SnapshotIncomplete ->
      snapshotIncomplete
    Snapshot ->
      snapshot
    Unpack ->
      unpackFiles
    Build ->
      build

update :: RIO PantryApp ()
update = do
  void $ updateHackageIndex $ Just "Updating hackage index"

constraints :: RIO PantryApp ()
constraints = do
  logInfo "Writing constraints.yaml"
  loadStackageConstraints "build-constraints.yaml" >>= liftIO . encodeFile "constraints.yaml"

snapshotIncomplete :: RIO PantryApp ()
snapshotIncomplete = do
  logInfo "Writing snapshot-incomplete.yaml"
  decodeFileThrow "constraints.yaml" >>= \constraints' ->
    makeSnapshot constraints' "my-test-snapshot-2" >>=
    liftIO . encodeFile "snapshot-incomplete.yaml"

snapshot :: RIO PantryApp ()
snapshot = do
  logInfo "Writing snapshot.yaml"
  incomplete <- loadPantrySnapshotLayerFile "snapshot-incomplete.yaml"
  complete <- completeSnapshotLayer incomplete
  liftIO $ encodeFile "snapshot.yaml" complete

unpackFiles :: RIO PantryApp ()
unpackFiles = do
  logInfo "Unpacking files"
  let fp = "snapshot.yaml"
  abs' <- resolveFile' fp
  snapshot' <- loadSnapshot $ SLFilePath $ ResolvedPath (RelFilePath (fromString fp)) abs'
  constraints' <- decodeFileThrow "constraints.yaml"
  dest <- resolveDir' "unpack-dir"
  unpackSnapshot constraints' snapshot' dest

build :: RIO PantryApp ()
build = do
  logInfo "Building"
  withWorkingDir "unpack-dir" $ proc
    "stack"
    (words "build --test --bench --no-rerun-tests --no-run-benchmarks --haddock")
    runProcess_

loadPantrySnapshotLayerFile :: FilePath -> RIO PantryApp RawSnapshotLayer
loadPantrySnapshotLayerFile fp = do
  abs' <- resolveFile' fp
  eres <- loadSnapshotLayer $ SLFilePath (ResolvedPath (RelFilePath (fromString fp)) abs')
  case eres of
    Left x -> error $ "should not happen: " ++ show (fp, x)
    Right (x, _) -> pure x
