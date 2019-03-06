{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import Curator hiding (Snapshot)
import Data.Yaml (encodeFile, decodeFileThrow)
import Options.Applicative hiding (action)
import qualified Pantry
import Path.IO (resolveFile', resolveDir')
import RIO.List (stripPrefix)
import RIO.PrettyPrint
import RIO.PrettyPrint.StylesUpdate
import RIO.Process
import RIO.Time

data CuratorOptions
  = Update
  | CheckTargetAvailable Target
  | Constraints
  | SnapshotIncomplete
  | Snapshot
  | CheckSnapshot
  | Unpack
  | Build

opts :: Parser CuratorOptions
opts = subparser
        ( simpleCmd "update" Update "Update Pantry databse from Hackage"
       <> command "checktargetavailable" (info (CheckTargetAvailable <$> target)
                                          (progDesc "Check if target snapshot isn't yet on Github"))
       <> simpleCmd "constraints" Constraints "Generate constraints file from build-constraints.yaml"
       <> simpleCmd "snapshotincomplete" SnapshotIncomplete "Generate incomplete snapshot"
       <> simpleCmd "snapshot" Snapshot "Complete locations in incomplete snapshot"
       <> simpleCmd "checksnapshot" CheckSnapshot "Check snapshot consistency"
       <> simpleCmd "unpack" Unpack "Unpack snapshot packages and create a Stack project for it"
       <> simpleCmd "build" Build "Build Stack project for a Stackage snapshot"
        )
  where
    simpleCmd nm constr desc = command nm (info (pure constr) (progDesc desc))
    target = argument (nightly <|> lts) (metavar "TARGET")
    nightly = maybeReader $ \s -> do
      s' <- stripPrefix "nightly-" s
      TargetNightly <$> parseTimeM False defaultTimeLocale "%Y-%m-%d" s'
    lts = maybeReader $ \s -> do
      s' <- stripPrefix "lts-" s
      case break (== '.') s' of
        (major, '.':minor) -> TargetLts <$> readMaybe major <*> readMaybe minor
        _ -> Nothing

allOpts :: ParserInfo CuratorOptions
allOpts = info (opts <**> helper)
  ( fullDesc
 <> progDesc "Special utilities for Stackage curators"
 <> header "curator - Stackage curator tool" )

main :: IO ()
main = runPantryApp $
  liftIO (execParser allOpts) >>= \case
    Update ->
      update
    CheckTargetAvailable t ->
      checkTargetAvailable t
    Constraints ->
      constraints
    SnapshotIncomplete ->
      snapshotIncomplete
    Snapshot ->
      snapshot
    CheckSnapshot ->
      checkSnapshot
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

loadSnapshotYaml :: RIO PantryApp Pantry.RawSnapshot
loadSnapshotYaml = do
  let fp = "snapshot.yaml"
  abs' <- resolveFile' fp
  loadSnapshot $ SLFilePath $ ResolvedPath (RelFilePath (fromString fp)) abs'

checkSnapshot :: RIO PantryApp ()
checkSnapshot = do
  logInfo "Checking dependencies in snapshot.yaml"
  decodeFileThrow "constraints.yaml" >>= \constraints' -> do
    snapshot' <- loadSnapshotYaml
    withFixedColorTerm $ checkDependencyGraph constraints' snapshot'

data FixedColorTermApp = FixedColorTermApp
    { fctApp :: PantryApp
    , fctWidth :: Int
    }

pantryAppL :: Lens' FixedColorTermApp PantryApp
pantryAppL = lens fctApp (\s a -> s{ fctApp = a})

instance HasLogFunc FixedColorTermApp where
  logFuncL = pantryAppL.logFuncL

instance HasStylesUpdate FixedColorTermApp where
  stylesUpdateL = lens (const $ StylesUpdate []) (\s _ -> s)

instance HasTerm FixedColorTermApp where
  useColorL = lens (const True) (\s _ -> s)
  termWidthL = lens fctWidth (\s w -> s{ fctWidth = w })

instance HasPantryConfig FixedColorTermApp where
  pantryConfigL = pantryAppL.pantryConfigL

instance HasProcessContext FixedColorTermApp where
  processContextL = pantryAppL.processContextL

withFixedColorTerm :: RIO FixedColorTermApp a -> RIO PantryApp a
withFixedColorTerm action = do
  app <- ask
  runRIO (FixedColorTermApp app defaultTerminalWidth) action

defaultTerminalWidth :: Int
defaultTerminalWidth = 100

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
    (words "build --test --bench --test-suite-timeout=600 --no-rerun-tests --no-run-benchmarks --haddock --color never")
    runProcess_

loadPantrySnapshotLayerFile :: FilePath -> RIO PantryApp RawSnapshotLayer
loadPantrySnapshotLayerFile fp = do
  abs' <- resolveFile' fp
  eres <- loadSnapshotLayer $ SLFilePath (ResolvedPath (RelFilePath (fromString fp)) abs')
  case eres of
    Left x -> error $ "should not happen: " ++ show (fp, x)
    Right (x, _) -> pure x
