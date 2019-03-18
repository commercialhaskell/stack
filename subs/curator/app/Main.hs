{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
import Curator hiding (Snapshot)
import Data.Yaml (encodeFile, decodeFileThrow)
import Options.Applicative hiding (action)
import qualified Pantry
import Path.IO (resolveFile', resolveDir')
import qualified RIO.ByteString.Lazy as BL
import RIO.List (stripPrefix)
import qualified RIO.Map as Map
import RIO.PrettyPrint
import RIO.PrettyPrint.StylesUpdate
import RIO.Process
import qualified RIO.Text as T
import RIO.Time

data CuratorOptions
  = Update
  | CheckTargetAvailable Target
  | Constraints
  | SnapshotIncomplete
  | Snapshot
  | CheckSnapshot
  | Unpack
  | Build Int
  | UploadDocs Target
  | UploadGithub Target
  | HackageDistro Target

opts :: Parser CuratorOptions
opts = subparser
        ( simpleCmd "update" Update "Update Pantry databse from Hackage"
       <> targetCmd "checktargetavailable" CheckTargetAvailable "Check if target snapshot isn't yet on Github"
       <> simpleCmd "constraints" Constraints "Generate constraints file from build-constraints.yaml"
       <> simpleCmd "snapshotincomplete" SnapshotIncomplete "Generate incomplete snapshot"
       <> simpleCmd "snapshot" Snapshot "Complete locations in incomplete snapshot"
       <> simpleCmd "checksnapshot" CheckSnapshot "Check snapshot consistency"
       <> simpleCmd "unpack" Unpack "Unpack snapshot packages and create a Stack project for it"
       <> command "build" (info (buildCmd <**> helper)
                           (progDesc "Build Stack project for a Stackage snapshot"))
       <> targetCmd "uploaddocs" UploadDocs "Upload documentation to an S3 bucket"
       <> targetCmd "uploadgithub" UploadGithub "Commit and push snapshot definition to Github repository"
       <> targetCmd "hackagedistro" HackageDistro "Upload list of snapshot packages on Hackage as a distro"
        )
  where
    simpleCmd nm constr desc = command nm (info (pure constr) (progDesc desc))
    targetCmd nm constr desc =
      command nm (info (constr <$> target <**> helper) (progDesc desc))
    target = argument (nightly <|> lts) ( metavar "TARGET"
                                       <> help "Target Stackage snapshot 'lts-MM.NN' or 'nightly-YYYY-MM-DD'"
                                        )
    nightly = maybeReader $ \s -> do
      s' <- stripPrefix "nightly-" s
      TargetNightly <$> parseTimeM False defaultTimeLocale "%Y-%m-%d" s'
    lts = maybeReader $ \s -> do
      s' <- stripPrefix "lts-" s
      case break (== '.') s' of
        (major, '.':minor) -> TargetLts <$> readMaybe major <*> readMaybe minor
        _ -> Nothing
    buildCmd = Build <$> argument auto ( help "Number of jobs to run Stackage build with"
                                      <> showDefault
                                      <> value 1
                                      <> metavar "JOBS"
                                       )

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
    Build jobs ->
      build jobs
    UploadDocs target ->
      uploadDocs' target
    UploadGithub target ->
      uploadGithub' target
    HackageDistro target ->
      hackageDistro target

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
  liftIO $ encodeFile snapshotFilename complete

loadSnapshotYaml :: RIO PantryApp Pantry.RawSnapshot
loadSnapshotYaml = do
  abs' <- resolveFile' snapshotFilename
  loadSnapshot $ SLFilePath $
    ResolvedPath (RelFilePath (fromString snapshotFilename)) abs'

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

unpackDir :: FilePath
unpackDir = "unpack-dir"

unpackFiles :: RIO PantryApp ()
unpackFiles = do
  logInfo "Unpacking files"
  abs' <- resolveFile' snapshotFilename
  snapshot' <- loadSnapshot $ SLFilePath $
               ResolvedPath (RelFilePath (fromString snapshotFilename)) abs'
  constraints' <- decodeFileThrow "constraints.yaml"
  dest <- resolveDir' unpackDir
  unpackSnapshot constraints' snapshot' dest

build :: Int -> RIO PantryApp ()
build jobs = do
  logInfo "Building"
  withWorkingDir unpackDir $ proc
    "stack"
    (words $ "build --test --bench --test-suite-timeout=600 --no-rerun-tests --no-run-benchmarks --haddock --color never --jobs=" ++ show jobs)
    runProcess_

hackageDistro :: Target -> RIO PantryApp ()
hackageDistro target = do
  logInfo "Uploading Hackage distro for snapshot.yaml"
  snapshot' <- loadSnapshotYaml
  let packageVersions =
        Map.mapMaybe (snapshotVersion . rspLocation) (rsPackages snapshot')
  uploadHackageDistro target packageVersions

uploadDocs' :: Target -> RIO PantryApp ()
uploadDocs' target = do
  docsDir <- fmap (T.unpack . T.dropSuffix "\n" . decodeUtf8Lenient . BL.toStrict) $
    withWorkingDir unpackDir $ proc "stack" (words "path --local-doc-root") readProcessStdout_
  logInfo "Uploading docs to S3"
  let bucket = "next.haddocks.stackage.org"
      prefix = utf8BuilderToText $
        case target of
          TargetNightly day ->
            let date = formatTime defaultTimeLocale (iso8601DateFormat Nothing) day
            in "nightly-" <> fromString date
          TargetLts x y ->
            "lts-" <> display x <> "." <> display y
  uploadDocs docsDir prefix bucket

uploadGithub' :: Target -> RIO PantryApp ()
uploadGithub' target = do
  logInfo "Uploading snapshot definition to Github"
  uploadGithub target

loadPantrySnapshotLayerFile :: FilePath -> RIO PantryApp RawSnapshotLayer
loadPantrySnapshotLayerFile fp = do
  abs' <- resolveFile' fp
  eres <- loadSnapshotLayer $ SLFilePath (ResolvedPath (RelFilePath (fromString fp)) abs')
  case eres of
    Left x -> error $ "should not happen: " ++ show (fp, x)
    Right (x, _) -> pure x
