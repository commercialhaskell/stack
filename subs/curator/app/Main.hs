{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Curator hiding (Snapshot)
import Data.Yaml (encodeFile, decodeFileThrow)
import Options.Applicative.Simple hiding (action)
import qualified Pantry
import Path.IO (resolveFile', resolveDir')
import Paths_curator (version)
import qualified RIO.ByteString.Lazy as BL
import RIO.List (stripPrefix)
import qualified RIO.Map as Map
import RIO.PrettyPrint
import RIO.PrettyPrint.StylesUpdate
import RIO.Process
import qualified RIO.Text as T
import RIO.Time

options :: IO ((), RIO PantryApp ())
options =
    simpleOptions $(simpleVersion version)
                  "curator - Stackage curator tool"
                  "Special utilities for Stackage curators"
                  (pure ())
                  commands
  where
    commands = do
      addCommand "update"
                 "Update Pantry databse from Hackage"
                 (const update)
                 (pure ())
      addCommand "checktargetavailable"
                 "Check if target snapshot isn't yet on Github"
                 checkTargetAvailable
                 parseTarget
      addCommand "constraints"
                 "Generate constraints file from build-constraints.yaml"
                 (const constraints)
                 (pure ())
      addCommand "snapshotincomplete"
                 "Generate incomplete snapshot"
                 (const snapshotIncomplete)
                 (pure ())
      addCommand "snapshot"
                 "Complete locations in incomplete snapshot"
                 (const snapshot)
                 (pure ())
      addCommand "checksnapshot"
                 "Check snapshot consistency"
                 (const checkSnapshot)
                 (pure ())
      addCommand "unpack"
                 "Unpack snapshot packages and create a Stack project for it"
                 (const unpackFiles)
                 (pure ())
      addCommand "build"
                 "Build Stack project for a Stackage snapshot"
                 build
                 parseJobs
      addCommand "uploaddocs"
                 "Upload documentation to an S3 bucket"
                 uploadDocs'
                 parseTarget
      addCommand "uploadgithub"
                 "Commit and push snapshot definition to Github repository"
                 uploadGithub'
                 parseTarget
      addCommand "hackagedistro"
                 "Upload list of snapshot packages on Hackage as a distro"
                 hackageDistro
                 parseTarget
    parseTarget =
      argument (nightly <|> lts) ( metavar "TARGET"
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
    parseJobs = argument auto ( help "Number of jobs to run Stackage build with"
                             <> showDefault
                             <> value 1
                             <> metavar "JOBS"
                              )

main :: IO ()
main = runPantryApp $ do
  ((), runCmd) <- liftIO options
  runCmd

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
