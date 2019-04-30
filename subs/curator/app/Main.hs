{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Curator hiding (Snapshot)
import Data.Yaml (encodeFile, decodeFileThrow)
import Network.HTTP.Client (httpLbs, newManager, parseUrlThrow, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Download (download)
import Options.Applicative.Simple hiding (action)
import qualified Pantry
import Path (toFilePath)
import Path.IO (doesFileExist, removeFile, resolveFile', resolveDir')
import Paths_curator (version)
import qualified RIO.ByteString.Lazy as BL
import RIO.List (stripPrefix)
import qualified RIO.Map as Map
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
      addCommand "check-target-available"
                 "Check if target snapshot isn't yet on Github"
                 checkTargetAvailable
                 parseTarget
      addCommand "constraints"
                 "Generate constraints file from build-constraints.yaml"
                 constraints
                 parseTarget
      addCommand "snapshot-incomplete"
                 "Generate incomplete snapshot"
                 (const snapshotIncomplete)
                 (pure ())
      addCommand "snapshot"
                 "Complete locations in incomplete snapshot"
                 (const snapshot)
                 (pure ())
      addCommand "check-snapshot"
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
      addCommand "upload-docs"
                 "Upload documentation to an S3 bucket"
                 uploadDocs'
                 parseTarget
      addCommand "upload-github"
                 "Commit and push snapshot definition to Github repository"
                 uploadGithub'
                 parseTarget
      addCommand "hackage-distro"
                 "Upload list of snapshot packages on Hackage as a distro"
                 hackageDistro
                 parseTarget
    parseTarget =
      option (nightly <|> lts) ( long "target"
                              <> metavar "TARGET"
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
    parseJobs = option auto ( long "jobs"
                           <> metavar "JOBS"
                           <> showDefault
                           <> value 1
                           <> help "Number of jobs to run Stackage build with"
                              )

main :: IO ()
main = runPantryApp $ do
  ((), runCmd) <- liftIO options
  runCmd

update :: RIO PantryApp ()
update = do
  void $ updateHackageIndex $ Just "Updating hackage index"

constraints :: Target -> RIO PantryApp ()
constraints target =
  case target of
    TargetLts x y | y > 0 -> do
      let prev = y - 1
          url = concat [ "https://raw.githubusercontent.com/" ++ constraintsRepo ++ "/master/lts/"
                        , show x
                        , "/"
                        , show prev
                        , ".yaml"
                        ]
      logInfo $ "Will reuse constraints.yaml from lts-" <> display x <> "." <> display prev
      req <- parseUrlThrow url
      constraintsPath <- resolveFile' constraintsFilename
      exists <- doesFileExist constraintsPath
      when exists $ do
        logWarn "Local constraints file will be deleted before downloading reused constraints"
        removeFile constraintsPath
      downloaded <- download req constraintsPath
      unless downloaded $
        error $ "Could not download constraints.yaml from " <> url
    _ -> do
      buildConstraintsPath <- resolveFile' "build-constraints.yaml"
      exists <- doesFileExist buildConstraintsPath
      stackageConstraints <- if exists
        then do
          logInfo "Reusing already existing file build-constraints.yaml"
          loadStackageConstraints $ toFilePath buildConstraintsPath
        else do
          logInfo $ "Downloading build-constraints from commercialhaskell/stackage"
          req <- parseUrlThrow "https://raw.githubusercontent.com/commercialhaskell/stackage/master/build-constraints.yaml"
          man <- liftIO $ newManager tlsManagerSettings
          liftIO (httpLbs req man) >>=
            loadStackageConstraintsBs . BL.toStrict . responseBody
      logInfo "Writing constraints.yaml"
      liftIO $ encodeFile constraintsFilename stackageConstraints

snapshotIncomplete :: RIO PantryApp ()
snapshotIncomplete = do
  logInfo "Writing snapshot-incomplete.yaml"
  decodeFileThrow constraintsFilename >>= \constraints' ->
    makeSnapshot constraints' >>=
    liftIO . encodeFile "snapshot-incomplete.yaml"

snapshot :: RIO PantryApp ()
snapshot = do
  logInfo "Writing snapshot.yaml"
  incomplete <- loadPantrySnapshotLayerFile "snapshot-incomplete.yaml"
  complete <- completeSnapshotLayer incomplete
  liftIO $ encodeFile snapshotFilename complete

loadSnapshotYaml :: RIO PantryApp Pantry.Snapshot
loadSnapshotYaml = do
  abs' <- resolveFile' snapshotFilename
  let sloc = SLFilePath $
        ResolvedPath (RelFilePath (fromString snapshotFilename)) abs'
  (snap, _, _) <- loadAndCompleteSnapshot sloc Map.empty Map.empty
  pure snap

checkSnapshot :: RIO PantryApp ()
checkSnapshot = do
  logInfo "Checking dependencies in snapshot.yaml"
  decodeFileThrow constraintsFilename >>= \constraints' -> do
    snapshot' <- loadSnapshotYaml
    checkDependencyGraph constraints' snapshot'

unpackDir :: FilePath
unpackDir = "unpack-dir"

unpackFiles :: RIO PantryApp ()
unpackFiles = do
  logInfo "Unpacking files"
  snapshot' <- loadSnapshotYaml
  constraints' <- decodeFileThrow constraintsFilename
  dest <- resolveDir' unpackDir
  unpackSnapshot constraints' snapshot' dest

build :: Int -> RIO PantryApp ()
build jobs = do
  logInfo "Building"
  withWorkingDir unpackDir $ proc
    "stack"
    (words $ "build --test --bench --test-suite-timeout=600 --no-rerun-tests --no-run-benchmarks --haddock --color never --no-interleaved-output --jobs=" ++ show jobs)
    runProcess_

hackageDistro :: Target -> RIO PantryApp ()
hackageDistro target = do
  logInfo "Uploading Hackage distro for snapshot.yaml"
  snapshot' <- loadSnapshotYaml
  let packageVersions =
        Map.mapMaybe (snapshotVersion . spLocation) (snapshotPackages snapshot')
  uploadHackageDistro target packageVersions

uploadDocs' :: Target -> RIO PantryApp ()
uploadDocs' target = do
  docsDir <- fmap (T.unpack . T.dropSuffix "\n" . decodeUtf8Lenient . BL.toStrict) $
    withWorkingDir unpackDir $ proc "stack" (words "path --local-doc-root") readProcessStdout_
  logInfo "Uploading docs to S3"
  let bucket = "next.haddock.stackage.org"
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
    Right x -> pure x
