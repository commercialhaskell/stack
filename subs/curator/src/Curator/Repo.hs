{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Curator.Repo
    ( checkTargetAvailable
    , uploadGithub
    ) where

import Conduit
import Curator.Constants
import Curator.Types
import Path
import Path.IO
import RIO
import RIO.Process
import RIO.Time

-- | Check if the given target is already used in the Github repos
checkTargetAvailable ::
       ( HasLogFunc env
       , HasProcessContext env
       , MonadReader env m
       , MonadIO m
       , MonadThrow m
       )
    => Target
    -> m ()
checkTargetAvailable t = do
    void $ checkoutConstraintsRepo t
    void $ checkoutSnapshotsRepo t

-- | Upload snapshot definition to Github repository
uploadGithub ::
       (HasLogFunc env, HasProcessContext env)
    => Target
    -> RIO env ()
uploadGithub target = do
    upload checkoutConstraintsRepo constraintsFilename
    upload checkoutSnapshotsRepo snapshotFilename
  where
    upload checkout srcFilename = do
        (git, snapshotFile, snapshotName) <- checkout target

        createDirIfMissing True $ parent snapshotFile
        runConduitRes $ sourceFile srcFilename .| sinkFile (toFilePath snapshotFile)

        void $ git ["add", toFilePath snapshotFile]
        void $ git ["commit", "-m", "Checking in " ++ snapshotName]
        void $ git ["push", "origin", "HEAD:master"]

checkoutSnapshotsRepo ::
       ( HasLogFunc env
       , HasProcessContext env
       , MonadReader env m
       , MonadIO m
       , MonadThrow m
       )
    => Target
    -> m ([String] -> m (), Path Abs File, String)
checkoutSnapshotsRepo t = checkoutRepo t dir url
  where
    url = "git@github.com:commercialhaskell/stackage-next"
    dir = $(mkRelDir "stackage-snapshots")

checkoutConstraintsRepo ::
       ( HasLogFunc env
       , HasProcessContext env
       , MonadReader env m
       , MonadIO m
       , MonadThrow m
       )
    => Target
    -> m ([String] -> m (), Path Abs File, String)
checkoutConstraintsRepo t = checkoutRepo t dir url
  where
    url = "git@github.com:commercialhaskell/stackage-constraints-next"
    dir = $(mkRelDir "stackage-constraints")

checkoutRepo ::
       ( HasLogFunc env
       , HasProcessContext env
       , MonadReader env m
       , MonadIO m
       , MonadThrow m
       )
    => Target
    -> Path Rel Dir
    -> String
    -> m ([String] -> m (), Path Abs File, String)
checkoutRepo target dirName repoUrl = do
    root <- fmap (</> $(mkRelDir "curator")) $ getAppUserDataDir "stackage"

    let repoDir = root </> dirName

        runIn wdir cmd args = do
            let wdir' = toFilePath wdir
            logInfo $ fromString wdir' <> ": " <> displayShow (cmd:args)
            withWorkingDir wdir' $ proc cmd args runProcess_

        git = runIn repoDir "git"

    (relSnapshotPath, snapshotName) <- case target of
        TargetNightly d -> do
            let (year, month, day) = toGregorian d
            year' <- parseRelDir (show year)
            month' <- parseRelDir (show month)
            day' <- parseRelFile (show day)
            fname <- day' <.> "yaml"
            pure ( $(mkRelDir "nightly") </> year' </> month' </> fname
                 , "nightly-" <> show year <> "-" <> show month <> "-" <> show day
                 )
        TargetLts x y -> do
            major <- parseRelDir (show x)
            minor <- parseRelFile (show y)
            fname <- minor <.> "yaml"
            pure ( $(mkRelDir "lts") </> major </> fname
                 , "lts-" <> show x <> "-" <> show y
                 )

    let destSnapshotFile = repoDir </> relSnapshotPath

    exists <- doesDirExist repoDir
    if exists
        then do
            git ["fetch"]
            git ["checkout", "origin/master"]
        else do
            createDirIfMissing True $ parent repoDir
            runIn $(mkRelDir ".") "git" ["clone", repoUrl, toFilePath repoDir]

    whenM (liftIO $ doesFileExist destSnapshotFile)
        $ error $ "File already exists: " ++ toFilePath destSnapshotFile

    return (git, destSnapshotFile, snapshotName)
