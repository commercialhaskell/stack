{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Curator.Repo
    ( checkTargetAvailable
    ) where

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
checkTargetAvailable = void . checkoutRepo

checkoutRepo ::
       ( HasLogFunc env
       , HasProcessContext env
       , MonadReader env m
       , MonadIO m
       , MonadThrow m
       )
    => Target
    -> m ([String] -> m (), Path Abs File)
checkoutRepo target = do
    root <- fmap (</> $(mkRelDir "curator")) $ getAppUserDataDir "stackage"

    let repoDir = root </> $(mkRelDir "stackage-snapshots")

        runIn wdir cmd args = do
            let wdir' = toFilePath wdir
            logInfo $ fromString wdir' <> ": " <> displayShow (cmd:args)
            withWorkingDir wdir' $ proc cmd args runProcess_

        git = runIn repoDir "git"

    relSnapshotPath <- case target of
        TargetNightly d -> do
            let (year, month, day) = toGregorian d
            year' <- parseRelDir (show year)
            month' <- parseRelDir (show month)
            day' <- parseRelFile (show day)
            fname <- day' <.> "yaml"
            pure $ $(mkRelDir "nightly") </> year' </> month' </> fname
        TargetLts x y -> do
            major <- parseRelDir (show x)
            minor <- parseRelFile (show y)
            fname <- minor <.> "yaml"
            pure $ $(mkRelDir "lts") </> major </> fname

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

    return (git, destSnapshotFile)
  where
    repoUrl = "git@github.com:commercialhaskell/stackage-next"
