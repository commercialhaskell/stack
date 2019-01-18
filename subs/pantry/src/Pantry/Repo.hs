{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pantry.Repo
  ( fetchRepos
  , getRepo
  , getRepoKey
  ) where

import Pantry.Types
import Pantry.Archive
import Pantry.Storage
import RIO
import Path.IO (resolveFile')
import RIO.FilePath ((</>))
import RIO.Directory (doesDirectoryExist)
import RIO.Process
import Database.Persist (Entity (..))
import qualified RIO.Text as T
import System.Console.ANSI (hSupportsANSIWithoutEmulation)
import System.Permissions (osIsWindows)

fetchRepos
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => [(Repo, PackageMetadata)]
  -> RIO env ()
fetchRepos pairs = do
  -- TODO be more efficient, group together shared archives
  for_ pairs $ uncurry getRepo

getRepoKey
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Repo
  -> PackageMetadata
  -> RIO env TreeKey
getRepoKey repo pm = packageTreeKey <$> getRepo repo pm -- potential optimization

getRepo
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Repo
  -> PackageMetadata
  -> RIO env Package
getRepo repo pm =
  withCache $ getRepo' repo pm
  where
    withCache
      :: RIO env Package
      -> RIO env Package
    withCache inner = do
      mtid <- withStorage (loadRepoCache repo (repoSubdir repo))
      case mtid of
        Just tid -> withStorage $ loadPackageById (PLIRepo repo pm) tid
        Nothing -> do
          package <- inner
          withStorage $ do
            ment <- getTreeForKey $ packageTreeKey package
            case ment of
              Nothing -> error $ "invariant violated, Tree not found: " ++ show (packageTreeKey package)
              Just (Entity tid _) -> storeRepoCache repo (repoSubdir repo) tid
          pure package

getRepo'
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Repo
  -> PackageMetadata
  -> RIO env Package
getRepo' repo@(Repo url commit repoType' subdir) pm =
  withSystemTempDirectory "get-repo" $
  \tmpdir -> withWorkingDir tmpdir $ do
    let suffix = "cloned"
        dir = tmpdir </> suffix
        tarball = tmpdir </> "foo.tar"

    let (commandName, resetArgs, submoduleArgs, archiveArgs) =
          case repoType' of
            RepoGit ->
              ( "git"
              , ["reset", "--hard", T.unpack commit]
              , Just ["submodule", "update", "--init", "--recursive"]
              , ["-c", "core.autocrlf=false", "archive", "-o", tarball, "HEAD"]
              )
            RepoHg ->
              ( "hg"
              , ["update", "-C", T.unpack commit]
              , Nothing
              , ["archive", tarball, "-X", ".hg_archival.txt"]
              )

    let runCommand args = void $ proc commandName args readProcess_

    logInfo $ "Cloning " <> display commit <> " from " <> display url
    runCommand ("clone" : [T.unpack url, suffix])
    -- On Windows 10, an upstream issue with the `git clone` command means that
    -- command clears, but does not then restore, the
    -- ENABLE_VIRTUAL_TERMINAL_PROCESSING flag for native terminals. The
    -- folowing hack re-enables the lost ANSI-capability.
    when osIsWindows $ void $ liftIO $ hSupportsANSIWithoutEmulation stdout
    created <- doesDirectoryExist dir
    unless created $ throwIO $ FailedToCloneRepo repo

    withWorkingDir dir $ do
      runCommand resetArgs
      traverse_ runCommand submoduleArgs
      -- On Windows 10, an upstream issue with the `git submodule` command means
      -- that command clears, but does not then restore, the
      -- ENABLE_VIRTUAL_TERMINAL_PROCESSING flag for native terminals. The
      -- folowing hack re-enables the lost ANSI-capability.
      when osIsWindows $ void $ liftIO $ hSupportsANSIWithoutEmulation stdout
      runCommand archiveArgs
    abs' <- resolveFile' tarball
    getArchive
      (PLIRepo repo pm)
      Archive
        { archiveLocation = ALFilePath $ ResolvedPath
            { resolvedRelative = RelFilePath $ T.pack tarball
            , resolvedAbsolute = abs'
            }
        , archiveHash = Nothing
        , archiveSize = Nothing
        , archiveSubdir = subdir
        }
      pm
