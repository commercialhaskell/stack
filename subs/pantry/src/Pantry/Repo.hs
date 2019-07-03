{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pantry.Repo
  ( fetchReposRaw
  , fetchRepos
  , getRepo
  , getRepoKey
  , createRepoArchive
  , withRepoArchive
  , withRepo
  ) where

import Pantry.Types
import Pantry.Archive
import Pantry.Storage
import RIO
import Path.IO (resolveFile')
import RIO.FilePath ((</>))
import RIO.Directory (doesDirectoryExist)
import qualified RIO.Map as Map
import RIO.Process
import Database.Persist (Entity (..))
import qualified RIO.Text as T
import System.Console.ANSI (hSupportsANSIWithoutEmulation)
import System.IsWindows (osIsWindows)

fetchReposRaw
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => [(Repo, RawPackageMetadata)]
  -> RIO env ()
fetchReposRaw pairs = for_ pairs $ uncurry getRepo

fetchRepos
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => [(Repo, PackageMetadata)]
  -> RIO env ()
fetchRepos pairs = do
  -- TODO be more efficient, group together shared archives
  fetchReposRaw $ map (second toRawPM) pairs

getRepoKey
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Repo
  -> RawPackageMetadata
  -> RIO env TreeKey
getRepoKey repo rpm = packageTreeKey <$> getRepo repo rpm -- potential optimization

getRepo
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Repo
  -> RawPackageMetadata
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
        Just tid -> withStorage $ loadPackageById (RPLIRepo repo pm) tid
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
  -> RawPackageMetadata
  -> RIO env Package
getRepo' repo rpm = do
  withRepoArchive repo $ \tarball -> do
    abs' <- resolveFile' tarball
    getArchivePackage
      (RPLIRepo repo rpm)
      RawArchive
        { raLocation = ALFilePath $ ResolvedPath
            { resolvedRelative = RelFilePath $ T.pack tarball
            , resolvedAbsolute = abs'
            }
        , raHash = Nothing
        , raSize = Nothing
        , raSubdir = repoSubdir repo
        }
      rpm

-- | Fetch a repository and create a (temporary) tar archive from it. Pass the
-- path of the generated tarball to the given action.
withRepoArchive
  :: forall env a. (HasLogFunc env, HasProcessContext env)
  => Repo
  -> (FilePath -> RIO env a)
  -> RIO env a
withRepoArchive repo action =
  withSystemTempDirectory "with-repo-archive" $ \tmpdir -> do
    let tarball = tmpdir </> "foo.tar"
    createRepoArchive repo tarball
    action tarball

-- | Run a git command, setting appropriate environment variable settings. See
-- <https://github.com/commercialhaskell/stack/issues/3748>.
runGitCommand
  :: (HasLogFunc env, HasProcessContext env)
  => [String] -- ^ args
  -> RIO env ()
runGitCommand args =
  withModifyEnvVars go $
  void $ proc "git" args readProcess_
  where
    go = Map.delete "GIT_DIR"
       . Map.delete "GIT_CEILING_DIRECTORIES"
       . Map.delete "GIT_WORK_TREE"
       . Map.delete "GIT_INDEX_FILE"
       . Map.delete "GIT_OBJECT_DIRECTORY" -- possible optimization: set this to something Pantry controls
       . Map.delete "GIT_ALTERNATE_OBJECT_DIRECTORIES"

-- | Run an hg command
runHgCommand
  :: (HasLogFunc env, HasProcessContext env)
  => [String] -- ^ args
  -> RIO env ()
runHgCommand args = void $ proc "hg" args readProcess_

-- | Create a tarball containing files from a repository
createRepoArchive ::
     forall env. (HasLogFunc env, HasProcessContext env)
  => Repo
  -> FilePath -- ^ Output tar archive filename
  -> RIO env ()
createRepoArchive repo tarball = do
  withRepo repo $
    case repoType repo of
      RepoGit -> do
        runGitCommand
          ["-c", "core.autocrlf=false", "archive", "-o", tarball, "HEAD"]
        let forceLocal =
              if osIsWindows
                then " --force-local "
                else mempty
       -- also include submodules files: use `git submodule foreach`
       -- to execute `git archive` in each submodule and generate tar
       -- archive. This generated archive is extracted to a temporary
       -- folder and the files in them are added to the tarball
       -- referenced by the variable tarball in the haskell code. You
       -- could do this with GNU -A option, but that doesn't work with
       -- bsdtar which is present in MacOS. So we do this now using a
       -- temporary folder which is works for both GNU tar and bsdtar.
        runGitCommand
          [ "submodule"
          , "foreach"
          , "--recursive"
          , "git -c core.autocrlf=false archive --prefix=$displaypath/ -o bar.tar HEAD;" <>
            " rm -rf temp; mkdir temp; mv bar.tar temp/; tar -C temp " <>
            forceLocal <>
            " -xf temp/bar.tar; " <>
            "rm temp/bar.tar; tar -C temp " <>
            forceLocal <>
            " -rf " <>
            tarball <>
            " . ;"
          ]
        if osIsWindows
        then do
          (outputStdout, _) <- proc "tar" ["--force-local", "-tvf", tarball] readProcess_
          logError $ displayShow outputStdout
        else do
          (outputStdout, _) <- proc "tar" ["-tvf", tarball] readProcess_
          logError $ displayShow outputStdout
      RepoHg -> runHgCommand ["archive", tarball, "-X", ".hg_archival.txt"]


-- | Clone the repository and execute the action with the working
-- directory set to the repository root.
--
-- @since 0.1.0.0
withRepo
  :: forall env a. (HasLogFunc env, HasProcessContext env)
  => Repo
  -> RIO env a
  -> RIO env a
withRepo repo@(Repo url commit repoType' _subdir) action =
  withSystemTempDirectory "with-repo" $ \tmpDir -> do
    -- Note we do not immediately change directories into the new temporary directory,
    -- but instead wait until we have finished cloning the repo. This is because the
    -- repo URL may be a relative path on the local filesystem, and we should interpret
    -- it as relative to the current directory, not the temporary directory.
    let dir = tmpDir </> "cloned"
        (runCommand, resetArgs, submoduleArgs) =
          case repoType' of
            RepoGit ->
              ( runGitCommand
              , ["reset", "--hard", T.unpack commit]
              , Just ["submodule", "update", "--init", "--recursive"]
              )
            RepoHg ->
              ( runHgCommand
              , ["update", "-C", T.unpack commit]
              , Nothing
              )
        fixANSIForWindows =
          -- On Windows 10, an upstream issue with the `git clone` command means that
          -- command clears, but does not then restore, the
          -- ENABLE_VIRTUAL_TERMINAL_PROCESSING flag for native terminals. The
          -- folowing hack re-enables the lost ANSI-capability.
          when osIsWindows $ void $ liftIO $ hSupportsANSIWithoutEmulation stdout

    logInfo $ "Cloning " <> display commit <> " from " <> display url
    runCommand ["clone", T.unpack url, dir]
    fixANSIForWindows
    created <- doesDirectoryExist dir
    unless created $ throwIO $ FailedToCloneRepo repo

    withWorkingDir dir $ do
      runCommand resetArgs
      traverse_ runCommand submoduleArgs
      fixANSIForWindows
      action
