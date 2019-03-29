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

-- | Create a tarball containing files from a repository
createRepoArchive
  :: forall env. (HasLogFunc env, HasProcessContext env)
  => Repo
  -> FilePath -- ^ Output tar archive filename
  -> RIO env ()
createRepoArchive repo tarball = do
  let runCommand cmd args = void $ proc cmd args readProcess_

  withRepo repo $ case repoType repo of
    RepoGit -> do
       runCommand "git" ["-c", "core.autocrlf=false", "archive", "-o", tarball, "HEAD"]
       -- also include submodules files: use `git submodule foreach` to
       -- execute `git archive` in each submodule and to append the
       -- generated archive to the main one with `tar -A`
       runCommand "git"
         [ "submodule", "foreach", "--recursive"
         , "git -c core.autocrlf=false archive --prefix=$displaypath/ -o bar.tar HEAD"
           <> " && if [ -f bar.tar ]; then tar --force-local -Af " <> tarball <> " bar.tar ; fi"
         ]
    RepoHg  -> runCommand "hg"  ["archive", tarball, "-X", ".hg_archival.txt"]


-- | Clone the repository and execute the action with the working
-- directory set to the repository root.
withRepo
  :: forall env a. (HasLogFunc env, HasProcessContext env)
  => Repo
  -> RIO env a
  -> RIO env a
withRepo repo@(Repo url commit repoType' _subdir) action =
  withSystemTempDirectory "with-repo" $
  \tmpdir -> withWorkingDir tmpdir $ do
    let suffix = "cloned"
        dir = tmpdir </> suffix

    let (commandName, resetArgs, submoduleArgs) =
          case repoType' of
            RepoGit ->
              ( "git"
              , ["reset", "--hard", T.unpack commit]
              , Just ["submodule", "update", "--init", "--recursive"]
              )
            RepoHg ->
              ( "hg"
              , ["update", "-C", T.unpack commit]
              , Nothing
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
      action
