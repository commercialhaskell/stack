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
import Pantry.Tree
import Pantry.Storage
import RIO
import Path.IO (resolveFile')
import RIO.FilePath ((</>))
import RIO.Directory (doesDirectoryExist)
import RIO.Process
import Database.Persist (Entity (..))
import qualified RIO.Text as T

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
getRepoKey repo pm = fst <$> getRepo repo pm -- potential optimization

getRepo
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Repo
  -> PackageMetadata
  -> RIO env (TreeKey, Tree)
getRepo repo pm =
  checkPackageMetadata (PLIRepo repo pm) pm $
  withCache $
  getRepo' repo pm
  where
    withCache
      :: RIO env (TreeKey, Tree)
      -> RIO env (TreeKey, Tree)
    withCache inner = do
      mtid <- withStorage (loadRepoCache repo (pmSubdir pm))
      case mtid of
        Just tid -> withStorage $ loadTreeById tid
        Nothing -> do
          (treeKey, tree) <- inner
          withStorage $ do
            ment <- getTreeSForKey treeKey
            case ment of
              Nothing -> error $ "invariant violated, TreeS not found: " ++ show treeKey
              Just (Entity tid _) -> storeRepoCache repo (pmSubdir pm) tid
          pure (treeKey, tree)

getRepo'
  :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => Repo
  -> PackageMetadata
  -> RIO env (TreeKey, Tree)
getRepo' repo@(Repo url commit repoType') pm =
  withSystemTempDirectory "get-repo" $
  \tmpdir -> withWorkingDir tmpdir $ do
    let suffix = "cloned"
        dir = tmpdir </> suffix
        tarball = tmpdir </> "foo.tar"

    let (commandName, cloneArgs, resetArgs, archiveArgs) =
          case repoType' of
            RepoGit ->
              ( "git"
              , ["--recursive"]
              , ["reset", "--hard", T.unpack commit]
              , ["archive", "-o", tarball, "HEAD"]
              )
            RepoHg ->
              ( "hg"
              , []
              , ["update", "-C", T.unpack commit]
              , ["archive", tarball, "-X", ".hg_archival.txt"]
              )

    logInfo $ "Cloning " <> display commit <> " from " <> display url
    void $ proc
      commandName
      ("clone" : cloneArgs ++ [T.unpack url, suffix])
      readProcess_
    created <- doesDirectoryExist dir
    unless created $ throwIO $ FailedToCloneRepo repo

    withWorkingDir dir $ do
      void $ proc commandName resetArgs readProcess_
      void $ proc commandName archiveArgs readProcess_
    abs' <- resolveFile' tarball
    getArchive
      Archive
        { archiveLocation = ALFilePath $ ResolvedPath
            { resolvedRelative = RelFilePath $ T.pack tarball
            , resolvedAbsolute = abs'
            }
        , archiveHash = Nothing
        , archiveSize = Nothing
        }
      PackageMetadata
        { pmName = Nothing
        , pmVersion = Nothing
        , pmTree = Nothing
        , pmCabal = Nothing
        , pmSubdir = pmSubdir pm
        }
