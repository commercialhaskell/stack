{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pantry.Repo -- FIXME needs to be implemented!
  ( fetchRepos
  , getRepo
  , getRepoKey
  ) where

import Pantry.Types
import Pantry.Storage
import Pantry.Tree
import RIO

fetchRepos
  :: (HasPantryConfig env, HasLogFunc env)
  => [(Repo, PackageMetadata)]
  -> RIO env ()
fetchRepos pairs = do
  -- FIXME be more efficient, group together shared archives
  for_ pairs $ uncurry getRepo

getRepoKey
  :: forall env. (HasPantryConfig env, HasLogFunc env)
  => Repo
  -> PackageMetadata
  -> RIO env TreeKey
getRepoKey repo pm = fst <$> getRepo repo pm -- potential optimization

getRepo
  :: forall env. (HasPantryConfig env, HasLogFunc env)
  => Repo
  -> PackageMetadata
  -> RIO env (TreeKey, Tree)
getRepo repo pm =
  checkPackageMetadata (PLRepo repo pm) pm $
  undefined

    {-
cloneRepo
    :: HasConfig env
    => Path Abs Dir -- ^ project root
    -> Text -- ^ URL
    -> Text -- ^ commit
    -> RepoType
    -> RIO env (Path Abs Dir)
cloneRepo projRoot url commit repoType' = do
    workDir <- view workDirL
    let nameBeforeHashing = case repoType' of
            RepoGit -> T.unwords [url, commit]
            RepoHg -> T.unwords [url, commit, "hg"]
        -- TODO: dedupe with code for snapshot hash?
        name = T.unpack $ decodeUtf8 $ S.take 12 $ B64URL.encode $ Mem.convert $ hashWith SHA256 $ encodeUtf8 nameBeforeHashing
        root = projRoot </> workDir </> $(mkRelDir "downloaded")

    dirRel <- parseRelDir name
    let dir = root </> dirRel

    exists <- doesDirExist dir
    unless exists $ do
        liftIO $ ignoringAbsence (removeDirRecur dir)

        let cloneAndExtract commandName cloneArgs resetCommand =
              withWorkingDir (toFilePath root) $ do
                ensureDir root
                logInfo $ "Cloning " <> display commit <> " from " <> display url
                proc commandName
                       ("clone" :
                        cloneArgs ++
                        [ T.unpack url
                        , toFilePathNoTrailingSep dir
                        ]) runProcess_
                created <- doesDirExist dir
                unless created $ throwM $ FailedToCloneRepo commandName
                withWorkingDir (toFilePath dir) $ readProcessNull commandName
                    (resetCommand ++ [T.unpack commit, "--"])
                    `catchAny` \case
                        ex -> do
                            logInfo $
                              "Please ensure that commit " <>
                              display commit <>
                              " exists within " <>
                              display url
                            throwM ex

        case repoType' of
            RepoGit -> cloneAndExtract "git" ["--recursive"] ["--git-dir=.git", "reset", "--hard"]
            RepoHg  -> cloneAndExtract "hg"  []              ["--repository", ".", "update", "-C"]

    return dir

    -}
