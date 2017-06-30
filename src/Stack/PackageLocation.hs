{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Deal with downloading, cloning, or whatever else is necessary for
-- getting a 'PackageLocation' into something Stack can work with.
module Stack.PackageLocation
  ( resolvePackageLocation
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import           Crypto.Hash (hashWith, SHA256(..))
import qualified Data.ByteArray as Mem (convert)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as L
import           Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Client (parseUrlThrow)
import Network.HTTP.Download (download)
import Path
import Path.Extra
import Path.IO
import Stack.Types.BuildPlan
import Stack.Types.Config
import System.IO (withBinaryFile, IOMode (ReadMode))
import System.Process.Read
import System.Process.Run

-- | Resolve a PackageLocation into a path, downloading and cloning as
-- necessary.
--
-- Returns the updated PackageLocation value with just a single subdir
-- (if relevant).
--
-- FIXME should probably have the option to just return an archive
-- location.
resolvePackageLocation
    :: (StackMiniM env m, HasConfig env)
    => EnvOverride
    -> Path Abs Dir -- ^ project root
    -> PackageLocation
    -> m [(Path Abs Dir, PackageLocation)]
resolvePackageLocation _ projRoot loc@(PLFilePath fp) = do
  path <- resolveDir projRoot fp
  return [(path, loc)]
resolvePackageLocation _ projRoot loc@(PLHttp url) = do
    workDir <- view workDirL
    let nameBeforeHashing = url
        -- TODO: dedupe with code for snapshot hash?
        name = T.unpack $ decodeUtf8 $ S.take 12 $ B64URL.encode $ Mem.convert $ hashWith SHA256 $ encodeUtf8 nameBeforeHashing
        root = projRoot </> workDir </> $(mkRelDir "downloaded")
        fileExtension' = ".http-archive"

    fileRel <- parseRelFile $ name ++ fileExtension'
    dirRel <- parseRelDir name
    dirRelTmp <- parseRelDir $ name ++ ".tmp"
    let file = root </> fileRel
        dir = root </> dirRel

    exists <- doesDirExist dir
    unless exists $ do
        ignoringAbsence (removeDirRecur dir)

        let dirTmp = root </> dirRelTmp
        ignoringAbsence (removeDirRecur dirTmp)

        let fp = toFilePath file
        req <- parseUrlThrow $ T.unpack url
        _ <- download req file

        let tryTar = do
                $logDebug $ "Trying to untar " <> T.pack fp
                liftIO $ withBinaryFile fp ReadMode $ \h -> do
                    lbs <- L.hGetContents h
                    let entries = Tar.read $ GZip.decompress lbs
                    Tar.unpack (toFilePath dirTmp) entries
            tryZip = do
                $logDebug $ "Trying to unzip " <> T.pack fp
                archive <- fmap Zip.toArchive $ liftIO $ L.readFile fp
                liftIO $  Zip.extractFilesFromArchive [Zip.OptDestination
                                                       (toFilePath dirTmp)] archive
            err = throwM $ UnableToExtractArchive url file

            catchAnyLog goodpath handler =
                catchAny goodpath $ \e -> do
                    $logDebug $ "Got exception: " <> T.pack (show e)
                    handler

        tryTar `catchAnyLog` tryZip `catchAnyLog` err
        renameDir dirTmp dir

    x <- listDir dir
    case x of
        ([dir'], []) -> return [(dir', loc)]
        (dirs, files) -> do
            ignoringAbsence (removeFile file)
            ignoringAbsence (removeDirRecur dir)
            throwM $ UnexpectedArchiveContents dirs files
resolvePackageLocation menv projRoot (PLRepo (Repo url commit repoType' subdirs)) = do
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
        ignoringAbsence (removeDirRecur dir)

        let cloneAndExtract commandName cloneArgs resetCommand = do
                ensureDir root
                callProcessInheritStderrStdout Cmd
                    { cmdDirectoryToRunIn = Just root
                    , cmdCommandToRun = commandName
                    , cmdEnvOverride = menv
                    , cmdCommandLineArguments =
                        "clone" :
                        cloneArgs ++
                        [ T.unpack url
                        , toFilePathNoTrailingSep dir
                        ]
                    }
                created <- doesDirExist dir
                unless created $ throwM $ FailedToCloneRepo commandName
                readProcessNull (Just dir) menv commandName
                    (resetCommand ++ [T.unpack commit, "--"])
                    `catch` \case
                        ex@ProcessFailed{} -> do
                            $logInfo $ "Please ensure that commit " <> commit <> " exists within " <> url
                            throwM ex
                        ex -> throwM ex

        case repoType' of
            RepoGit -> cloneAndExtract "git" ["--recursive"] ["--git-dir=.git", "reset", "--hard"]
            RepoHg  -> cloneAndExtract "hg"  []              ["--repository", ".", "update", "-C"]

    forM subdirs $ \subdir -> do
      dir' <- resolveDir dir subdir
      return (dir', PLRepo $ Repo url commit repoType' [subdir])
