{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Deal with downloading, cloning, or whatever else is necessary for
-- getting a 'PackageLocation' into something Stack can work with.
module Stack.PackageLocation
  ( resolveSinglePackageLocation
  , resolveMultiPackageLocation
  , loadSingleRawCabalFile
  , loadMultiRawCabalFiles
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import           Crypto.Hash (hashWith, SHA256(..))
import qualified Data.ByteArray as Mem (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as L
import           Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Client (parseUrlThrow)
import Network.HTTP.Download (download)
import Path
import Path.Extra
import Path.IO
import Stack.Package
import Stack.Types.BuildPlan
import Stack.Types.Config
import Stack.Types.PackageIdentifier
import System.IO (withBinaryFile, IOMode (ReadMode))
import System.Process.Read
import System.Process.Run

-- | Same as 'resolveMultiPackageLocation', but works on a
-- 'SinglePackageLocation'.
resolveSinglePackageLocation
    :: (StackMiniM env m, HasConfig env)
    => EnvOverride
    -> Path Abs Dir -- ^ project root
    -> SinglePackageLocation
    -> m (Path Abs Dir)
resolveSinglePackageLocation _ projRoot (PLFilePath fp) = do
  path <- resolveDir projRoot fp
  return path
resolveSinglePackageLocation menv projRoot (PLIndex pir) = do
    $logError "resolvePackageLocation on PLIndex called, this isn't a good idea" -- FIXME maybe we'll be OK with this after all?
    error "FIXME"
    {-
    workDir <- view workDirL
    let nameBeforeHashing = T.pack $ show pir
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
    -}
resolveSinglePackageLocation _ projRoot (PLHttp url) = do
    workDir <- view workDirL

        -- TODO: dedupe with code for snapshot hash?
    let name = T.unpack $ decodeUtf8 $ S.take 12 $ B64URL.encode $ Mem.convert $ hashWith SHA256 $ encodeUtf8 url
        root = projRoot </> workDir </> $(mkRelDir "downloaded")
        fileExtension' = ".http-archive"

    fileRel <- parseRelFile $ name ++ fileExtension'
    dirRel <- parseRelDir name
    dirRelTmp <- parseRelDir $ name ++ ".tmp"
    let file = root </> fileRel
        dir = root </> dirRel

    exists <- doesDirExist dir
    unless exists $ do
        liftIO $ ignoringAbsence (removeDirRecur dir)

        let dirTmp = root </> dirRelTmp
        liftIO $ ignoringAbsence (removeDirRecur dirTmp)

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
        ([dir'], []) -> return dir'
        (dirs, files) -> liftIO $ do
            ignoringAbsence (removeFile file)
            ignoringAbsence (removeDirRecur dir)
            throwIO $ UnexpectedArchiveContents dirs files
resolveSinglePackageLocation menv projRoot (PLRepo (Repo url commit repoType' subdir)) =
    cloneRepo menv projRoot url commit repoType' >>= flip resolveDir subdir

-- | Resolve a PackageLocation into a path, downloading and cloning as
-- necessary.
--
-- Returns the updated PackageLocation value with just a single subdir
-- (if relevant).
--
-- FIXME should probably have the option to just return an archive
-- location.
resolveMultiPackageLocation
    :: (StackMiniM env m, HasConfig env)
    => EnvOverride
    -> Path Abs Dir -- ^ project root
    -> PackageLocation
    -> m [(Path Abs Dir, SinglePackageLocation)]
resolveMultiPackageLocation x y (PLFilePath fp) = do
  dir <- resolveSinglePackageLocation x y (PLFilePath fp)
  return [(dir, PLFilePath fp)]
resolveMultiPackageLocation x y (PLIndex pir) = do
  dir <- resolveSinglePackageLocation x y (PLIndex pir)
  return [(dir, PLIndex pir)]
resolveMultiPackageLocation x y (PLHttp url) = do
  dir <- resolveSinglePackageLocation x y (PLHttp url)
  return [(dir, PLHttp url)]
resolveMultiPackageLocation menv projRoot (PLRepo (Repo url commit repoType' subdirs)) = do
    dir <- cloneRepo menv projRoot url commit repoType'

    forM subdirs $ \subdir -> do
      dir' <- resolveDir dir subdir
      return (dir', PLRepo $ Repo url commit repoType' subdir)

cloneRepo
    :: (StackMiniM env m, HasConfig env)
    => EnvOverride
    -> Path Abs Dir -- ^ project root
    -> Text -- ^ URL
    -> Text -- ^ commit
    -> RepoType
    -> m (Path Abs Dir)
cloneRepo menv projRoot url commit repoType' = do
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

    return dir

-- | Load the raw bytes in the cabal files present in the given
-- 'SinglePackageLocation'.
loadSingleRawCabalFile
  :: forall m env.
     (StackMiniM env m, HasConfig env)
  => (PackageIdentifierRevision -> IO ByteString) -- ^ lookup in index
  -> EnvOverride
  -> Path Abs Dir -- ^ project root, used for checking out necessary files
  -> SinglePackageLocation
  -> m ByteString
-- Need special handling of PLIndex for efficiency (just read from the
-- index tarball) and correctness (get the cabal file from the index,
-- not the package tarball itself, yay Hackage revisions).
loadSingleRawCabalFile loadFromIndex _ _ (PLIndex pir) = liftIO $ loadFromIndex pir
loadSingleRawCabalFile _ menv root loc =
  resolveSinglePackageLocation menv root loc >>=
  findOrGenerateCabalFile >>=
  liftIO . S.readFile . toFilePath

-- | Same as 'loadSingleRawCabalFile', but for 'PackageLocation' There
-- may be multiple results if dealing with a repository with subdirs,
-- in which case the returned 'PackageLocation' will have just the
-- relevant subdirectory selected.
loadMultiRawCabalFiles
  :: forall m env.
     (StackMiniM env m, HasConfig env)
  => (PackageIdentifierRevision -> IO ByteString) -- ^ lookup in index
  -> EnvOverride
  -> Path Abs Dir -- ^ project root, used for checking out necessary files
  -> PackageLocation
  -> m [(ByteString, SinglePackageLocation)]
-- Need special handling of PLIndex for efficiency (just read from the
-- index tarball) and correctness (get the cabal file from the index,
-- not the package tarball itself, yay Hackage revisions).
loadMultiRawCabalFiles x y z (PLIndex pir) = do
  bs <- loadSingleRawCabalFile x y z (PLIndex pir)
  return [(bs, PLIndex pir)]
loadMultiRawCabalFiles _ menv root loc = do
  resolveMultiPackageLocation menv root loc >>= mapM go
  where
    go (dir, loc') = do
      cabalFile <- findOrGenerateCabalFile dir
      bs <- liftIO $ S.readFile $ toFilePath cabalFile
      return (bs, loc')
