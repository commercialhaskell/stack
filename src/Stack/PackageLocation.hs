{-# LANGUAGE NoImplicitPrelude #-}
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
  , loadMultiRawCabalFilesIndex
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import Stack.Prelude
import           Crypto.Hash (hashWith, SHA256(..))
import qualified Data.ByteArray as Mem (convert)
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Client (parseUrlThrow)
import Network.HTTP.Download.Verified
import Path
import Path.Extra
import Path.IO
import Stack.Package
import Stack.Types.BuildPlan
import Stack.Types.Config
import Stack.Types.PackageIdentifier
import qualified System.Directory as Dir
import System.Process.Read
import System.Process.Run

-- | Same as 'resolveMultiPackageLocation', but works on a
-- 'SinglePackageLocation'.
resolveSinglePackageLocation
    :: HasConfig env
    => EnvOverride
    -> Path Abs Dir -- ^ project root
    -> PackageLocation FilePath
    -> RIO env (Path Abs Dir)
resolveSinglePackageLocation _ projRoot (PLFilePath fp) = resolveDir projRoot fp
resolveSinglePackageLocation _ projRoot (PLArchive (Archive url subdir msha)) = do
    workDir <- view workDirL

        -- TODO: dedupe with code for snapshot hash?
    let name = T.unpack $ decodeUtf8 $ S.take 12 $ B64URL.encode $ Mem.convert $ hashWith SHA256 $ encodeUtf8 url
        root = projRoot </> workDir </> $(mkRelDir "downloaded")
        fileExtension' = ".http-archive"

    fileRel <- parseRelFile $ name ++ fileExtension'
    dirRel <- parseRelDir name
    dirRelTmp <- parseRelDir $ name ++ ".tmp"
    let fileDownload = root </> fileRel
        dir = root </> dirRel

    exists <- doesDirExist dir
    unless exists $ do
        liftIO $ ignoringAbsence (removeDirRecur dir)

        let dirTmp = root </> dirRelTmp
        liftIO $ ignoringAbsence (removeDirRecur dirTmp)

        urlExists <- liftIO $ Dir.doesFileExist $ T.unpack url
        file <-
          if urlExists
            then do
              file <- liftIO $ Dir.canonicalizePath (T.unpack url) >>= parseAbsFile
              case msha of
                Nothing -> return ()
                Just sha -> do
                  actualSha <- mkStaticSHA256FromFile file
                  when (sha /= actualSha) $ error $ concat
                    [ "Invalid SHA256 found for local archive "
                    , show file
                    , "\nExpected: "
                    , T.unpack $ staticSHA256ToText sha
                    , "\nActual:   "
                    , T.unpack $ staticSHA256ToText actualSha
                    ]
              return file
            else do
              req <- parseUrlThrow $ T.unpack url
              let dreq = DownloadRequest
                    { drRequest = req
                    , drHashChecks =
                        case msha of
                          Nothing -> []
                          Just sha ->
                            [HashCheck
                              { hashCheckAlgorithm = SHA256
                              , hashCheckHexDigest = CheckHexDigestByteString $ staticSHA256ToBase16 sha
                              }]
                    , drLengthCheck = Nothing -- TODO add length info?
                    , drRetryPolicy = drRetryPolicyDefault
                    }
              _ <- verifiedDownload dreq fileDownload (const $ return ())
              return fileDownload

        let fp = toFilePath file

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
        ([dir'], []) -> resolveDir dir' subdir
        (dirs, files) -> liftIO $ do
            ignoringAbsence (removeFile fileDownload)
            ignoringAbsence (removeDirRecur dir)
            throwIO $ UnexpectedArchiveContents dirs files
resolveSinglePackageLocation menv projRoot (PLRepo (Repo url commit repoType' subdir)) =
    cloneRepo menv projRoot url commit repoType' >>= flip resolveDir subdir

-- | Resolve a PackageLocation into a path, downloading and cloning as
-- necessary.
--
-- Returns the updated PackageLocation value with just a single subdir
-- (if relevant).
resolveMultiPackageLocation
    :: HasConfig env
    => EnvOverride
    -> Path Abs Dir -- ^ project root
    -> PackageLocation Subdirs
    -> RIO env [(Path Abs Dir, PackageLocation FilePath)]
resolveMultiPackageLocation x y (PLFilePath fp) = do
  dir <- resolveSinglePackageLocation x y (PLFilePath fp)
  return [(dir, PLFilePath fp)]
resolveMultiPackageLocation x y (PLArchive (Archive url subdirs msha)) = do
  dir <- resolveSinglePackageLocation x y (PLArchive (Archive url "." msha))
  let subdirs' =
        case subdirs of
          DefaultSubdirs -> ["."]
          ExplicitSubdirs subs -> subs
  forM subdirs' $ \subdir -> do
    dir' <- resolveDir dir subdir
    return (dir', PLArchive (Archive url subdir msha))
resolveMultiPackageLocation menv projRoot (PLRepo (Repo url commit repoType' subdirs)) = do
  dir <- cloneRepo menv projRoot url commit repoType'

  let subdirs' =
        case subdirs of
          DefaultSubdirs -> ["."]
          ExplicitSubdirs subs -> subs
  forM subdirs' $ \subdir -> do
    dir' <- resolveDir dir subdir
    return (dir', PLRepo $ Repo url commit repoType' subdir)

cloneRepo
    :: HasConfig env
    => EnvOverride
    -> Path Abs Dir -- ^ project root
    -> Text -- ^ URL
    -> Text -- ^ commit
    -> RepoType
    -> RIO env (Path Abs Dir)
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
  :: forall env.
     HasConfig env
  => (PackageIdentifierRevision -> IO ByteString) -- ^ lookup in index
  -> EnvOverride
  -> Path Abs Dir -- ^ project root, used for checking out necessary files
  -> PackageLocationIndex FilePath
  -> RIO env ByteString
-- Need special handling of PLIndex for efficiency (just read from the
-- index tarball) and correctness (get the cabal file from the index,
-- not the package tarball itself, yay Hackage revisions).
loadSingleRawCabalFile loadFromIndex _ _ (PLIndex pir) = liftIO $ loadFromIndex pir
loadSingleRawCabalFile _ menv root (PLOther loc) =
  resolveSinglePackageLocation menv root loc >>=
  findOrGenerateCabalFile >>=
  liftIO . S.readFile . toFilePath

-- | Same as 'loadMultiRawCabalFiles' but for 'PackageLocationIndex'.
loadMultiRawCabalFilesIndex
  :: forall env.
     HasConfig env
  => (PackageIdentifierRevision -> IO ByteString) -- ^ lookup in index
  -> EnvOverride
  -> Path Abs Dir -- ^ project root, used for checking out necessary files
  -> PackageLocationIndex Subdirs
  -> RIO env [(ByteString, PackageLocationIndex FilePath)]
-- Need special handling of PLIndex for efficiency (just read from the
-- index tarball) and correctness (get the cabal file from the index,
-- not the package tarball itself, yay Hackage revisions).
loadMultiRawCabalFilesIndex loadFromIndex _ _ (PLIndex pir) = do
  bs <- liftIO $ loadFromIndex pir
  return [(bs, PLIndex pir)]
loadMultiRawCabalFilesIndex _ x y (PLOther z) =
  map (second PLOther) <$> loadMultiRawCabalFiles x y z

-- | Same as 'loadSingleRawCabalFile', but for 'PackageLocation' There
-- may be multiple results if dealing with a repository with subdirs,
-- in which case the returned 'PackageLocation' will have just the
-- relevant subdirectory selected.
loadMultiRawCabalFiles
  :: forall env.
     HasConfig env
  => EnvOverride
  -> Path Abs Dir -- ^ project root, used for checking out necessary files
  -> PackageLocation Subdirs
  -> RIO env [(ByteString, PackageLocation FilePath)]
loadMultiRawCabalFiles menv root loc =
    resolveMultiPackageLocation menv root loc >>= mapM go
  where
    go (dir, loc') = do
      cabalFile <- findOrGenerateCabalFile dir
      bs <- liftIO $ S.readFile $ toFilePath cabalFile
      return (bs, loc')
