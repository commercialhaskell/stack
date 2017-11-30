{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Deal with downloading, cloning, or whatever else is necessary for
-- getting a 'PackageLocation' into something Stack can work with.
module Stack.PackageLocation
  ( resolveSinglePackageLocation
  , resolveMultiPackageLocation
  , parseSingleCabalFile
  , parseSingleCabalFileIndex
  , parseMultiCabalFiles
  , parseMultiCabalFilesIndex
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
import Distribution.PackageDescription (GenericPackageDescription)
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
    => Path Abs Dir -- ^ project root
    -> PackageLocation FilePath
    -> RIO env (Path Abs Dir)
resolveSinglePackageLocation projRoot (PLFilePath fp) = resolveDir projRoot fp
resolveSinglePackageLocation projRoot (PLArchive (Archive url subdir msha)) = do
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
                logDebug $ "Trying to untar " <> T.pack fp
                liftIO $ withBinaryFile fp ReadMode $ \h -> do
                    lbs <- L.hGetContents h
                    let entries = Tar.read $ GZip.decompress lbs
                    Tar.unpack (toFilePath dirTmp) entries
            tryZip = do
                logDebug $ "Trying to unzip " <> T.pack fp
                archive <- fmap Zip.toArchive $ liftIO $ L.readFile fp
                liftIO $  Zip.extractFilesFromArchive [Zip.OptDestination
                                                       (toFilePath dirTmp)] archive
            err = throwM $ UnableToExtractArchive url file

            catchAnyLog goodpath handler =
                catchAny goodpath $ \e -> do
                    logDebug $ "Got exception: " <> T.pack (show e)
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
resolveSinglePackageLocation projRoot (PLRepo (Repo url commit repoType' subdir)) =
    cloneRepo projRoot url commit repoType' >>= flip resolveDir subdir

-- | Resolve a PackageLocation into a path, downloading and cloning as
-- necessary.
--
-- Returns the updated PackageLocation value with just a single subdir
-- (if relevant).
resolveMultiPackageLocation
    :: HasConfig env
    => Path Abs Dir -- ^ project root
    -> PackageLocation Subdirs
    -> RIO env [(Path Abs Dir, PackageLocation FilePath)]
resolveMultiPackageLocation y (PLFilePath fp) = do
  dir <- resolveSinglePackageLocation y (PLFilePath fp)
  return [(dir, PLFilePath fp)]
resolveMultiPackageLocation y (PLArchive (Archive url subdirs msha)) = do
  dir <- resolveSinglePackageLocation y (PLArchive (Archive url "." msha))
  let subdirs' =
        case subdirs of
          DefaultSubdirs -> ["."]
          ExplicitSubdirs subs -> subs
  forM subdirs' $ \subdir -> do
    dir' <- resolveDir dir subdir
    return (dir', PLArchive (Archive url subdir msha))
resolveMultiPackageLocation projRoot (PLRepo (Repo url commit repoType' subdirs)) = do
  dir <- cloneRepo projRoot url commit repoType'

  let subdirs' =
        case subdirs of
          DefaultSubdirs -> ["."]
          ExplicitSubdirs subs -> subs
  forM subdirs' $ \subdir -> do
    dir' <- resolveDir dir subdir
    return (dir', PLRepo $ Repo url commit repoType' subdir)

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
        menv <- getMinimalEnvOverride

        let cloneAndExtract commandName cloneArgs resetCommand = do
                ensureDir root
                logInfo $ "Cloning " <> commit <> " from " <> url
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
                            logInfo $ "Please ensure that commit " <> commit <> " exists within " <> url
                            throwM ex
                        ex -> throwM ex

        case repoType' of
            RepoGit -> cloneAndExtract "git" ["--recursive"] ["--git-dir=.git", "reset", "--hard"]
            RepoHg  -> cloneAndExtract "hg"  []              ["--repository", ".", "update", "-C"]

    return dir

-- | Parse the cabal files present in the given
-- 'PackageLocationIndex FilePath'.
parseSingleCabalFileIndex
  :: forall env.
     HasConfig env
  => (PackageIdentifierRevision -> IO ByteString) -- ^ lookup in index
  -> Path Abs Dir -- ^ project root, used for checking out necessary files
  -> PackageLocationIndex FilePath
  -> RIO env GenericPackageDescription
-- Need special handling of PLIndex for efficiency (just read from the
-- index tarball) and correctness (get the cabal file from the index,
-- not the package tarball itself, yay Hackage revisions).
parseSingleCabalFileIndex loadFromIndex _ (PLIndex pir) = readPackageUnresolvedIndex loadFromIndex pir
parseSingleCabalFileIndex _ root (PLOther loc) = lpvGPD <$> parseSingleCabalFile root False loc

parseSingleCabalFile
  :: forall env. HasConfig env
  => Path Abs Dir -- ^ project root, used for checking out necessary files
  -> Bool -- ^ print warnings?
  -> PackageLocation FilePath
  -> RIO env LocalPackageView
parseSingleCabalFile root printWarnings loc = do
  dir <- resolveSinglePackageLocation root loc
  (gpd, cabalfp) <- readPackageUnresolvedDir dir printWarnings
  return LocalPackageView
    { lpvCabalFP = cabalfp
    , lpvGPD = gpd
    , lpvLoc = loc
    }

-- | Load and parse cabal files into 'GenericPackageDescription's
parseMultiCabalFiles
  :: forall env. HasConfig env
  => Path Abs Dir -- ^ project root, used for checking out necessary files
  -> Bool -- ^ print warnings?
  -> PackageLocation Subdirs
  -> RIO env [LocalPackageView]
parseMultiCabalFiles root printWarnings loc0 =
  resolveMultiPackageLocation root loc0 >>=
  mapM (\(dir, loc1) -> do
    (gpd, cabalfp) <- readPackageUnresolvedDir dir printWarnings
    return LocalPackageView
      { lpvCabalFP = cabalfp
      , lpvGPD = gpd
      , lpvLoc = loc1
      })

-- | 'parseMultiCabalFiles' but supports 'PLIndex'
parseMultiCabalFilesIndex
  :: forall env. HasConfig env
  => (PackageIdentifierRevision -> IO ByteString)
  -> Path Abs Dir -- ^ project root, used for checking out necessary files
  -> PackageLocationIndex Subdirs
  -> RIO env [(GenericPackageDescription, PackageLocationIndex FilePath)]
parseMultiCabalFilesIndex loadFromIndex _root (PLIndex pir) =
  (pure . (, PLIndex pir)) <$>
  readPackageUnresolvedIndex loadFromIndex pir
parseMultiCabalFilesIndex _ root (PLOther loc0) =
  map (\lpv -> (lpvGPD lpv, PLOther $ lpvLoc lpv)) <$>
  parseMultiCabalFiles root False loc0
