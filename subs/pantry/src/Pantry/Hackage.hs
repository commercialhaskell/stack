{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pantry.Hackage
  ( updateHackageIndex
  , hackageIndexTarballL
  ) where

import RIO
import Data.Aeson
import Conduit
import Crypto.Hash.Conduit (sinkHash)
import Data.Conduit.Tar
import qualified RIO.Text as T
import Data.Text.Unsafe (unsafeTail)
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import Pantry.Types hiding (FileType (..))
import Pantry.Storage
import Pantry.StaticSHA256
import Network.URI (parseURI)
import Network.HTTP.Client.TLS (getGlobalManager)
import Data.Time (getCurrentTime)
import RIO.FilePath ((</>))
import qualified Distribution.Text
import Distribution.Types.PackageName (unPackageName)
import System.IO (SeekMode (..))

import qualified Hackage.Security.Client as HS
import qualified Hackage.Security.Client.Repository.Cache as HS
import qualified Hackage.Security.Client.Repository.Remote as HS
import qualified Hackage.Security.Client.Repository.HttpLib.HttpClient as HS
import qualified Hackage.Security.Util.Path as HS
import qualified Hackage.Security.Util.Pretty as HS

hackageDirL :: HasPantryConfig env => SimpleGetter env FilePath
hackageDirL = pantryConfigL.to ((</> "hackage") . pcRootDir)

hackageIndexTarballL :: HasPantryConfig env => SimpleGetter env FilePath
hackageIndexTarballL = hackageDirL.to (</> "00-index.tar")

-- | Download the most recent 01-index.tar file from Hackage and
-- update the database tables.
--
-- Returns @True@ if an update occurred, @False@ if we've already
-- updated once.
updateHackageIndex
  :: (HasPantryConfig env, HasLogFunc env)
  => Maybe Utf8Builder -- ^ reason for updating, if any
  -> RIO env Bool
updateHackageIndex mreason = gateUpdate $ do
    for_ mreason logInfo
    pc <- view pantryConfigL
    let HackageSecurityConfig keyIds threshold url = pcHackageSecurity pc
    root <- view hackageDirL
    tarball <- view hackageIndexTarballL
    baseURI <-
        case parseURI $ T.unpack url of
            Nothing -> throwString $ "Invalid Hackage Security base URL: " ++ T.unpack url
            Just x -> return x
    manager <- liftIO getGlobalManager
    run <- askRunInIO
    let logTUF = run . logInfo . fromString . HS.pretty
        withRepo = HS.withRepository
            (HS.makeHttpLib manager)
            [baseURI]
            HS.defaultRepoOpts
            HS.Cache
                { HS.cacheRoot = HS.fromAbsoluteFilePath root
                , HS.cacheLayout = HS.cabalCacheLayout
                }
            HS.hackageRepoLayout
            HS.hackageIndexLayout
            logTUF
    didUpdate <- liftIO $ withRepo $ \repo -> HS.uncheckClientErrors $ do
        needBootstrap <- HS.requiresBootstrap repo
        when needBootstrap $ do
            HS.bootstrap
                repo
                (map (HS.KeyId . T.unpack) keyIds)
                (HS.KeyThreshold $ fromIntegral threshold)
        now <- getCurrentTime
        HS.checkForUpdates repo (Just now)

    case didUpdate of
        HS.NoUpdates -> logInfo "No package index update available"
        HS.HasUpdates -> logInfo "Updated package index downloaded"

    withStorage $ do
      -- Alright, here's the story. In theory, we only ever append to
      -- a tarball. Therefore, we can store the last place we
      -- populated our cache from, and fast forward to that point. But
      -- there are two issues with that:
      --
      -- 1. Hackage may rebase, in which case we need to recalculate
      -- everything from the beginning. Unfortunately,
      -- hackage-security doesn't let us know when that happens.
      --
      -- 2. Some paranoia about files on the filesystem getting
      -- modified out from under us.
      --
      -- Therefore, we store both the last read-to index, _and_ the
      -- SHA256 of all of the contents until that point. When updating
      -- the cache, we calculate the new SHA256 of the whole file, and
      -- the SHA256 of the previous read-to point. If the old hashes
      -- match, we can do an efficient fast forward. Otherwise, we
      -- clear the old cache and repopulate.
      minfo <- loadLatestCacheUpdate
      (offset, newHash, newSize) <- lift $ withBinaryFile tarball ReadMode $ \h -> do
        logInfo "Calculating hashes to check for hackage-security rebases or filesystem changes"
        newSize <- fromIntegral <$> hFileSize h
        (offset, newHash) <-
          case minfo of
            Nothing -> do
              logInfo "No old cache found, populating cache from scratch"
              newHash <- runConduit $ sourceHandle h .| sinkHash
              pure (0, mkStaticSHA256FromDigest newHash)
            Just (oldSize, oldHash) -> do
              (oldHash', newHash) <- runConduit $ sourceHandle h .| getZipSink ((,)
                <$> ZipSink (mkStaticSHA256FromDigest <$> (takeCE (fromIntegral oldSize) .| sinkHash))
                <*> ZipSink sinkHash)
              offset <-
                if oldHash == oldHash'
                  then oldSize <$ logInfo "Updating preexisting cache, should be quick"
                  else 0 <$ do
                    logInfo "Package index change detected"
                    logInfo $ "Old size: " <> display oldSize
                    logInfo $ "Old hash: " <> display oldHash
                    logInfo $ "New hash: " <> display oldHash'
                    logInfo "Forcing a recache"
              pure (offset, mkStaticSHA256FromDigest newHash)
        pure (offset, newHash, newSize)

      lift $ logInfo $ "Populating cache from file size " <> display newSize <> ", hash " <> display newHash
      when (offset == 0) clearHackageRevisions
      populateCache tarball (fromIntegral offset) `onException`
        lift (logStickyDone "Failed populating package index cache")
      storeCacheUpdate newSize newHash
    logStickyDone "Package index cache populated"
  where
    gateUpdate inner = do
      pc <- view pantryConfigL
      join $ modifyMVar (pcUpdateRef pc) $ \toUpdate -> pure $
        if toUpdate
          then (False, True <$ inner)
          else (False, pure False)

-- | Populate the SQLite tables with Hackage index information.
populateCache
  :: (HasPantryConfig env, HasLogFunc env)
  => FilePath -- ^ tarball
  -> Integer -- ^ where to start processing from
  -> ReaderT SqlBackend (RIO env) ()
populateCache fp offset = withBinaryFile fp ReadMode $ \h -> do
  lift $ logInfo "Populating package index cache ..."
  counter <- newIORef (0 :: Int)
  hSeek h AbsoluteSeek offset
  runConduit $ sourceHandle h .| untar (perFile counter)
  where

    perFile counter fi
      | FTNormal <- fileType fi
      , Right path <- decodeUtf8' $ filePath fi
      , Just (name, version, filename) <- parseNameVersionSuffix path =
          if
            | filename == "package.json" ->
                sinkLazy >>= lift . addJSON name version
            | filename == T.pack (unPackageName name) <> ".cabal" -> do
                (BL.toStrict <$> sinkLazy) >>= lift . addCabal name version

                count <- readIORef counter
                let count' = count + 1
                writeIORef counter count'
                when (count' `mod` 400 == 0) $
                  lift $ lift $
                  logSticky $ "Processed " <> display count' <> " cabal files"
            | otherwise -> pure ()
      | otherwise = pure ()

    addJSON name version lbs =
      case eitherDecode' lbs of
        Left e -> lift $ logError $
          "Error processing Hackage security metadata for " <>
          fromString (Distribution.Text.display name) <> "-" <>
          fromString (Distribution.Text.display version) <> ": " <>
          fromString e
        Right (PackageDownload sha size) ->
          storeHackageTarballInfo name version sha size

    addCabal name version bs = do
      (blobTableId, _blobKey) <- storeBlob bs

      storeHackageRevision name version blobTableId

      -- Some older Stackage snapshots ended up with slightly
      -- modified cabal files, in particular having DOS-style
      -- line endings (CRLF) converted to Unix-style (LF). As a
      -- result, we track both hashes with and without CR
      -- characters stripped for compatibility with these older
      -- snapshots.
      --
      -- FIXME let's convert all old snapshots, correct the
      -- hashes, and drop this hack!
      let cr = 13
      when (cr `B.elem` bs) $ void $ storeBlob $ B.filter (/= cr) bs

    breakSlash x
        | T.null z = Nothing
        | otherwise = Just (y, unsafeTail z)
      where
        (y, z) = T.break (== '/') x

    parseNameVersionSuffix t1 = do
        (name, t2) <- breakSlash t1
        (version, filename) <- breakSlash t2

        name' <- Distribution.Text.simpleParse $ T.unpack name
        version' <- Distribution.Text.simpleParse $ T.unpack version

        Just (name', version', filename)

-- | Package download info from Hackage
data PackageDownload = PackageDownload !StaticSHA256 !Word
instance FromJSON PackageDownload where
    parseJSON = withObject "PackageDownload" $ \o1 -> do
        o2 <- o1 .: "signed"
        Object o3 <- o2 .: "targets"
        Object o4:_ <- return $ toList o3
        len <- o4 .: "length"
        hashes <- o4 .: "hashes"
        sha256' <- hashes .: "sha256"
        sha256 <-
          case mkStaticSHA256FromText sha256' of
            Left e -> fail $ "Invalid sha256: " ++ show e
            Right x -> return x
        return $ PackageDownload sha256 len
