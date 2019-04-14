{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pantry.Hackage
  ( updateHackageIndex
  , DidUpdateOccur (..)
  , RequireHackageIndex (..)
  , hackageIndexTarballL
  , getHackageTarball
  , getHackageTarballKey
  , getHackageCabalFile
  , getHackagePackageVersions
  , getHackagePackageVersionRevisions
  , getHackageTypoCorrections
  , UsePreferredVersions (..)
  ) where

import RIO
import RIO.Process
import Data.Aeson
import Conduit
import Data.Conduit.Tar
import qualified RIO.Text as T
import qualified RIO.Map as Map
import Data.Text.Unsafe (unsafeTail)
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import Pantry.Archive
import Pantry.Types hiding (FileType (..))
import Pantry.Storage
import Pantry.Tree
import qualified Pantry.SHA256 as SHA256
import Network.URI (parseURI)
import Data.Time (getCurrentTime)
import Path ((</>), Path, Abs, Rel, Dir, File, toFilePath, parseRelDir, parseRelFile)
import qualified Distribution.Text
import qualified Distribution.PackageDescription as Cabal
import System.IO (SeekMode (..))
import qualified Data.List.NonEmpty as NE
import Data.Text.Metrics (damerauLevenshtein)
import Distribution.Types.Version (versionNumbers)
import Distribution.Types.VersionRange (withinRange)

import qualified Hackage.Security.Client as HS
import qualified Hackage.Security.Client.Repository.Cache as HS
import qualified Hackage.Security.Client.Repository.Remote as HS
import qualified Hackage.Security.Client.Repository.HttpLib.HttpClient as HS
import qualified Hackage.Security.Util.Path as HS
import qualified Hackage.Security.Util.Pretty as HS

hackageRelDir :: Path Rel Dir
hackageRelDir = either impureThrow id $ parseRelDir "hackage"

hackageDirL :: HasPantryConfig env => SimpleGetter env (Path Abs Dir)
hackageDirL = pantryConfigL.to ((</> hackageRelDir) . pcRootDir)

indexRelFile :: Path Rel File
indexRelFile = either impureThrow id $ parseRelFile "00-index.tar"

-- | Where does pantry download its 01-index.tar file from Hackage?
--
-- @since 0.1.0.0
hackageIndexTarballL :: HasPantryConfig env => SimpleGetter env (Path Abs File)
hackageIndexTarballL = hackageDirL.to (</> indexRelFile)

-- | Did an update occur when running 'updateHackageIndex'?
--
-- @since 0.1.0.0
data DidUpdateOccur = UpdateOccurred | NoUpdateOccurred

-- | Download the most recent 01-index.tar file from Hackage and
-- update the database tables.
--
-- This function will only perform an update once per 'PantryConfig'
-- for user sanity. See the return value to find out if it happened.
--
-- @since 0.1.0.0
updateHackageIndex
  :: (HasPantryConfig env, HasLogFunc env)
  => Maybe Utf8Builder -- ^ reason for updating, if any
  -> RIO env DidUpdateOccur
updateHackageIndex mreason = do
  storage <- view $ pantryConfigL.to pcStorage
  gateUpdate $ withWriteLock_ storage $ do
    for_ mreason logInfo
    pc <- view pantryConfigL
    let HackageSecurityConfig keyIds threshold url ignoreExpiry = pcHackageSecurity pc
    root <- view hackageDirL
    tarball <- view hackageIndexTarballL
    baseURI <-
        case parseURI $ T.unpack url of
            Nothing -> throwString $ "Invalid Hackage Security base URL: " ++ T.unpack url
            Just x -> return x
    run <- askRunInIO
    let logTUF = run . logInfo . fromString . HS.pretty
        withRepo = HS.withRepository
            HS.httpLib
            [baseURI]
            HS.defaultRepoOpts
            HS.Cache
                { HS.cacheRoot = HS.fromAbsoluteFilePath $ toFilePath root
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
        maybeNow <- if ignoreExpiry
                      then pure Nothing
                      else Just <$> getCurrentTime
        HS.checkForUpdates repo maybeNow

    case didUpdate of
      HS.NoUpdates -> logInfo "No package index update available"
      HS.HasUpdates -> do
        logInfo "Updated package index downloaded"
        updateCache tarball
    logStickyDone "Package index cache populated"
  where
    -- This is the one action in the Pantry codebase known to hold a
    -- write lock on the database for an extended period of time. To
    -- avoid failures due to SQLite locks failing, we take our own
    -- lock outside of SQLite for this action.
    --
    -- See https://github.com/commercialhaskell/stack/issues/4471
    updateCache tarball = withStorage $ do
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
      (offset, newHash, newSize) <- lift $ withBinaryFile (toFilePath tarball) ReadMode $ \h -> do
        logInfo "Calculating hashes to check for hackage-security rebases or filesystem changes"

        -- The size of the new index tarball, ignoring the required
        -- (by the tar spec) 1024 null bytes at the end, which will be
        -- mutated in the future by other updates.
        newSize :: Word <- (fromIntegral . max 0 . subtract 1024) <$> hFileSize h
        let sinkSHA256 len = takeCE (fromIntegral len) .| SHA256.sinkHash

        case minfo of
          Nothing -> do
            logInfo "No old cache found, populating cache from scratch"
            newHash <- runConduit $ sourceHandle h .| sinkSHA256 newSize
            pure (0, newHash, newSize)
          Just (FileSize oldSize, oldHash) -> do
            -- oldSize and oldHash come from the database, and tell
            -- us what we cached already. Compare against
            -- oldHashCheck, which assuming the tarball has not been
            -- rebased will be the same as oldHash. At the same
            -- time, calculate newHash, which is the hash of the new
            -- content as well.
            (oldHashCheck, newHash) <- runConduit $ sourceHandle h .| getZipSink ((,)
              <$> ZipSink (sinkSHA256 oldSize)
              <*> ZipSink (sinkSHA256 newSize)
                                                                             )
            offset <-
              if oldHash == oldHashCheck
                then oldSize <$ logInfo "Updating preexisting cache, should be quick"
                else 0 <$ do
                  logInfo "Package index change detected, that's pretty unusual"
                  logInfo $ "Old size: " <> display oldSize
                  logInfo $ "Old hash (orig) : " <> display oldHash
                  logInfo $ "New hash (check): " <> display oldHashCheck
                  logInfo "Forcing a recache"
            pure (offset, newHash, newSize)

      lift $ logInfo $ "Populating cache from file size " <> display newSize <> ", hash " <> display newHash
      when (offset == 0) clearHackageRevisions
      populateCache tarball (fromIntegral offset) `onException`
        lift (logStickyDone "Failed populating package index cache")
      storeCacheUpdate (FileSize newSize) newHash
    gateUpdate inner = do
      pc <- view pantryConfigL
      join $ modifyMVar (pcUpdateRef pc) $ \toUpdate -> pure $
        if toUpdate
          then (False, UpdateOccurred <$ inner)
          else (False, pure NoUpdateOccurred)

-- | Populate the SQLite tables with Hackage index information.
populateCache
  :: (HasPantryConfig env, HasLogFunc env)
  => Path Abs File -- ^ tarball
  -> Integer -- ^ where to start processing from
  -> ReaderT SqlBackend (RIO env) ()
populateCache fp offset = withBinaryFile (toFilePath fp) ReadMode $ \h -> do
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
            | filename == unSafeFilePath (cabalFileName name) -> do
                (BL.toStrict <$> sinkLazy) >>= lift . addCabal name version

                count <- readIORef counter
                let count' = count + 1
                writeIORef counter count'
                when (count' `mod` 400 == 0) $
                  lift $ lift $
                  logSticky $ "Processed " <> display count' <> " cabal files"
            | otherwise -> pure ()
      | FTNormal <- fileType fi
      , Right path <- decodeUtf8' $ filePath fi
      , (nameT, "/preferred-versions") <- T.break (== '/') path
      , Just name <- parsePackageName $ T.unpack nameT = do
          lbs <- sinkLazy
          case decodeUtf8' $ BL.toStrict lbs of
            Left _ -> pure () -- maybe warning
            Right p -> lift $ storePreferredVersion name p
      | otherwise = pure ()

    addJSON name version lbs =
      case eitherDecode' lbs of
        Left e -> lift $ logError $
          "Error processing Hackage security metadata for " <>
          fromString (Distribution.Text.display name) <> "-" <>
          fromString (Distribution.Text.display version) <> ": " <>
          fromString e
        Right (PackageDownload sha size) ->
          storeHackageTarballInfo name version sha $ FileSize size

    addCabal name version bs = do
      (blobTableId, _blobKey) <- storeBlob bs

      storeHackageRevision name version blobTableId

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
data PackageDownload = PackageDownload !SHA256 !Word
instance FromJSON PackageDownload where
    parseJSON = withObject "PackageDownload" $ \o1 -> do
        o2 <- o1 .: "signed"
        Object o3 <- o2 .: "targets"
        Object o4:_ <- return $ toList o3
        len <- o4 .: "length"
        hashes <- o4 .: "hashes"
        sha256' <- hashes .: "sha256"
        sha256 <-
          case SHA256.fromHexText sha256' of
            Left e -> fail $ "Invalid sha256: " ++ show e
            Right x -> return x
        return $ PackageDownload sha256 len

getHackageCabalFile
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageIdentifierRevision
  -> RIO env ByteString
getHackageCabalFile pir@(PackageIdentifierRevision _ _ cfi) = do
  bid <- resolveCabalFileInfo pir
  bs <- withStorage $ loadBlobById bid
  case cfi of
    CFIHash sha msize -> do
      let sizeMismatch =
            case msize of
              Nothing -> False
              Just size -> FileSize (fromIntegral (B.length bs)) /= size
          shaMismatch = sha /= SHA256.hashBytes bs
      when (sizeMismatch || shaMismatch)
        $ error $ "getHackageCabalFile: size or SHA mismatch for " ++ show (pir, bs)
    _ -> pure ()
  pure bs

resolveCabalFileInfo
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageIdentifierRevision
  -> RIO env BlobId
resolveCabalFileInfo pir@(PackageIdentifierRevision name ver cfi) = do
  mres <- inner
  case mres of
    Just res -> pure res
    Nothing -> do
      updated <- updateHackageIndex $ Just $ "Cabal file info not found for " <> display pir <> ", updating"
      mres' <-
        case updated of
          UpdateOccurred -> inner
          NoUpdateOccurred -> pure Nothing
      case mres' of
        Nothing -> fuzzyLookupCandidates name ver >>= throwIO . UnknownHackagePackage pir
        Just res -> pure res
  where
    inner =
      case cfi of
        CFIHash sha _msize -> withStorage $ loadBlobBySHA sha
        CFIRevision rev -> (fmap fst . Map.lookup rev) <$> withStorage (loadHackagePackageVersion name ver)
        CFILatest -> (fmap (fst . fst) . Map.maxView) <$> withStorage (loadHackagePackageVersion name ver)

-- | Given package identifier and package caches, return list of packages
-- with the same name and the same two first version number components found
-- in the caches.
fuzzyLookupCandidates
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> RIO env FuzzyResults
fuzzyLookupCandidates name ver0 = do
  m <- getHackagePackageVersions YesRequireHackageIndex UsePreferredVersions name
  if Map.null m
    then FRNameNotFound <$> getHackageTypoCorrections name
    else
      case Map.lookup ver0 m of
        Nothing -> do
          let withVers vers = pure $ FRVersionNotFound $ flip NE.map vers $ \(ver, revs) ->
                case Map.maxView revs of
                  Nothing -> error "fuzzyLookupCandidates: no revisions"
                  Just (BlobKey sha size, _) -> PackageIdentifierRevision name ver (CFIHash sha (Just size))
          case NE.nonEmpty $ filter (sameMajor . fst) $ Map.toList m of
            Just vers -> withVers vers
            Nothing ->
              case NE.nonEmpty $ Map.toList m of
                Nothing -> error "fuzzyLookupCandidates: no versions"
                Just vers -> withVers vers
        Just revisions ->
          let pirs = map
                (\(BlobKey sha size) -> PackageIdentifierRevision name ver0 (CFIHash sha (Just size)))
                (Map.elems revisions)
           in case NE.nonEmpty pirs of
                Nothing -> error "fuzzyLookupCandidates: no revisions"
                Just pirs' -> pure $ FRRevisionNotFound pirs'
  where
    sameMajor v = toMajorVersion v == toMajorVersion ver0

toMajorVersion :: Version -> [Int]
toMajorVersion v =
  case versionNumbers v of
    []    -> [0, 0]
    [a]   -> [a, 0]
    a:b:_ -> [a, b]

-- | Try to come up with typo corrections for given package identifier
-- using Hackage package names. This can provide more user-friendly
-- information in error messages.
--
-- @since 0.1.0.0
getHackageTypoCorrections
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> RIO env [PackageName]
getHackageTypoCorrections name1 =
    withStorage $ sinkHackagePackageNames
      (\name2 -> name1 `distance` name2 < 4)
      (takeC 10 .| sinkList)
    where
      distance = damerauLevenshtein `on` (T.pack . packageNameString)

-- | Should we pay attention to Hackage's preferred versions?
--
-- @since 0.1.0.0
data UsePreferredVersions = UsePreferredVersions | IgnorePreferredVersions
  deriving Show

-- | Require that the Hackage index is populated.
--
-- @since 0.1.0.0
data RequireHackageIndex
  = YesRequireHackageIndex
    -- ^ If there is nothing in the Hackage index, then perform an update
  | NoRequireHackageIndex
    -- ^ Do not perform an update
  deriving Show

initializeIndex
  :: (HasPantryConfig env, HasLogFunc env)
  => RequireHackageIndex
  -> RIO env ()
initializeIndex NoRequireHackageIndex = pure ()
initializeIndex YesRequireHackageIndex = do
  cabalCount <- withStorage countHackageCabals
  when (cabalCount == 0) $ void $
    updateHackageIndex $ Just $ "No information from Hackage index, updating"

-- | Returns the versions of the package available on Hackage.
--
-- @since 0.1.0.0
getHackagePackageVersions
  :: (HasPantryConfig env, HasLogFunc env)
  => RequireHackageIndex
  -> UsePreferredVersions
  -> PackageName -- ^ package name
  -> RIO env (Map Version (Map Revision BlobKey))
getHackagePackageVersions req usePreferred name = do
  initializeIndex req
  withStorage $ do
    mpreferred <-
      case usePreferred of
        UsePreferredVersions -> loadPreferredVersion name
        IgnorePreferredVersions -> pure Nothing
    let predicate :: Version -> Map Revision BlobKey -> Bool
        predicate = fromMaybe (\_ _ -> True) $ do
          preferredT1 <- mpreferred
          preferredT2 <- T.stripPrefix (T.pack $ packageNameString name) preferredT1
          vr <- Distribution.Text.simpleParse $ T.unpack preferredT2
          Just $ \v _ -> withinRange v vr
    Map.filterWithKey predicate <$> loadHackagePackageVersions name

-- | Returns the versions of the package available on Hackage.
--
-- @since 0.1.0.0
getHackagePackageVersionRevisions
  :: (HasPantryConfig env, HasLogFunc env)
  => RequireHackageIndex
  -> PackageName -- ^ package name
  -> Version -- ^ package version
  -> RIO env (Map Revision BlobKey)
getHackagePackageVersionRevisions req name version = do
  initializeIndex req
  withStorage $
    Map.map snd <$> loadHackagePackageVersion name version

withCachedTree
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => RawPackageLocationImmutable
  -> PackageName
  -> Version
  -> BlobId -- ^ cabal file contents
  -> RIO env Package
  -> RIO env Package
withCachedTree rpli name ver bid inner = do
  mres <- withStorage $ loadHackageTree rpli name ver bid
  case mres of
    Just package -> pure package
    Nothing -> do
      package <- inner
      withStorage $ storeHackageTree name ver bid $ packageTreeKey package
      pure package

getHackageTarballKey
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageIdentifierRevision
  -> RIO env TreeKey
getHackageTarballKey pir@(PackageIdentifierRevision name ver (CFIHash sha _msize)) = do
  mres <- withStorage $ loadHackageTreeKey name ver sha
  case mres of
    Nothing -> packageTreeKey <$> getHackageTarball pir Nothing
    Just key -> pure key
getHackageTarballKey pir = packageTreeKey <$> getHackageTarball pir Nothing

getHackageTarball
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => PackageIdentifierRevision
  -> Maybe TreeKey
  -> RIO env Package
getHackageTarball pir@(PackageIdentifierRevision name ver _cfi) mtreeKey = do
  cabalFile <- resolveCabalFileInfo pir
  cabalFileKey <- withStorage $ getBlobKey cabalFile
  let rpli = RPLIHackage pir mtreeKey
  withCachedTree rpli name ver cabalFile $ do
    mpair <- withStorage $ loadHackageTarballInfo name ver
    (sha, size) <-
      case mpair of
        Just pair -> pure pair
        Nothing -> do
          let exc = NoHackageCryptographicHash $ PackageIdentifier name ver
          updated <- updateHackageIndex $ Just $ display exc <> ", updating"
          mpair2 <-
            case updated of
              UpdateOccurred -> withStorage $ loadHackageTarballInfo name ver
              NoUpdateOccurred -> pure Nothing
          case mpair2 of
            Nothing -> throwIO exc
            Just pair2 -> pure pair2
    pc <- view pantryConfigL
    let urlPrefix = hscDownloadPrefix $ pcHackageSecurity pc
        url = mconcat
          [ urlPrefix
          , "package/"
          , T.pack $ Distribution.Text.display name
          , "-"
          , T.pack $ Distribution.Text.display ver
          , ".tar.gz"
          ]
    package <- getArchivePackage
      rpli
      RawArchive
        { raLocation = ALUrl url
        , raHash = Just sha
        , raSize = Just size
        , raSubdir = T.empty -- no subdirs on Hackage
        }
      RawPackageMetadata
        { rpmName = Just name
        , rpmVersion = Just ver
        , rpmTreeKey = Nothing -- with a revision cabal file will differ giving a different tree
        , rpmCabal = Nothing -- cabal file in the tarball may be different!
        }

    case packageTree package of
      TreeMap m -> do
        let (PCCabalFile (TreeEntry _ ft)) = packageCabalEntry package
            cabalEntry = TreeEntry cabalFileKey ft
            tree' = TreeMap $ Map.insert (cabalFileName name) cabalEntry m
            ident = PackageIdentifier name ver

        cabalBS <- withStorage $ do
          let BlobKey sha' _ = cabalFileKey
          mcabalBS <- loadBlobBySHA sha'
          case mcabalBS of
            Nothing -> error $ "Invariant violated, cabal file key: " ++ show cabalFileKey
            Just bid -> loadBlobById bid

        (_warnings, gpd) <- rawParseGPD (Left rpli) cabalBS
        let gpdIdent = Cabal.package $ Cabal.packageDescription gpd
        when (ident /= gpdIdent) $ throwIO $
          MismatchedCabalFileForHackage pir Mismatch
            { mismatchExpected = ident
            , mismatchActual = gpdIdent
            }

        (_tid, treeKey') <- withStorage $ storeTree rpli ident tree' (BFCabal (cabalFileName name) cabalEntry)
        pure Package
          { packageTreeKey = treeKey'
          , packageTree = tree'
          , packageIdent = ident
          , packageCabalEntry = PCCabalFile cabalEntry
          }
