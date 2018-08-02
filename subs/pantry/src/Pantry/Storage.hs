{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Pantry.Storage
  ( SqlBackend
  , initStorage
  , withStorage
  , storeBlob
  , loadBlob
  , loadBlobById
  , loadBlobBySHA
  , getBlobKey
  , loadURLBlob
  , storeURLBlob
  , clearHackageRevisions
  , storeHackageRevision
  , loadHackagePackageVersions
  , loadHackagePackageVersion
  , loadHackageCabalFile
  , loadLatestCacheUpdate
  , storeCacheUpdate
  , storeHackageTarballInfo
  , loadHackageTarballInfo
  , storeTree
  , loadTree
  , loadTreeById
  , storeHackageTree
  , loadHackageTree
  , loadHackageTreeKey
  , storeArchiveCache
  , loadArchiveCache
  , storeCrlfHack
  , checkCrlfHack
  , checkTreeKey
  , checkPackageMetadata
  , loadPackageIdentFromTree
    -- avoid warnings
  , BlobTableId
  , HackageCabalId
  , HackageTarballId
  , CacheUpdateId
  , SfpId
  , TreeSId
  , TreeEntrySId
  ) where

import RIO
import qualified RIO.ByteString as B
import Pantry.Types
import Database.Persist
import Database.Persist.Sqlite -- FIXME allow PostgreSQL too
import Database.Persist.TH
import RIO.Orphans ()
import Pantry.StaticSHA256
import qualified RIO.Map as Map
import RIO.Time (UTCTime, getCurrentTime)
import Path (Path, Abs, File, toFilePath)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BlobTable sql=blob
    hash StaticSHA256
    size FileSize
    contents ByteString
    UniqueBlobHash hash
UrlBlobTable sql=url_blob
    url Text
    blob BlobTableId
    time UTCTime
    UniqueUrlTime url time
Name sql=package_name
    name PackageNameP
    UniquePackageName name
VersionTable sql=version
    version VersionP
    UniqueVersion version
HackageTarball
    name NameId
    version VersionTableId
    hash StaticSHA256
    size FileSize
    UniqueHackageTarball name version
HackageCabal
    name NameId
    version VersionTableId
    revision Revision
    cabal BlobTableId
    tree TreeSId Maybe
    UniqueHackage name version revision
CacheUpdate
    time UTCTime
    size FileSize
    hash StaticSHA256
ArchiveCache
    time UTCTime
    url Text
    subdir Text
    sha StaticSHA256
    size FileSize
    tree TreeSId

Sfp sql=file_path
    path SafeFilePath
    UniqueSfp path
TreeS sql=tree
    key BlobTableId
    tarball BlobTableId Maybe
    cabal BlobTableId Maybe
    subdir Text Maybe
    UniqueTree key
TreeEntryS sql=tree_entry
    tree TreeSId
    path SfpId
    blob BlobTableId
    type FileType

CrlfHack
    stripped BlobTableId
    original BlobTableId
    UniqueCrlfHack stripped
|]

initStorage
  :: HasLogFunc env
  => Path Abs File -- ^ storage file
  -> (Storage -> RIO env a)
  -> RIO env  a
initStorage fp inner = do
  pool <- createSqlitePool (fromString $ toFilePath fp) 1
  migrates <- runSqlPool (runMigrationSilent migrateAll) pool
  forM_ migrates $ \mig -> logDebug $ "Migration output: " <> display mig
  inner (Storage pool)

withStorage
  :: (HasPantryConfig env, HasLogFunc env)
  => ReaderT SqlBackend (RIO env) a
  -> RIO env a
withStorage action = do
  Storage pool <- view $ pantryConfigL.to pcStorage
  runSqlPool action pool

getNameId
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> ReaderT SqlBackend (RIO env) NameId
getNameId = fmap (either entityKey id) . insertBy . Name . PackageNameP

getVersionId
  :: (HasPantryConfig env, HasLogFunc env)
  => Version
  -> ReaderT SqlBackend (RIO env) VersionTableId
getVersionId = fmap (either entityKey id) . insertBy . VersionTable . VersionP

getPathId
  :: (HasPantryConfig env, HasLogFunc env)
  => SafeFilePath
  -> ReaderT SqlBackend (RIO env) SfpId
getPathId = fmap (either entityKey id) . insertBy . Sfp

storeBlob
  :: (HasPantryConfig env, HasLogFunc env)
  => ByteString
  -> ReaderT SqlBackend (RIO env) (BlobTableId, BlobKey)
storeBlob bs = do
  let sha = mkStaticSHA256FromBytes bs
      size = FileSize $ fromIntegral $ B.length bs
  keys <- selectKeysList [BlobTableHash ==. sha] []
  key <-
    case keys of
      [] -> insert BlobTable
              { blobTableHash = sha
              , blobTableSize = size
              , blobTableContents = bs
              }
      key:rest -> assert (null rest) (pure key)
  pure (key, BlobKey sha size)

loadBlob
  :: (HasPantryConfig env, HasLogFunc env)
  => BlobKey
  -> ReaderT SqlBackend (RIO env) (Maybe ByteString)
loadBlob (BlobKey sha size) = do
  ment <- getBy $ UniqueBlobHash sha
  case ment of
    Nothing -> pure Nothing
    Just (Entity _ bt)
      | blobTableSize bt == size -> pure $ Just $ blobTableContents bt
      | otherwise ->
          Nothing <$ lift (logWarn $
             "Mismatched blob size detected for SHA " <> display sha <>
             ". Expected size: " <> display size <>
             ". Actual size: " <> display (blobTableSize bt))

loadBlobBySHA
  :: (HasPantryConfig env, HasLogFunc env)
  => StaticSHA256
  -> ReaderT SqlBackend (RIO env) (Maybe ByteString)
loadBlobBySHA sha = fmap (fmap (blobTableContents . entityVal)) $ getBy $ UniqueBlobHash sha

loadBlobById
  :: (HasPantryConfig env, HasLogFunc env)
  => BlobTableId
  -> ReaderT SqlBackend (RIO env) ByteString
loadBlobById bid = do
  mbt <- get bid
  case mbt of
    Nothing -> error "loadBlobById: ID doesn't exist in database"
    Just bt -> pure $ blobTableContents bt

getBlobKey
  :: (HasPantryConfig env, HasLogFunc env)
  => BlobTableId
  -> ReaderT SqlBackend (RIO env) BlobKey
getBlobKey bid = do
  res <- rawSql "SELECT hash, size FROM blob WHERE id=?" [toPersistValue bid]
  case res of
    [] -> error $ "getBlobKey failed due to missing ID: " ++ show bid
    [(Single sha, Single size)] -> pure $ BlobKey sha size
    _ -> error $ "getBlobKey failed due to non-unique ID: " ++ show (bid, res)

getBlobTableId
  :: (HasPantryConfig env, HasLogFunc env)
  => BlobKey
  -> ReaderT SqlBackend (RIO env) (Maybe BlobTableId)
getBlobTableId (BlobKey sha size) = do
  res <- rawSql "SELECT id FROM blob WHERE hash=? AND size=?"
           [toPersistValue sha, toPersistValue size]
  pure $ listToMaybe $ map unSingle res

loadURLBlob
  :: (HasPantryConfig env, HasLogFunc env)
  => Text
  -> ReaderT SqlBackend (RIO env) (Maybe ByteString)
loadURLBlob url = do
  ment <- rawSql
    "SELECT blob.contents\n\
    \FROM blob, url_blob\n\
    \WHERE url=?\
    \  AND url_blob.blob=blob.id\n\
    \ ORDER BY url_blob.time DESC"
    [toPersistValue url]
  case ment of
    [] -> pure Nothing
    (Single bs) : _ -> pure $ Just bs

storeURLBlob
  :: (HasPantryConfig env, HasLogFunc env)
  => Text
  -> ByteString
  -> ReaderT SqlBackend (RIO env) ()
storeURLBlob url blob = do
  (blobId, _) <- storeBlob blob
  now <- getCurrentTime
  insert_ UrlBlobTable
        { urlBlobTableUrl = url
        , urlBlobTableBlob = blobId
        , urlBlobTableTime = now
        }

clearHackageRevisions
  :: (HasPantryConfig env, HasLogFunc env)
  => ReaderT SqlBackend (RIO env) ()
clearHackageRevisions = deleteWhere ([] :: [Filter HackageCabal])

storeHackageRevision
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> BlobTableId
  -> ReaderT SqlBackend (RIO env) ()
storeHackageRevision name version key = do
  nameid <- getNameId name
  versionid <- getVersionId version
  rev <- count
    [ HackageCabalName ==. nameid
    , HackageCabalVersion ==. versionid
    ]
  insert_ HackageCabal
    { hackageCabalName = nameid
    , hackageCabalVersion = versionid
    , hackageCabalRevision = Revision (fromIntegral rev)
    , hackageCabalCabal = key
    , hackageCabalTree = Nothing
    }

-- FIXME something to update the hackageCabalTree when we have it

loadHackagePackageVersions
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> ReaderT SqlBackend (RIO env) (Map Version (Map Revision BlobKey))
loadHackagePackageVersions name = do
  nameid <- getNameId name
  -- would be better with esequeleto
  (Map.fromListWith Map.union . map go) <$> rawSql
    "SELECT hackage.revision, version.version, blob.hash, blob.size\n\
    \FROM hackage_cabal as hackage, version, blob\n\
    \WHERE hackage.name=?\n\
    \AND   hackage.version=version.id\n\
    \AND   hackage.cabal=blob.id"
    [toPersistValue nameid]
  where
    go (Single revision, Single (VersionP version), Single key, Single size) =
      (version, Map.singleton revision (BlobKey key size))

loadHackagePackageVersion
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> ReaderT SqlBackend (RIO env) (Map Revision (BlobTableId, BlobKey))
loadHackagePackageVersion name version = do
  nameid <- getNameId name
  versionid <- getVersionId version
  -- would be better with esequeleto
  (Map.fromList . map go) <$> rawSql
    "SELECT hackage.revision, blob.hash, blob.size, blob.id\n\
    \FROM hackage_cabal as hackage, version, blob\n\
    \WHERE hackage.name=?\n\
    \AND   hackage.version=?\n\
    \AND   hackage.cabal=blob.id"
    [toPersistValue nameid, toPersistValue versionid]
  where
    go (Single revision, Single sha, Single size, Single bid) =
      (revision, (bid, BlobKey sha size))

loadHackageCabalFile
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> CabalFileInfo
  -> ReaderT SqlBackend (RIO env) (Maybe ByteString)
loadHackageCabalFile name version cfi = do
  nameid <- getNameId name
  versionid <- getVersionId version
  case cfi of
    CFILatest -> selectFirst
      [ HackageCabalName ==. nameid
      , HackageCabalVersion ==. versionid
      ]
      [Desc HackageCabalRevision] >>= withHackEnt
    CFIRevision rev ->
      getBy (UniqueHackage nameid versionid rev) >>= withHackEnt
    CFIHash sha msize -> do
      ment <- getBy $ UniqueBlobHash sha
      pure $ do
        Entity _ bt <- ment
        case msize of
          Nothing -> pure ()
          Just size -> guard $ blobTableSize bt == size -- FIXME report an error if this mismatches?
        -- FIXME also consider validating the ByteString length against blobTableSize
        pure $ blobTableContents bt
  where
    withHackEnt = traverse $ \(Entity _ h) -> do
      Just blob <- get $ hackageCabalCabal h
      pure $ blobTableContents blob

loadLatestCacheUpdate
  :: (HasPantryConfig env, HasLogFunc env)
  => ReaderT SqlBackend (RIO env) (Maybe (FileSize, StaticSHA256))
loadLatestCacheUpdate =
    fmap go <$> selectFirst [] [Desc CacheUpdateTime]
  where
    go (Entity _ cu) = (cacheUpdateSize cu, cacheUpdateHash cu)

storeCacheUpdate
  :: (HasPantryConfig env, HasLogFunc env)
  => FileSize
  -> StaticSHA256
  -> ReaderT SqlBackend (RIO env) ()
storeCacheUpdate size hash' = do
  now <- getCurrentTime
  insert_ CacheUpdate
    { cacheUpdateTime = now
    , cacheUpdateSize = size
    , cacheUpdateHash = hash'
    }

storeHackageTarballInfo
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> StaticSHA256
  -> FileSize
  -> ReaderT SqlBackend (RIO env) ()
storeHackageTarballInfo name version sha size = do
  nameid <- getNameId name
  versionid <- getVersionId version
  void $ insertBy HackageTarball
    { hackageTarballName = nameid
    , hackageTarballVersion = versionid
    , hackageTarballHash = sha
    , hackageTarballSize = size
    }

loadHackageTarballInfo
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> ReaderT SqlBackend (RIO env) (Maybe (StaticSHA256, FileSize))
loadHackageTarballInfo name version = do
  nameid <- getNameId name
  versionid <- getVersionId version
  fmap go <$> getBy (UniqueHackageTarball nameid versionid)
  where
    go (Entity _ ht) = (hackageTarballHash ht, hackageTarballSize ht)

storeTree
  :: (HasPantryConfig env, HasLogFunc env)
  => Tree
  -> ReaderT SqlBackend (RIO env) (TreeSId, TreeKey)
storeTree tree = do
  (bid, blobKey) <- storeBlob $ renderTree tree
  case tree of
    TreeMap m -> do
      etid <- insertBy TreeS
        { treeSKey = bid
        , treeSTarball = Nothing
        , treeSCabal = Nothing -- FIXME maybe fill in some data here?
        , treeSSubdir = Nothing
        }
      case etid of
        Left (Entity tid _) -> pure (tid, TreeKey blobKey) -- already in database, assume it matches
        Right tid -> do
          for_ (Map.toList m) $ \(sfp, TreeEntry blobKey' ft) -> do
            sfpid <- getPathId sfp
            mbid <- getBlobTableId blobKey'
            bid' <-
              case mbid of
                Nothing -> error $ "Cannot store tree, contains unknown blob: " ++ show blobKey'
                Just bid' -> pure bid'
            insert_ TreeEntryS
              { treeEntrySTree = tid
              , treeEntrySPath = sfpid
              , treeEntrySBlob = bid'
              , treeEntrySType = ft
              }
          pure (tid, TreeKey blobKey)

loadTree
  :: (HasPantryConfig env, HasLogFunc env)
  => TreeKey
  -> ReaderT SqlBackend (RIO env) (Maybe Tree)
loadTree (TreeKey key) = do
  mbid <- getBlobTableId key
  case mbid of
    Nothing -> pure Nothing
    Just bid -> do
      ment <- getBy $ UniqueTree bid
      case ment of
        Nothing -> pure Nothing
        Just ent -> Just <$> loadTreeByEnt ent

loadTreeById
  :: (HasPantryConfig env, HasLogFunc env)
  => TreeSId
  -> ReaderT SqlBackend (RIO env) (TreeKey, Tree)
loadTreeById tid = do
  Just ts <- get tid
  tree <- loadTreeByEnt $ Entity tid ts
  key <- getBlobKey $ treeSKey ts
  pure (TreeKey key, tree)

loadTreeByEnt
  :: (HasPantryConfig env, HasLogFunc env)
  => Entity TreeS
  -> ReaderT SqlBackend (RIO env) Tree
loadTreeByEnt (Entity tid t) = do
  case (treeSTarball t, treeSCabal t, treeSSubdir t) of
    (Just tarball, Just cabal, Just subdir) -> do
      tarballkey <- getBlobKey tarball
      cabalkey <- getBlobKey cabal
      error "we don't support TreeTarball yet"
      {-
      pure $ TreeTarball PackageTarball
        { ptBlob = tarballkey
        , ptCabal = cabalkey
        , ptSubdir = T.unpack subdir
        }
      -}
    (x, y, z) -> assert (isNothing x && isNothing y && isNothing z) $ do
      entries <- rawSql
        "SELECT file_path.path, blob.hash, blob.size, tree_entry.type\n\
        \FROM tree_entry, blob, file_path\n\
        \WHERE tree_entry.tree=?\n\
        \AND   tree_entry.blob=blob.id\n\
        \AND   tree_entry.path=file_path.id"
        [toPersistValue tid]
      pure $ TreeMap $ Map.fromList $ map
        (\(Single sfp, Single sha, Single size, Single ft) ->
             (sfp, TreeEntry (BlobKey sha size) ft))
        entries

storeHackageTree
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> BlobTableId
  -> TreeSId
  -> ReaderT SqlBackend (RIO env) ()
storeHackageTree name version cabal tid = do
  nameid <- getNameId name
  versionid <- getVersionId version
  updateWhere
    [ HackageCabalName ==. nameid
    , HackageCabalVersion ==. versionid
    , HackageCabalCabal ==. cabal
    ]
    [HackageCabalTree =. Just tid]

loadHackageTreeKey
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> StaticSHA256
  -> ReaderT SqlBackend (RIO env) (Maybe TreeKey)
loadHackageTreeKey name ver sha = do
  res <- rawSql
    "SELECT treeblob.hash, treeblob.size\n\
    \FROM blob as treeblob, blob as cabalblob, package_name, version, hackage_cabal, tree\n\
    \WHERE package_name.name=?\n\
    \AND   version.version=?\n\
    \AND   cabalblob.hash=?\n\
    \AND   hackage_cabal.name=package_name.id\n\
    \AND   hackage_cabal.version=version.id\n\
    \AND   hackage_cabal.cabal=cabalblob.id\n\
    \AND   hackage_cabal.tree=tree.id\n\
    \AND   tree.key=treeblob.id"
    [ toPersistValue $ PackageNameP name
    , toPersistValue $ VersionP ver
    , toPersistValue sha
    ]
  case res of
    [] -> pure Nothing
    (Single treesha, Single size):_ -> pure $ Just $ TreeKey $ BlobKey treesha size

loadHackageTree
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> BlobTableId
  -> ReaderT SqlBackend (RIO env) (Maybe (TreeKey, Tree))
loadHackageTree name ver bid = do
  nameid <- getNameId name
  versionid <- getVersionId ver
  ment <- selectFirst
    [ HackageCabalName ==. nameid
    , HackageCabalVersion ==. versionid
    , HackageCabalCabal ==. bid
    , HackageCabalTree !=. Nothing
    ]
    []
  case ment of
    Nothing -> pure Nothing
    Just (Entity _ hc) ->
      case hackageCabalTree hc of
        Nothing -> assert False $ pure Nothing
        Just x -> Just <$> loadTreeById x

storeArchiveCache
  :: (HasPantryConfig env, HasLogFunc env)
  => Text -- ^ URL
  -> Text -- ^ subdir
  -> StaticSHA256
  -> FileSize
  -> TreeSId
  -> ReaderT SqlBackend (RIO env) ()
storeArchiveCache url subdir sha size tid = do
  now <- getCurrentTime
  insert_ ArchiveCache
    { archiveCacheTime = now
    , archiveCacheUrl = url
    , archiveCacheSubdir = subdir
    , archiveCacheSha = sha
    , archiveCacheSize = size
    , archiveCacheTree = tid
    }

loadArchiveCache
  :: (HasPantryConfig env, HasLogFunc env)
  => Text -- ^ URL
  -> Text -- ^ subdir
  -> ReaderT SqlBackend (RIO env) [(StaticSHA256, FileSize, TreeSId)]
loadArchiveCache url subdir = map go <$> selectList
  [ ArchiveCacheUrl ==. url
  , ArchiveCacheSubdir ==. subdir
  ]
  [Desc ArchiveCacheTime]
  where
    go (Entity _ ac) = (archiveCacheSha ac, archiveCacheSize ac, archiveCacheTree ac)

-- Back in the days of all-cabal-hashes, we had a few cabal files that
-- had CRLF/DOS-style line endings in them. The Git version ended up
-- stripping out those CRLFs. Now, the hashes in those old Stackage
-- snapshots don't match up to any hash in the 01-index.tar file. This
-- table lets us undo that mistake, but mapping back from the stripped
-- version to the original. This is used by the Pantry.OldStackage
-- module. Once we convert all snapshots and stop using the old
-- format, this hack can disappear entirely.
storeCrlfHack
  :: (HasPantryConfig env, HasLogFunc env)
  => BlobTableId -- ^ stripped
  -> BlobTableId -- ^ original
  -> ReaderT SqlBackend (RIO env) ()
storeCrlfHack stripped orig = void $ insertBy CrlfHack
  { crlfHackStripped = stripped
  , crlfHackOriginal = orig
  }

checkCrlfHack
  :: (HasPantryConfig env, HasLogFunc env)
  => BlobKey -- ^ from the Stackage snapshot
  -> ReaderT SqlBackend (RIO env) BlobKey
checkCrlfHack stripped = do
  mstrippedId <- getBlobTableId stripped
  strippedId <-
    case mstrippedId of
      Nothing -> error $ "checkCrlfHack: no ID found for " ++ show stripped
      Just x -> pure x
  ment <- getBy $ UniqueCrlfHack strippedId
  case ment of
    Nothing -> pure stripped
    Just (Entity _ ch) -> getBlobKey $ crlfHackOriginal ch

checkTreeKey
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> Maybe TreeKey
  -> RIO env (TreeKey, Tree)
  -> RIO env (TreeKey, Tree)
checkTreeKey _ Nothing inner = inner
checkTreeKey pl (Just expectedTreeKey) inner = do
  undefined

    {-
        for_ mtreeKey $ \expectedKey -> when (treeKey /= expectedKey) $
          throwIO $ TreeKeyMismatch (PLHackage pir mtreeKey) expectedKey treeKey
    -}

-- ensure name, version, etc are correct
checkPackageMetadata
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> PackageMetadata
  -> RIO env (TreeKey, Tree)
  -> RIO env (TreeKey, Tree)
checkPackageMetadata pl pm inner = do
  (treeKey, tree) <- checkTreeKey pl (pmTree pm) inner
  -- even if we aren't given a name and version, still load this to
  -- force the check of the cabal file name being accurate
  (cabalBlobKey, ident@(PackageIdentifier name version))
    <- loadPackageIdentFromTree pl tree
  let err = throwIO $ MismatchedPackageMetadata pl pm cabalBlobKey ident
  for_ (pmName pm) $ \name' -> when (name /= name') err
  for_ (pmVersion pm) $ \version' -> when (version /= version') err
  for_ (pmCabal pm) $ \cabal' -> when (cabalBlobKey /= cabal') err
  pure (treeKey, tree)

-- | Returns the cabal blob key
loadPackageIdentFromTree
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageLocation
  -> Tree
  -> RIO env (BlobKey, PackageIdentifier)
loadPackageIdentFromTree pl tree = undefined
  -- FIXME ensure that the cabal file name match the package name
