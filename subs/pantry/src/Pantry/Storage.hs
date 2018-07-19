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
  , storeHackageTree
  , loadHackageTree
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
import qualified RIO.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BlobTable sql=blob
    hash BlobKey
    size Word
    contents ByteString
    UniqueBlobHash hash
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
    size Word
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
    size Word
    hash StaticSHA256

Sfp sql=file_path
    path SafeFilePath
    UniqueSfp path
TreeS sql=tree
    key TreeKey
    tarball BlobTableId Maybe
    cabal BlobTableId Maybe
    subdir Text Maybe
    UniqueTree key
TreeEntryS sql=tree_entry
    tree TreeSId
    path SfpId
    blob BlobTableId
    type FileType
|]

initStorage
  :: HasLogFunc env
  => FilePath -- ^ storage file
  -> RIO env Storage
initStorage fp = do
  pool <- createSqlitePool (fromString fp) 1
  migrates <- runSqlPool (runMigrationSilent migrateAll) pool
  forM_ migrates $ \mig -> logDebug $ "Migration output: " <> display mig
  pure (Storage pool)

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

storeBlob
  :: (HasPantryConfig env, HasLogFunc env)
  => ByteString
  -> ReaderT SqlBackend (RIO env) (BlobTableId, BlobKey)
storeBlob bs = do
  let blobKey = BlobKey $ mkStaticSHA256FromBytes bs
  keys <- selectKeysList [BlobTableHash ==. blobKey] []
  key <-
    case keys of
      [] -> insert BlobTable
              { blobTableHash = blobKey
              , blobTableSize = fromIntegral $ B.length bs
              , blobTableContents = bs
              }
      key:rest -> assert (null rest) (pure key)
  pure (key, blobKey)

getBlobKey
  :: (HasPantryConfig env, HasLogFunc env)
  => BlobTableId
  -> ReaderT SqlBackend (RIO env) BlobKey
getBlobKey bid = do
  res <- rawSql "SELECT hash FROM blob WHERE id=?" [toPersistValue bid]
  case res of
    [] -> error $ "getBlobKey failed due to missing ID: " ++ show bid
    [Single x] -> pure x
    _ -> error $ "getBlobKey failed due to non-unique ID: " ++ show (bid, res)

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
  -> ReaderT SqlBackend (RIO env) (Map Version (Map Revision CabalHash))
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
      (version, Map.singleton revision (CabalHash key (Just size)))

loadHackagePackageVersion
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> ReaderT SqlBackend (RIO env) (Map Revision (StaticSHA256, Word, BlobTableId))
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
    go (Single revision, Single key, Single size, Single bid) =
      (revision, (key, size, bid))

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
    CFIHash (CabalHash (BlobKey -> blobKey) msize) -> do
      ment <- getBy $ UniqueBlobHash blobKey
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

    {-
CacheUpdate
    time UTCTime
    size Word
    hash StaticSHA256
    -}

loadLatestCacheUpdate
  :: (HasPantryConfig env, HasLogFunc env)
  => ReaderT SqlBackend (RIO env) (Maybe (Word, StaticSHA256))
loadLatestCacheUpdate =
    fmap go <$> selectFirst [] [Desc CacheUpdateTime]
  where
    go (Entity _ cu) = (cacheUpdateSize cu, cacheUpdateHash cu)

storeCacheUpdate
  :: (HasPantryConfig env, HasLogFunc env)
  => Word
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
  -> Word
  -> ReaderT SqlBackend (RIO env) ()
storeHackageTarballInfo name version sha size = do
  nameid <- getNameId name
  versionid <- getVersionId version
  insert_ HackageTarball
    { hackageTarballName = nameid
    , hackageTarballVersion = versionid
    , hackageTarballHash = sha
    , hackageTarballSize = size
    }

loadHackageTarballInfo
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> ReaderT SqlBackend (RIO env) (Maybe (StaticSHA256, Word))
loadHackageTarballInfo name version = do
  nameid <- getNameId name
  versionid <- getVersionId version
  fmap go <$> getBy (UniqueHackageTarball nameid versionid)
  where
    go (Entity _ ht) = (hackageTarballHash ht, hackageTarballSize ht)

storeTree
  :: (HasPantryConfig env, HasLogFunc env)
  => Tree
  -> ReaderT SqlBackend (RIO env) TreeKey
storeTree = undefined

loadTree
  :: (HasPantryConfig env, HasLogFunc env)
  => TreeKey
  -> ReaderT SqlBackend (RIO env) (Maybe Tree)
loadTree key = do
  ment <- getBy $ UniqueTree key
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
  pure (treeSKey ts, tree)

loadTreeByEnt
  :: (HasPantryConfig env, HasLogFunc env)
  => Entity TreeS
  -> ReaderT SqlBackend (RIO env) Tree
loadTreeByEnt (Entity tid t) = do
  case (treeSTarball t, treeSCabal t, treeSSubdir t) of
    (Just tarball, Just cabal, Just subdir) -> do
      tarballkey <- getBlobKey tarball
      cabalkey <- getBlobKey cabal
      pure $ TreeTarball PackageTarball
        { ptBlob = tarballkey
        , ptCabal = cabalkey
        , ptSubdir = T.unpack subdir
        }
    (x, y, z) -> assert (isNothing x && isNothing y && isNothing z) $ do
      entries <- rawSql
        "SELECT file_path.path, blob.hash, tree_entry.type\n\
        \FROM tree_entry, blob, file_path\n\
        \WHERE tree_entry.id=?\n\
        \AND   tree_entry.blob=blob.id\n\
        \AND   tree_entry.path=file_path.id"
        [toPersistValue tid]
      pure $ TreeMap $ Map.fromList $ map
        (\(Single sfp, Single blobKey, Single ft) ->
             (sfp, TreeEntry blobKey ft))
        entries

storeHackageTree
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> BlobTableId
  -> TreeSId
  -> ReaderT SqlBackend (RIO env) ()
storeHackageTree = undefined

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
