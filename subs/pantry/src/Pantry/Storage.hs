{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
module Pantry.Storage
  ( SqlBackend
  , initStorage
  , withStorage
  , migrateAll
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
  , loadLatestCacheUpdate
  , storeCacheUpdate
  , storeHackageTarballInfo
  , loadHackageTarballInfo
  , getHPackBlobKeyById
  , storeTree
  , loadTree
  , storeHPack
  , loadPackageById
  , getPackageNameById
  , getPackageNameId
  , getVersionId
  , getTreeForKey
  , storeHackageTree
  , loadHackageTree
  , loadHackageTreeKey
  , storeArchiveCache
  , loadArchiveCache
  , storeRepoCache
  , loadRepoCache
  , storePreferredVersion
  , loadPreferredVersion
  , sinkHackagePackageNames
  , loadCabalBlobKey
  , hpackToCabal
  , countHackageCabals
  , getSnapshotCacheByHash
  , getSnapshotCacheId
  , storeSnapshotModuleCache
  , loadExposedModulePackages
  , PackageNameId
  , PackageName
  , VersionId
  , ModuleNameId
  , Version
  , Unique(..)
  , EntityField(..)
    -- avoid warnings
  , BlobId
  , Key(unBlobKey)
  , HackageCabalId
  , HackageCabal(..)
  , HackageTarballId
  , CacheUpdateId
  , FilePathId
  , Tree(..)
  , TreeId
  , TreeEntry(..)
  , TreeEntryId
  , ArchiveCacheId
  , RepoCacheId
  , PreferredVersionsId
  , UrlBlobId
  , SnapshotCacheId
  , PackageExposedModuleId
  ) where

import RIO hiding (FilePath)
import RIO.Process
import qualified RIO.ByteString as B
import qualified Pantry.Types as P
import qualified RIO.List as List
import qualified RIO.FilePath as FilePath
import RIO.FilePath ((</>), takeDirectory)
import RIO.Directory (createDirectoryIfMissing, setPermissions, getPermissions, setOwnerExecutable)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import RIO.Orphans ()
import qualified Pantry.SHA256 as SHA256
import qualified RIO.Map as Map
import qualified RIO.Text as T
import RIO.Time (UTCTime, getCurrentTime)
import Path (Path, Abs, File, Dir, toFilePath, filename, parseAbsDir, fromAbsFile, fromRelFile)
import Path.IO (listDir, createTempDir, getTempDir, removeDirRecur)
import Pantry.HPack (hpackVersion, hpack)
import Conduit
import Data.Acquire (with)
import Pantry.Types (PackageNameP (..), VersionP (..), SHA256, FileSize (..), FileType (..), HasPantryConfig, BlobKey, Repo (..), TreeKey, SafeFilePath, Revision (..), Package (..), SnapshotCacheHash (..))
import qualified Pantry.SQLite as SQLite

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- Raw blobs
Blob
    sha SHA256
    size FileSize
    contents ByteString
    UniqueBlobSha sha
-- Previously downloaded blobs from given URLs.
-- May change over time, so we keep a time column too.
UrlBlob sql=url_blob
    url Text
    blob BlobId
    time UTCTime
    UniqueUrlTime url time

-- For normalization, and avoiding storing strings in a bunch of
-- tables.
PackageName
    name P.PackageNameP
    UniquePackageName name
Version
    version P.VersionP
    UniqueVersion version
FilePath
    path P.SafeFilePath
    UniqueSfp path

-- Secure download information for a package on Hackage. This does not
-- contain revision information, since sdist tarballs are (blessedly)
-- unmodified on Hackage.
HackageTarball
    name PackageNameId
    version VersionId
    sha SHA256
    size FileSize
    UniqueHackageTarball name version

-- An individual cabal file from Hackage, representing a specific
-- revision.
HackageCabal
    name PackageNameId
    version VersionId
    revision P.Revision
    cabal BlobId

    -- If available: the full tree containing the HackageTarball
    -- contents with the cabal file modified.
    tree TreeId Maybe
    UniqueHackage name version revision

-- Any preferred-version information from Hackage
PreferredVersions
    name PackageNameId
    preferred Text
    UniquePreferred name

-- Last time we downloaded a 01-index.tar file from Hackage and
-- updated the three previous tables.
CacheUpdate
    -- When did we do the update?
    time UTCTime

    -- How big was the file when we updated, ignoring the last two
    -- all-null 512-byte blocks.
    size FileSize

    -- SHA256 of the first 'size' bytes of the file
    sha SHA256

-- A tree containing a Haskell package. See associated TreeEntry
-- table.
Tree
    key BlobId

    -- If the treeCabal field is Nothing, it means the Haskell package
    -- doesn't have a corresponding cabal file for it. This may be the case
    -- for haskell package referenced by git repository with only a hpack file.
    cabal BlobId Maybe
    cabalType FileType
    name PackageNameId
    version VersionId
    UniqueTree key

HPack
   tree TreeId

   -- hpack version used for generating this cabal file
   version VersionId

   -- Generated cabal file for the given tree and hpack version
   cabalBlob BlobId
   cabalPath FilePathId

   UniqueHPack tree version

-- An individual file within a Tree.
TreeEntry
    tree TreeId
    path FilePathId
    blob BlobId
    type FileType

-- Like UrlBlob, but stores the contents as a Tree.
ArchiveCache
    time UTCTime
    url Text
    subdir Text
    sha SHA256
    size FileSize
    tree TreeId

-- Like ArchiveCache, but for a Repo.
RepoCache
    time UTCTime
    url Text
    type P.RepoType
    commit Text
    subdir Text
    tree TreeId

-- Identified by sha of all immutable packages contained in a snapshot
-- and GHC version used
SnapshotCache
    sha SHA256
    UniqueSnapshotCache sha

PackageExposedModule
    snapshotCache SnapshotCacheId
    module ModuleNameId
    package PackageNameId

ModuleName
    name P.ModuleNameP
    UniqueModule name
|]

initStorage
  :: HasLogFunc env
  => Path Abs File -- ^ storage file
  -> (P.Storage -> RIO env a)
  -> RIO env a
initStorage =
  SQLite.initStorage "Pantry" migrateAll

withStorage
  :: (HasPantryConfig env, HasLogFunc env)
  => ReaderT SqlBackend (RIO env) a
  -> RIO env a
withStorage action =
  flip SQLite.withStorage_ action =<< view (P.pantryConfigL.to P.pcStorage)

-- | This is a helper type to distinguish db queries between different rdbms backends. The important
-- part is that the affects described in this data type should be semantically equivalent between
-- the supported engines.
data RdbmsActions env a = RdbmsActions
  { raSqlite :: !(ReaderT SqlBackend (RIO env) a)
  -- ^ A query that is specific to SQLite
  , raPostgres :: !(ReaderT SqlBackend (RIO env) a)
  -- ^ A query that is specific to PostgreSQL
  }

-- | This function provides a way to create queries supported by multiple sql backends.
rdbmsAwareQuery
  :: RdbmsActions env a
  -> ReaderT SqlBackend (RIO env) a
rdbmsAwareQuery RdbmsActions {raSqlite, raPostgres} = do
  rdbms <- connRDBMS <$> ask
  case rdbms of
    "postgresql" -> raPostgres
    "sqlite" -> raSqlite
    _ -> error $ "rdbmsAwareQuery: unsupported rdbms '" ++ T.unpack rdbms ++ "'"


getPackageNameById
  :: PackageNameId
  -> ReaderT SqlBackend (RIO env) (Maybe P.PackageName)
getPackageNameById = fmap (unPackageNameP . packageNameName <$>) . get


getPackageNameId
  :: P.PackageName
  -> ReaderT SqlBackend (RIO env) PackageNameId
getPackageNameId = fmap (either entityKey id) . insertBy . PackageName . PackageNameP

getVersionId
  :: P.Version
  -> ReaderT SqlBackend (RIO env) VersionId
getVersionId = fmap (either entityKey id) . insertBy . Version . VersionP

storeBlob
  :: ByteString
  -> ReaderT SqlBackend (RIO env) (BlobId, BlobKey)
storeBlob bs = do
  let sha = SHA256.hashBytes bs
      size = FileSize $ fromIntegral $ B.length bs
  keys <- selectKeysList [BlobSha ==. sha] []
  key <-
    case keys of
      [] ->
        rdbmsAwareQuery
          RdbmsActions
            { raSqlite =
                insert Blob {blobSha = sha, blobSize = size, blobContents = bs}
            , raPostgres =
                do rawExecute
                     "INSERT INTO blob(sha, size, contents) VALUES (?, ?, ?) ON CONFLICT DO NOTHING"
                     [ toPersistValue sha
                     , toPersistValue size
                     , toPersistValue bs
                     ]
                   rawSql
                     "SELECT blob.id FROM blob WHERE blob.sha = ?"
                     [toPersistValue sha] >>= \case
                     [Single key] -> pure key
                     _ ->
                       error
                         "soreBlob: there was a critical problem storing a blob."
            }
      key:rest -> assert (null rest) (pure key)
  pure (key, P.BlobKey sha size)

loadBlob ::
     HasLogFunc env
  => BlobKey
  -> ReaderT SqlBackend (RIO env) (Maybe ByteString)
loadBlob (P.BlobKey sha size) = do
  ment <- getBy $ UniqueBlobSha sha
  case ment of
    Nothing -> pure Nothing
    Just (Entity _ bt)
      | blobSize bt == size -> pure $ Just $ blobContents bt
      | otherwise ->
          Nothing <$ lift (logWarn $
             "Mismatched blob size detected for SHA " <> display sha <>
             ". Expected size: " <> display size <>
             ". Actual size: " <> display (blobSize bt))

loadBlobBySHA :: SHA256 -> ReaderT SqlBackend (RIO env) (Maybe BlobId)
loadBlobBySHA sha = listToMaybe <$> selectKeysList [BlobSha ==. sha] []

loadBlobById :: BlobId -> ReaderT SqlBackend (RIO env) ByteString
loadBlobById bid = do
  mbt <- get bid
  case mbt of
    Nothing -> error "loadBlobById: ID doesn't exist in database"
    Just bt -> pure $ blobContents bt

getBlobKey :: BlobId -> ReaderT SqlBackend (RIO env) BlobKey
getBlobKey bid = do
  res <- rawSql "SELECT sha, size FROM blob WHERE id=?" [toPersistValue bid]
  case res of
    [] -> error $ "getBlobKey failed due to missing ID: " ++ show bid
    [(Single sha, Single size)] -> pure $ P.BlobKey sha size
    _ -> error $ "getBlobKey failed due to non-unique ID: " ++ show (bid, res)

getBlobId :: BlobKey -> ReaderT SqlBackend (RIO env) (Maybe BlobId)
getBlobId (P.BlobKey sha size) = do
  res <- rawSql "SELECT id FROM blob WHERE sha=? AND size=?"
           [toPersistValue sha, toPersistValue size]
  pure $ listToMaybe $ map unSingle res

loadURLBlob :: Text -> ReaderT SqlBackend (RIO env) (Maybe ByteString)
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

storeURLBlob :: Text -> ByteString -> ReaderT SqlBackend (RIO env) ()
storeURLBlob url blob = do
  (blobId, _) <- storeBlob blob
  now <- getCurrentTime
  insert_ UrlBlob
        { urlBlobUrl = url
        , urlBlobBlob = blobId
        , urlBlobTime = now
        }

clearHackageRevisions :: ReaderT SqlBackend (RIO env) ()
clearHackageRevisions = deleteWhere ([] :: [Filter HackageCabal])

storeHackageRevision ::
     P.PackageName -> P.Version -> BlobId -> ReaderT SqlBackend (RIO env) ()
storeHackageRevision name version key = do
  nameid <- getPackageNameId name
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

loadHackagePackageVersions
  :: P.PackageName
  -> ReaderT SqlBackend (RIO env) (Map P.Version (Map Revision BlobKey))
loadHackagePackageVersions name = do
  nameid <- getPackageNameId name
  -- would be better with esequeleto
  (Map.fromListWith Map.union . map go) <$> rawSql
    "SELECT hackage.revision, version.version, blob.sha, blob.size\n\
    \FROM hackage_cabal as hackage, version, blob\n\
    \WHERE hackage.name=?\n\
    \AND   hackage.version=version.id\n\
    \AND   hackage.cabal=blob.id"
    [toPersistValue nameid]
  where
    go (Single revision, Single (P.VersionP version), Single key, Single size) =
      (version, Map.singleton revision (P.BlobKey key size))

loadHackagePackageVersion
  :: P.PackageName
  -> P.Version
  -> ReaderT SqlBackend (RIO env) (Map Revision (BlobId, P.BlobKey))
loadHackagePackageVersion name version = do
  nameid <- getPackageNameId name
  versionid <- getVersionId version
  -- would be better with esequeleto
  (Map.fromList . map go) <$> rawSql
    "SELECT hackage.revision, blob.sha, blob.size, blob.id\n\
    \FROM hackage_cabal as hackage, version, blob\n\
    \WHERE hackage.name=?\n\
    \AND   hackage.version=?\n\
    \AND   hackage.cabal=blob.id"
    [toPersistValue nameid, toPersistValue versionid]
  where
    go (Single revision, Single sha, Single size, Single bid) =
      (revision, (bid, P.BlobKey sha size))

loadLatestCacheUpdate
  :: ReaderT SqlBackend (RIO env) (Maybe (FileSize, SHA256))
loadLatestCacheUpdate =
    fmap go <$> selectFirst [] [Desc CacheUpdateTime]
  where
    go (Entity _ cu) = (cacheUpdateSize cu, cacheUpdateSha cu)

storeCacheUpdate :: FileSize -> SHA256 -> ReaderT SqlBackend (RIO env) ()
storeCacheUpdate size sha = do
  now <- getCurrentTime
  insert_ CacheUpdate
    { cacheUpdateTime = now
    , cacheUpdateSize = size
    , cacheUpdateSha = sha
    }

storeHackageTarballInfo
  :: P.PackageName
  -> P.Version
  -> SHA256
  -> FileSize
  -> ReaderT SqlBackend (RIO env) ()
storeHackageTarballInfo name version sha size = do
  nameid <- getPackageNameId name
  versionid <- getVersionId version
  void $ insertBy HackageTarball
    { hackageTarballName = nameid
    , hackageTarballVersion = versionid
    , hackageTarballSha = sha
    , hackageTarballSize = size
    }

loadHackageTarballInfo
  :: P.PackageName
  -> P.Version
  -> ReaderT SqlBackend (RIO env) (Maybe (SHA256, FileSize))
loadHackageTarballInfo name version = do
  nameid <- getPackageNameId name
  versionid <- getVersionId version
  fmap go <$> getBy (UniqueHackageTarball nameid versionid)
  where
    go (Entity _ ht) = (hackageTarballSha ht, hackageTarballSize ht)

storeCabalFile ::
       ByteString
    -> P.PackageName
    -> ReaderT SqlBackend (RIO env) BlobId
storeCabalFile cabalBS pkgName = do
    (bid, _) <- storeBlob cabalBS
    let cabalFile = P.cabalFileName pkgName
    _ <- insertBy FilePath {filePathPath = cabalFile}
    return bid

loadFilePath ::
       SafeFilePath
    -> ReaderT SqlBackend (RIO env) (Entity FilePath)
loadFilePath path = do
    fp <- getBy $ UniqueSfp path
    case fp of
        Nothing ->
            error $
            "loadFilePath: No row found for " <>
            (T.unpack $ P.unSafeFilePath path)
        Just record -> return record

loadHPackTreeEntity :: TreeId -> ReaderT SqlBackend (RIO env) (Entity TreeEntry)
loadHPackTreeEntity tid = do
  filepath <- loadFilePath P.hpackSafeFilePath
  let filePathId :: FilePathId = entityKey filepath
  hpackTreeEntry <-
    selectFirst [TreeEntryTree ==. tid, TreeEntryPath ==. filePathId] []
  case hpackTreeEntry of
    Nothing ->
      error $
      "loadHPackTreeEntity: No package.yaml file found in TreeEntry for TreeId:  " ++
      show tid
    Just record -> return record

storeHPack ::
       (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
    => P.RawPackageLocationImmutable
    -> TreeId
    -> ReaderT SqlBackend (RIO env) (Key HPack)
storeHPack rpli tid = do
    vid <- hpackVersionId
    hpackRecord <- getBy (UniqueHPack tid vid)
    case hpackRecord of
      Nothing -> generateHPack rpli tid vid
      Just record -> return $ entityKey record

loadCabalBlobKey :: HPackId -> ReaderT SqlBackend (RIO env) BlobKey
loadCabalBlobKey hpackId = do
  hpackRecord <- getJust hpackId
  getBlobKey $ hPackCabalBlob hpackRecord

generateHPack ::
       (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
    => P.RawPackageLocationImmutable -- ^ for exceptions
    -> TreeId
    -> VersionId
    -> ReaderT SqlBackend (RIO env) (Key HPack)
generateHPack rpli tid vid = do
    tree <- getTree tid
    (pkgName, cabalBS) <- hpackToCabalS rpli tree
    bid <- storeCabalFile cabalBS pkgName
    let cabalFile = P.cabalFileName pkgName
    fid <- insertBy FilePath {filePathPath = cabalFile}
    let hpackRecord =
            HPack
                { hPackTree = tid
                , hPackVersion = vid
                , hPackCabalBlob = bid
                , hPackCabalPath = either entityKey id fid
                }
    either entityKey id <$> insertBy hpackRecord


hpackVersionId ::
       (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
    => ReaderT SqlBackend (RIO env) VersionId
hpackVersionId = do
    hpackSoftwareVersion <- lift hpackVersion
    fmap (either entityKey id) $
      insertBy $
      Version {versionVersion = P.VersionP hpackSoftwareVersion}


getFilePathId :: SafeFilePath -> ReaderT SqlBackend (RIO env) FilePathId
getFilePathId sfp =
  selectKeysList [FilePathPath ==. sfp] [] >>= \case
    [fpId] -> pure fpId
    [] ->
      rdbmsAwareQuery
        RdbmsActions
          { raSqlite = insert $ FilePath sfp
          , raPostgres =
              do rawExecute
                   "INSERT INTO file_path(path) VALUES (?) ON CONFLICT DO NOTHING"
                   [toPersistValue sfp]
                 rawSql
                   "SELECT id FROM file_path WHERE path = ?"
                   [toPersistValue sfp] >>= \case
                   [Single key] -> pure key
                   _ ->
                     error
                       "getFilePathId: there was a critical problem storing a blob."
          }
    _ ->
      error $
      "getFilePathId: FilePath unique constraint key is violated for: " ++ fp
  where
    fp = T.unpack (P.unSafeFilePath sfp)


storeTree
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => P.RawPackageLocationImmutable -- ^ for exceptions
  -> P.PackageIdentifier
  -> P.Tree
  -> P.BuildFile
  -> ReaderT SqlBackend (RIO env) (TreeId, P.TreeKey)
storeTree rpli (P.PackageIdentifier name version) tree@(P.TreeMap m) buildFile = do
  (bid, blobKey) <- storeBlob $ P.renderTree tree
  (cabalid, ftype) <- case buildFile of
                P.BFHpack (P.TreeEntry _ ftype) -> pure (Nothing, ftype)
                P.BFCabal _ (P.TreeEntry (P.BlobKey btypeSha _) ftype) -> do
                               buildTypeid <- loadBlobBySHA btypeSha
                               buildid <-
                                   case buildTypeid of
                                     Just buildId -> pure buildId
                                     Nothing -> error $ "storeTree: " ++ (show buildFile) ++ " BlobKey not found: " ++ show (tree, btypeSha)
                               return (Just buildid, ftype)
  nameid <- getPackageNameId name
  versionid <- getVersionId version
  etid <- insertBy Tree
    { treeKey = bid
    , treeCabal = cabalid
    , treeCabalType = ftype
    , treeName = nameid
    , treeVersion = versionid
    }

  (tid, pTreeKey) <- case etid of
    Left (Entity tid _) -> pure (tid, P.TreeKey blobKey) -- already in database, assume it matches
    Right tid -> do
      for_ (Map.toList m) $ \(sfp, P.TreeEntry blobKey' ft) -> do
        sfpid <- getFilePathId sfp
        mbid <- getBlobId blobKey'
        bid' <-
          case mbid of
            Nothing -> error $ "Cannot store tree, contains unknown blob: " ++ show blobKey'
            Just bid' -> pure bid'
        insert_ TreeEntry
          { treeEntryTree = tid
          , treeEntryPath = sfpid
          , treeEntryBlob = bid'
          , treeEntryType = ft
          }
      pure (tid, P.TreeKey blobKey)
  case buildFile of
    P.BFHpack _ -> storeHPack rpli tid >> return ()
    P.BFCabal _ _ -> return ()
  return (tid, pTreeKey)

getTree :: TreeId -> ReaderT SqlBackend (RIO env) P.Tree
getTree tid = do
  (mts :: Maybe Tree) <- get tid
  ts <-
      case mts of
        Nothing ->
            error $ "getTree: invalid foreign key " ++ show tid
        Just ts -> pure ts
  loadTreeByEnt $ Entity tid ts

loadTree :: P.TreeKey -> ReaderT SqlBackend (RIO env) (Maybe P.Tree)
loadTree key = do
  ment <- getTreeForKey key
  case ment of
    Nothing -> pure Nothing
    Just ent -> Just <$> loadTreeByEnt ent

getTreeForKey
  :: TreeKey
  -> ReaderT SqlBackend (RIO env) (Maybe (Entity Tree))
getTreeForKey (P.TreeKey key) = do
  mbid <- getBlobId key
  case mbid of
    Nothing -> pure Nothing
    Just bid -> getBy $ UniqueTree bid

loadPackageById ::
       (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
    => P.RawPackageLocationImmutable -- ^ for exceptions
    -> TreeId
    -> ReaderT SqlBackend (RIO env) Package
loadPackageById rpli tid = do
    (mts :: Maybe Tree) <- get tid
    ts <-
        case mts of
            Nothing ->
                error $ "loadPackageById: invalid foreign key " ++ show tid
            Just ts -> pure ts
    (tree :: P.Tree) <- loadTreeByEnt $ Entity tid ts
    (blobKey :: BlobKey) <- getBlobKey $ treeKey ts
    (mname :: Maybe PackageName) <- get $ treeName ts
    name <-
        case mname of
            Nothing ->
                error $
                "loadPackageByid: invalid foreign key " ++ show (treeName ts)
            Just (PackageName (P.PackageNameP name)) -> pure name
    mversion <- get $ treeVersion ts
    version <-
        case mversion of
            Nothing ->
                error $
                "loadPackageByid: invalid foreign key " ++ show (treeVersion ts)
            Just (Version (P.VersionP version)) -> pure version
    let ident = P.PackageIdentifier name version
    (packageEntry, mtree) <-
        case treeCabal ts of
            Just keyBlob -> do
                cabalKey <- getBlobKey keyBlob
                return
                    ( P.PCCabalFile $ P.TreeEntry cabalKey (treeCabalType ts)
                    , tree)
            Nothing -> do
                hpackVid <- hpackVersionId
                hpackEntity <- getBy (UniqueHPack tid hpackVid)
                let (P.TreeMap tmap) = tree
                    cabalFile = P.cabalFileName name
                case hpackEntity of
                    Nothing
                        -- This case will happen when you either
                        -- update stack with a new hpack version or
                        -- use different hpack version via
                        -- --with-hpack option.
                     -> do
                        (hpackId :: HPackId) <- storeHPack rpli tid
                        hpackRecord <- getJust hpackId
                        getHPackCabalFile hpackRecord ts tmap cabalFile
                    Just (Entity _ item) ->
                        getHPackCabalFile item ts tmap cabalFile
    pure
        Package
            { packageTreeKey = P.TreeKey blobKey
            , packageTree = mtree
            , packageCabalEntry = packageEntry
            , packageIdent = ident
            }

getHPackBlobKey :: HPack -> ReaderT SqlBackend (RIO env) BlobKey
getHPackBlobKey hpackRecord = do
  let treeId = hPackTree hpackRecord
  hpackEntity <- loadHPackTreeEntity treeId
  getBlobKey (treeEntryBlob $ entityVal hpackEntity)

getHPackBlobKeyById :: HPackId -> ReaderT SqlBackend (RIO env) BlobKey
getHPackBlobKeyById hpackId = do
  hpackRecord <- getJust hpackId
  getHPackBlobKey hpackRecord


getHPackCabalFile ::
       (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
    => HPack
    -> Tree
    -> Map SafeFilePath P.TreeEntry
    -> SafeFilePath
    -> ReaderT SqlBackend (RIO env) (P.PackageCabal, P.Tree)
getHPackCabalFile hpackRecord ts tmap cabalFile = do
    cabalKey <- getBlobKey (hPackCabalBlob hpackRecord)
    hpackKey <- getHPackBlobKey hpackRecord
    hpackSoftwareVersion <- lift hpackVersion
    let fileType = treeCabalType ts
        cbTreeEntry = P.TreeEntry cabalKey fileType
        hpackTreeEntry = P.TreeEntry hpackKey fileType
        tree = P.TreeMap $ Map.insert cabalFile cbTreeEntry tmap
    return
        ( P.PCHpack $
          P.PHpack
              { P.phOriginal = hpackTreeEntry
              , P.phGenerated = cbTreeEntry
              , P.phVersion = hpackSoftwareVersion
              }
        , tree)

loadTreeByEnt :: Entity Tree -> ReaderT SqlBackend (RIO env) P.Tree
loadTreeByEnt (Entity tid _t) = do
  entries <- rawSql
    "SELECT file_path.path, blob.sha, blob.size, tree_entry.type\n\
    \FROM tree_entry, blob, file_path\n\
    \WHERE tree_entry.tree=?\n\
    \AND   tree_entry.blob=blob.id\n\
    \AND   tree_entry.path=file_path.id"
    [toPersistValue tid]
  pure $ P.TreeMap $ Map.fromList $ map
    (\(Single sfp, Single sha, Single size, Single ft) ->
         (sfp, P.TreeEntry (P.BlobKey sha size) ft))
    entries

storeHackageTree
  :: P.PackageName
  -> P.Version
  -> BlobId
  -> P.TreeKey
  -> ReaderT SqlBackend (RIO env) ()
storeHackageTree name version cabal treeKey' = do
  nameid <- getPackageNameId name
  versionid <- getVersionId version
  ment <- getTreeForKey treeKey'
  for_ ment $ \ent -> updateWhere
    [ HackageCabalName ==. nameid
    , HackageCabalVersion ==. versionid
    , HackageCabalCabal ==. cabal
    ]
    [HackageCabalTree =. Just (entityKey ent)]

loadHackageTreeKey
  :: P.PackageName
  -> P.Version
  -> SHA256
  -> ReaderT SqlBackend (RIO env) (Maybe TreeKey)
loadHackageTreeKey name ver sha = do
  res <- rawSql
    "SELECT treeblob.sha, treeblob.size\n\
    \FROM blob as treeblob, blob as cabalblob, package_name, version, hackage_cabal, tree\n\
    \WHERE package_name.name=?\n\
    \AND   version.version=?\n\
    \AND   cabalblob.sha=?\n\
    \AND   hackage_cabal.name=package_name.id\n\
    \AND   hackage_cabal.version=version.id\n\
    \AND   hackage_cabal.cabal=cabalblob.id\n\
    \AND   hackage_cabal.tree=tree.id\n\
    \AND   tree.key=treeblob.id"
    [ toPersistValue $ P.PackageNameP name
    , toPersistValue $ P.VersionP ver
    , toPersistValue sha
    ]
  case res of
    [] -> pure Nothing
    (Single treesha, Single size):_ ->
      pure $ Just $ P.TreeKey $ P.BlobKey treesha size

loadHackageTree
  :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
  => P.RawPackageLocationImmutable -- ^ for exceptions
  -> P.PackageName
  -> P.Version
  -> BlobId
  -> ReaderT SqlBackend (RIO env) (Maybe Package)
loadHackageTree rpli name ver bid = do
  nameid <- getPackageNameId name
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
        Just tid -> Just <$> loadPackageById rpli tid

storeArchiveCache
  :: Text -- ^ URL
  -> Text -- ^ subdir
  -> SHA256
  -> FileSize
  -> P.TreeKey
  -> ReaderT SqlBackend (RIO env) ()
storeArchiveCache url subdir sha size treeKey' = do
  now <- getCurrentTime
  ment <- getTreeForKey treeKey'
  for_ ment $ \ent -> insert_ ArchiveCache
    { archiveCacheTime = now
    , archiveCacheUrl = url
    , archiveCacheSubdir = subdir
    , archiveCacheSha = sha
    , archiveCacheSize = size
    , archiveCacheTree = entityKey ent
    }

loadArchiveCache
  :: Text -- ^ URL
  -> Text -- ^ subdir
  -> ReaderT SqlBackend (RIO env) [(SHA256, FileSize, TreeId)]
loadArchiveCache url subdir = map go <$> selectList
  [ ArchiveCacheUrl ==. url
  , ArchiveCacheSubdir ==. subdir
  ]
  [Desc ArchiveCacheTime]
  where
    go (Entity _ ac) = (archiveCacheSha ac, archiveCacheSize ac, archiveCacheTree ac)

storeRepoCache
  :: Repo
  -> Text -- ^ subdir
  -> TreeId
  -> ReaderT SqlBackend (RIO env) ()
storeRepoCache repo subdir tid = do
  now <- getCurrentTime
  insert_ RepoCache
    { repoCacheTime = now
    , repoCacheUrl = repoUrl repo
    , repoCacheType = repoType repo
    , repoCacheCommit = repoCommit repo
    , repoCacheSubdir = subdir
    , repoCacheTree = tid
    }

loadRepoCache
  :: Repo
  -> Text -- ^ subdir
  -> ReaderT SqlBackend (RIO env) (Maybe TreeId)
loadRepoCache repo subdir = fmap (repoCacheTree . entityVal) <$> selectFirst
  [ RepoCacheUrl ==. repoUrl repo
  , RepoCacheType ==. repoType repo
  , RepoCacheCommit ==. repoCommit repo
  , RepoCacheSubdir ==. subdir
  ]
  [Desc RepoCacheTime]

storePreferredVersion ::
     P.PackageName -> Text -> ReaderT SqlBackend (RIO env) ()
storePreferredVersion name p = do
  nameid <- getPackageNameId name
  ment <- getBy $ UniquePreferred nameid
  case ment of
    Nothing -> insert_ PreferredVersions
      { preferredVersionsName = nameid
      , preferredVersionsPreferred = p
      }
    Just (Entity pid _) -> update pid [PreferredVersionsPreferred =. p]

loadPreferredVersion ::
     P.PackageName -> ReaderT SqlBackend (RIO env) (Maybe Text)
loadPreferredVersion name = do
  nameid <- getPackageNameId name
  fmap (preferredVersionsPreferred . entityVal) <$> getBy (UniquePreferred nameid)

sinkHackagePackageNames
  :: (P.PackageName -> Bool)
  -> ConduitT P.PackageName Void (ReaderT SqlBackend (RIO env)) a
  -> ReaderT SqlBackend (RIO env) a
sinkHackagePackageNames predicate sink = do
  acqSrc <- selectSourceRes [] []
  with acqSrc $ \src -> runConduit
    $ src
   .| concatMapMC go
   .| sink
  where
    go (Entity nameid (PackageName (PackageNameP name)))
      | predicate name = do
          -- Make sure it's actually on Hackage. Would be much more
          -- efficient with some raw SQL and an inner join, but we
          -- don't have a Conduit version of rawSql.
          onHackage <- checkOnHackage nameid
          pure $ if onHackage then Just name else Nothing
      | otherwise = pure Nothing

    checkOnHackage nameid = do
      cnt <- count [HackageCabalName ==. nameid]
      pure $ cnt > 0

-- | Get the filename for the cabal file in the given directory.
--
-- If no .cabal file is present, or more than one is present, an exception is
-- thrown via 'throwM'.
--
-- If the directory contains a file named package.yaml, hpack is used to
-- generate a .cabal file from it.
findOrGenerateCabalFile
    :: forall env. (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
    => Path Abs Dir -- ^ package directory
    -> RIO env (P.PackageName, Path Abs File)
findOrGenerateCabalFile pkgDir = do
    hpack pkgDir
    files <- filter (flip hasExtension "cabal" . toFilePath) . snd
         <$> listDir pkgDir
    -- If there are multiple files, ignore files that start with
    -- ".". On unixlike environments these are hidden, and this
    -- character is not valid in package names. The main goal is
    -- to ignore emacs lock files - see
    -- https://github.com/commercialhaskell/stack/issues/1897.
    let isHidden ('.':_) = True
        isHidden _ = False
    case filter (not . isHidden . fromRelFile . filename) files of
        [] -> throwIO $ P.NoCabalFileFound pkgDir
        [x] -> maybe
          (throwIO $ P.InvalidCabalFilePath x)
          (\pn -> pure $ (pn, x)) $
            List.stripSuffix ".cabal" (toFilePath (filename x)) >>=
            P.parsePackageName
        _:_ -> throwIO $ P.MultipleCabalFilesFound pkgDir files
      where hasExtension fp x = FilePath.takeExtension fp == "." ++ x

-- | Similar to 'hpackToCabal' but doesn't require a new connection to database.
hpackToCabalS :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
              => P.RawPackageLocationImmutable -- ^ for exceptions
              -> P.Tree
              -> ReaderT SqlBackend (RIO env) (P.PackageName, ByteString)
hpackToCabalS rpli tree = do
  tmpDir <- lift $ do
              tdir <- getTempDir
              createTempDir tdir "hpack-pkg-dir"
  unpackTreeToDir rpli tmpDir tree
  (packageName, cfile) <- lift $ findOrGenerateCabalFile tmpDir
  !bs <- lift $ B.readFile (fromAbsFile cfile)
  lift $ removeDirRecur tmpDir
  return $ (packageName, bs)

hpackToCabal :: (HasPantryConfig env, HasLogFunc env, HasProcessContext env)
           => P.RawPackageLocationImmutable -- ^ for exceptions
           -> P.Tree
           -> RIO env (P.PackageName, ByteString)
hpackToCabal rpli tree = withSystemTempDirectory "hpack-pkg-dir" $ \tmpdir -> do
               tdir <- parseAbsDir tmpdir
               withStorage $ unpackTreeToDir rpli tdir tree
               (packageName, cfile) <- findOrGenerateCabalFile tdir
               bs <- B.readFile (fromAbsFile cfile)
               return (packageName, bs)

unpackTreeToDir
  :: (HasPantryConfig env, HasLogFunc env)
  => P.RawPackageLocationImmutable -- ^ for exceptions
  -> Path Abs Dir -- ^ dest dir, will be created if necessary
  -> P.Tree
  -> ReaderT SqlBackend (RIO env) ()
unpackTreeToDir rpli (toFilePath -> dir) (P.TreeMap m) = do
  for_  (Map.toList m) $ \(sfp, P.TreeEntry blobKey ft) -> do
    let dest = dir </> T.unpack (P.unSafeFilePath sfp)
    createDirectoryIfMissing True $ takeDirectory dest
    mbs <- loadBlob blobKey
    case mbs of
      Nothing -> do
        -- TODO when we have pantry wire stuff, try downloading
        throwIO $ P.TreeReferencesMissingBlob rpli sfp blobKey
      Just bs -> do
        B.writeFile dest bs
        case ft of
          FTNormal -> pure ()
          FTExecutable -> liftIO $ do
            perms <- getPermissions dest
            setPermissions dest $ setOwnerExecutable True perms

countHackageCabals :: ReaderT SqlBackend (RIO env) Int
countHackageCabals = do
  res <- rawSql
    "SELECT COUNT(*)\n\
    \FROM hackage_cabal"
    []
  case res of
    [] -> pure 0
    (Single n):_ ->
      pure n

getSnapshotCacheByHash
  :: SnapshotCacheHash
  -> ReaderT SqlBackend (RIO env) (Maybe SnapshotCacheId)
getSnapshotCacheByHash =
  fmap (fmap entityKey) . getBy . UniqueSnapshotCache . unSnapshotCacheHash

getSnapshotCacheId
  :: SnapshotCacheHash
  -> ReaderT SqlBackend (RIO env) SnapshotCacheId
getSnapshotCacheId =
  fmap (either entityKey id) . insertBy . SnapshotCache . unSnapshotCacheHash

getModuleNameId
  :: P.ModuleName
  -> ReaderT SqlBackend (RIO env) ModuleNameId
getModuleNameId =
  fmap (either entityKey id) . insertBy . ModuleName . P.ModuleNameP

storeSnapshotModuleCache
  :: SnapshotCacheId
  -> Map P.PackageName (Set P.ModuleName)
  -> ReaderT SqlBackend (RIO env) ()
storeSnapshotModuleCache cache packageModules =
  forM_ (Map.toList packageModules) $ \(pn, modules) -> do
    package <- getPackageNameId pn
    forM_ modules $ \m -> do
      moduleName <- getModuleNameId m
      insert_ PackageExposedModule
        { packageExposedModuleSnapshotCache = cache
        , packageExposedModulePackage = package
        , packageExposedModuleModule = moduleName
        }

loadExposedModulePackages
  :: SnapshotCacheId
  -> P.ModuleName
  -> ReaderT SqlBackend (RIO env) [P.PackageName]
loadExposedModulePackages cacheId mName =
  map go <$> rawSql
    "SELECT package_name.name\n\
    \FROM package_name, package_exposed_module, module_name\n\
    \WHERE module_name.name=?\n\
    \AND   package_exposed_module.snapshot_cache=?\n\
    \AND   module_name.id=package_exposed_module.module\n\
    \AND   package_name.id=package_exposed_module.package"
    [ toPersistValue (P.ModuleNameP mName)
    , toPersistValue cacheId
    ]
  where
    go (Single (P.PackageNameP m)) = m
