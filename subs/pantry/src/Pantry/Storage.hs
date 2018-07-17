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
  , loadHackageCabalFile
    -- avoid warnings
  , BlobTableId
  , HackageId
  ) where

import RIO
import Pantry.Types
import Database.Persist
import Database.Persist.Sqlite -- FIXME allow PostgreSQL too
import Database.Persist.TH
import RIO.Orphans ()
import Pantry.StaticSHA256
import qualified RIO.Map as Map

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BlobTable sql=blob
    hash BlobKey
    contents ByteString
    UniqueBlobHash hash
Name sql=package_name
    name PackageNameP
    UniquePackageName name
VersionTable sql=version
    version VersionP
    UniqueVersion version
Hackage
    name NameId
    version VersionTableId
    revision Word
    cabal BlobTableId
    UniqueHackage name version revision
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
              , blobTableContents = bs
              }
      key:rest -> assert (null rest) (pure key)
  pure (key, blobKey)

clearHackageRevisions
  :: (HasPantryConfig env, HasLogFunc env)
  => ReaderT SqlBackend (RIO env) ()
clearHackageRevisions = deleteWhere ([] :: [Filter Hackage])

storeHackageRevision
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> Version
  -> BlobTableId
  -> ReaderT SqlBackend (RIO env) ()
storeHackageRevision name version key = do
  nameid <- getNameId name
  versionid <- getVersionId version
  rev <- count [HackageName ==. nameid, HackageVersion ==. versionid]
  insert_ Hackage
    { hackageName = nameid
    , hackageVersion = versionid
    , hackageRevision = fromIntegral rev
    , hackageCabal = key
    }

loadHackagePackageVersions
  :: (HasPantryConfig env, HasLogFunc env)
  => PackageName
  -> ReaderT SqlBackend (RIO env) (Map Version CabalHash)
loadHackagePackageVersions name = do
  nameid <- getNameId name
  -- would be better with esequeleto
  (Map.fromList . map go) <$> rawSql
    "SELECT version.version, blob.hash\n\
    \FROM hackage, version, blob\n\
    \WHERE hackage.name=?\n\
    \AND   hackage.version=version.id\n\
    \AND   hackage.cabal=blob.id"
    [toPersistValue nameid]
  where
    go (Single (VersionP version), Single key) = (version, CabalHash key)

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
      [ HackageName ==. nameid
      , HackageVersion ==. versionid
      ]
      [Desc HackageRevision] >>= withHackEnt
    CFIRevision rev ->
      getBy (UniqueHackage nameid versionid rev) >>= withHackEnt
    CFIHash msize (CabalHash (BlobKey -> blobKey)) ->
      fmap (blobTableContents . entityVal) <$> getBy (UniqueBlobHash blobKey)
  where
    withHackEnt = traverse $ \(Entity _ h) -> do
      Just blob <- get $ hackageCabal h
      pure $ blobTableContents blob
