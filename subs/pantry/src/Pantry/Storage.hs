{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.Storage
  ( SqlBackend
  , initStorage
  , withStorage
  , storeBlob
  , clearHackageRevisions
  , storeHackageRevision
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
import qualified Crypto.Hash

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BlobTable sql=blob
    hash Text
    contents ByteString
    UniqueBlobHash hash
Name sql=package_name
    name Text
    UniquePackageName name
Version
    version Text
    UniqueVersion version
Hackage
    name NameId
    version VersionId
    revision Int
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
  => Text
  -> ReaderT SqlBackend (RIO env) NameId
getNameId = fmap (either entityKey id) . insertBy . Name

getVersionId
  :: (HasPantryConfig env, HasLogFunc env)
  => Text
  -> ReaderT SqlBackend (RIO env) VersionId
getVersionId = fmap (either entityKey id) . insertBy . Version

storeBlob
  :: (HasPantryConfig env, HasLogFunc env)
  => ByteString
  -> ReaderT SqlBackend (RIO env) (BlobTableId, BlobKey)
storeBlob bs = do
  let h = Crypto.Hash.hash bs
      txt = fromString $ show h
  keys <- selectKeysList [BlobTableHash ==. txt] []
  key <-
    case keys of
      [] -> insert BlobTable
              { blobTableHash = txt
              , blobTableContents = bs
              }
      key:rest -> assert (null rest) (pure key)
  pure (key, BlobKey h)

clearHackageRevisions
  :: (HasPantryConfig env, HasLogFunc env)
  => ReaderT SqlBackend (RIO env) ()
clearHackageRevisions = deleteWhere ([] :: [Filter Hackage])

storeHackageRevision
  :: (HasPantryConfig env, HasLogFunc env)
  => Text -- ^ name
  -> Text -- ^ version
  -> BlobTableId
  -> ReaderT SqlBackend (RIO env) ()
storeHackageRevision name version key = do
  nameid <- getNameId name
  versionid <- getVersionId version
  rev <- count [HackageName ==. nameid, HackageVersion ==. versionid]
  insert_ Hackage
    { hackageName = nameid
    , hackageVersion = versionid
    , hackageRevision = rev
    , hackageCabal = key
    }
