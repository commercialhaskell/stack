{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.SQLite
  ( P.Storage
  , initStorage
  , withStorage
  ) where

import RIO hiding (FilePath)
import qualified Pantry.Types as P
import Database.Persist.Sqlite
import RIO.Orphans ()
import Path (Path, Abs, File, toFilePath, parent)
import Path.IO (ensureDir)
import Data.Pool (destroyAllResources)
import Pantry.Types (PantryException (MigrationFailure))

initStorage
  :: HasLogFunc env
  => Text
  -> Migration
  -> Path Abs File -- ^ storage file
  -> (P.Storage -> RIO env a)
  -> RIO env a
initStorage description migration fp inner = do
  ensureDir $ parent fp
  bracket
    (createSqlitePoolFromInfo (sqinfo False) 1)
    (liftIO . destroyAllResources) $ \pool -> do
    migrates <- wrapMigrationFailure $ runSqlPool (runMigrationSilent migration) pool
    forM_ migrates $ \mig -> logDebug $ "Migration executed: " <> display mig
  bracket
    (createSqlitePoolFromInfo (sqinfo True) 1)
    (liftIO . destroyAllResources) $ \pool -> inner (P.Storage pool)
  where
    wrapMigrationFailure = handle (throwIO . MigrationFailure description fp)
    sqinfo fk = set extraPragmas ["PRAGMA busy_timeout=2000;"]
           $ set fkEnabled fk
           $ mkSqliteConnectionInfo (fromString $ toFilePath fp)

withStorage
  :: HasLogFunc env
  => ReaderT SqlBackend (RIO env) a
  -> P.Storage
  -> RIO env a
withStorage action (P.Storage pool) =
  runSqlPool action pool
