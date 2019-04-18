{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.SQLite
  ( Storage (..)
  , initStorage
  ) where

import RIO hiding (FilePath)
import Database.Persist.Sqlite
import RIO.Orphans ()
import Path (Path, Abs, File, toFilePath, parent)
import Path.IO (ensureDir)
import Pantry.Types (PantryException (MigrationFailure), Storage (..))
import System.FileLock (withFileLock, withTryFileLock, SharedExclusive (..))

initStorage
  :: HasLogFunc env
  => Text
  -> Migration
  -> Path Abs File -- ^ storage file
  -> (Storage -> RIO env a)
  -> RIO env a
initStorage description migration fp inner = do
  ensureDir $ parent fp

  migrates <- withWriteLock fp $ wrapMigrationFailure $
    withSqliteConnInfo (sqinfo True) $ runReaderT $
    runMigrationSilent migration
  forM_ migrates $ \mig -> logDebug $ "Migration executed: " <> display mig

  withSqlitePoolInfo (sqinfo False) 1 $ \pool -> inner $ Storage
    -- NOTE: Currently, we take a write lock on every action. This is
    -- a bit heavyweight, but it avoids the SQLITE_BUSY errors
    -- reported in
    -- <https://github.com/commercialhaskell/stack/issues/4471>
    -- completely. We can investigate more elegant solutions in the
    -- future, such as separate read and write actions or introducing
    -- smarter retry logic.
    { withStorage_ = withWriteLock fp . flip runSqlPool pool
    , withWriteLock_ = id
    }
  where
    wrapMigrationFailure = handleAny (throwIO . MigrationFailure description fp)

    sqinfo isMigration
           = set extraPragmas ["PRAGMA busy_timeout=2000;"]

           -- When doing a migration, we want to disable foreign key
           -- checking, since the order in which tables are created by
           -- the migration scripts may not respect foreign keys. The
           -- rest of the time: enforce those foreign keys.
           $ set fkEnabled (not isMigration)

           $ mkSqliteConnectionInfo (fromString $ toFilePath fp)

-- | Ensure that only one process is trying to write to the database
-- at a time. See
-- https://github.com/commercialhaskell/stack/issues/4471 and comments
-- above.
withWriteLock
  :: HasLogFunc env
  => Path Abs File -- ^ SQLite database file
  -> RIO env a
  -> RIO env a
withWriteLock dbFile inner = do
  let lockFile = toFilePath dbFile ++ ".pantry-write-lock"
  withRunInIO $ \run -> do
    mres <- withTryFileLock lockFile Exclusive $ const $ run inner
    case mres of
      Just res -> pure res
      Nothing -> do
        run $ logInfo "Unable to get a write lock on the Pantry database, waiting..."
        shouldStopComplainingVar <- newTVarIO False
        let complainer = fix $ \loop -> do
              delay <- registerDelay $ 60 * 1000 * 1000 -- 1 minute
              shouldComplain <-
                atomically $
                  -- Delay has triggered, time to complain again
                  (readTVar delay >>= checkSTM >> pure True) <|>
                  -- Time to stop complaining, ignore that delay immediately
                  (readTVar shouldStopComplainingVar >>= checkSTM >> pure False)
              when shouldComplain $ do
                run $ logWarn "Still waiting on the Pantry database write lock..."
                loop
            stopComplaining = atomically $ writeTVar shouldStopComplainingVar True
            worker = withFileLock lockFile Exclusive $ const $ do
              run $ logInfo "Acquired the Pantry database write lock"
              stopComplaining
              run inner
        runConcurrently $ Concurrently complainer
                       *> Concurrently (worker `finally` stopComplaining)
