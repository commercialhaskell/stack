{-# LANGUAGE NoImplicitPrelude #-}

-- | The module of this name differs as between Windows and non-Windows builds.
-- This is the non-Windows version.
module System.Permissions
  ( setScriptPerms
  , osIsWindows
  , setFileExecutable
  ) where

import           RIO
import qualified System.Posix.Files as Posix

-- | True if using Windows OS.
osIsWindows :: Bool
osIsWindows = False

setScriptPerms :: MonadIO m => FilePath -> m ()
setScriptPerms fp = do
  liftIO $ Posix.setFileMode fp $
    Posix.ownerReadMode `Posix.unionFileModes`
    Posix.ownerWriteMode `Posix.unionFileModes`
    Posix.groupReadMode `Posix.unionFileModes`
    Posix.otherReadMode

setFileExecutable :: MonadIO m => FilePath -> m ()
setFileExecutable fp = liftIO $ Posix.setFileMode fp 0o755
