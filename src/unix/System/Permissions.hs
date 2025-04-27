{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : System.Permissions
License     : BSD-3-Clause

The module of this name differs as between Windows and non-Windows builds. This
is the non-Windows version.
-}

module System.Permissions
  ( osIsMacOS
  , osIsWindows
  , setFileExecutable
  , setScriptPerms
  ) where

import           RIO
import qualified System.Posix.Files as Posix
import           System.Info ( os )

-- | True if using macOS.
osIsMacOS :: Bool
osIsMacOS = os == "darwin"

-- | False if not using Windows.
osIsWindows :: Bool
osIsWindows = False

setFileExecutable :: MonadIO m => FilePath -> m ()
setFileExecutable fp = liftIO $ Posix.setFileMode fp 0o755

setScriptPerms :: MonadIO m => FilePath -> m ()
setScriptPerms fp =
  liftIO $ Posix.setFileMode fp $
    Posix.ownerReadMode `Posix.unionFileModes`
    Posix.ownerWriteMode `Posix.unionFileModes`
    Posix.groupReadMode `Posix.unionFileModes`
    Posix.otherReadMode
