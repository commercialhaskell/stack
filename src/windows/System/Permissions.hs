-- | The module of this name differs as between Windows and non-Windows builds.
-- This is the Windows version.
module System.Permissions
  ( osIsMacOS
  , osIsWindows
  , setFileExecutable
  , setScriptPerms
  ) where

-- | False if using Windows.
osIsMacOS :: Bool
osIsMacOS = False

-- | True if using Windows.
osIsWindows :: Bool
osIsWindows = True

setFileExecutable :: Monad m => FilePath -> m ()
setFileExecutable _ = pure ()

setScriptPerms :: Monad m => FilePath -> m ()
setScriptPerms _ = pure ()
