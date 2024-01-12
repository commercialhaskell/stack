{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | The module of this name differs as between Windows and non-Windows builds.
-- This is the Windows version. Non-Windows builds rely on the unix package,
-- which exposes a module of the same name.

module System.Posix.User
  ( getEffectiveUserID
  , getEffectiveGroupID
  , getGroups
  , getUserEntryForName
  , homeDirectory
  , setGroupID
  , setUserID
  ) where

import           System.IO.Error ( illegalOperationErrorType, mkIOError )
import           System.PosixCompat.Types ( GroupID, UserID )

unsupported :: String -> IO a
unsupported f = ioError $ mkIOError illegalOperationErrorType x Nothing Nothing
 where
  x = "System.Posix.User." ++ f ++ ": not supported on Windows."

getEffectiveUserID :: IO UserID
getEffectiveUserID = unsupported "getEffectiveUserID"

getEffectiveGroupID :: IO GroupID
getEffectiveGroupID = unsupported "getEffectiveGroupID"

getGroups :: IO [GroupID]
getGroups = return []

getUserEntryForName :: String -> IO UserEntry
getUserEntryForName _ = unsupported "getUserEntryForName"

setGroupID :: GroupID -> IO ()
setGroupID _ = return ()

setUserID :: UserID -> IO ()
setUserID _ = return ()

data UserEntry = UserEntry
    { userName      :: String
    , userPassword  :: String
    , userID        :: UserID
    , userGroupID   :: GroupID
    , userGecos     :: String
    , homeDirectory :: String
    , userShell     :: String
    } deriving (Eq, Read, Show)

homeDirectory :: UserEntry -> String
homeDirectory ue = ue.homeDirectory
