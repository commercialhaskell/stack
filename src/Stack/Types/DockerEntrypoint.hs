{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoFieldSelectors  #-}

{-|
Module      : Stack.Types.DockerEntrypoint
License     : BSD-3-Clause
-}

module Stack.Types.DockerEntrypoint
  ( DockerEntrypoint (..)
  , DockerUser (..)
  ) where

import           Stack.Prelude
import           System.PosixCompat.Types ( FileMode, GroupID, UserID )

-- | Data passed into Docker container for the Docker entrypoint's use
newtype DockerEntrypoint = DockerEntrypoint
  { user :: Maybe DockerUser
    -- ^ UID/GID/etc of host user, if we wish to perform UID/GID switch in
    -- container
  }
  deriving (Read, Show)

-- | Docker host user info
data DockerUser = DockerUser
  { uid :: UserID -- ^ uid
  , gid :: GroupID -- ^ gid
  , groups :: [GroupID] -- ^ Supplemental groups
  , umask :: FileMode -- ^ File creation mask }
  }
  deriving (Read, Show)
