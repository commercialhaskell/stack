{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.DockerEntrypoint
  ( DockerEntrypoint (..)
  , DockerUser (..)
  ) where

import           Stack.Prelude
import           System.PosixCompat.Types ( FileMode, GroupID, UserID )

-- | Data passed into Docker container for the Docker entrypoint's use
newtype DockerEntrypoint = DockerEntrypoint
  { deUser :: Maybe DockerUser
    -- ^ UID/GID/etc of host user, if we wish to perform UID/GID switch in
    -- container
  }
  deriving (Read, Show)

-- | Docker host user info
data DockerUser = DockerUser
  { duUid :: UserID -- ^ uid
  , duGid :: GroupID -- ^ gid
  , duGroups :: [GroupID] -- ^ Supplemental groups
  , duUmask :: FileMode -- ^ File creation mask }
  }
  deriving (Read, Show)
