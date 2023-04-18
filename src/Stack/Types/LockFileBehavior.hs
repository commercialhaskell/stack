{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.LockFileBehavior
  ( LockFileBehavior (..)
  , readLockFileBehavior
  ) where

import qualified Data.Map as Map
import           Options.Applicative ( ReadM )
import qualified Options.Applicative.Types as OA
import qualified RIO.List as List
import           Stack.Prelude

-- | How to interact with lock files
data LockFileBehavior
  = LFBReadWrite
    -- ^ Read and write lock files
  | LFBReadOnly
    -- ^ Read lock files, but do not write them
  | LFBIgnore
    -- ^ Entirely ignore lock files
  | LFBErrorOnWrite
    -- ^ Error out on trying to write a lock file. This can be used to
    -- ensure that lock files in a repository already ensure
    -- reproducible builds.
  deriving (Bounded, Enum, Show)

-- | Parser for 'LockFileBehavior'
readLockFileBehavior :: ReadM LockFileBehavior
readLockFileBehavior = do
  s <- OA.readerAsk
  case Map.lookup s m of
    Just x -> pure x
    Nothing -> OA.readerError $ "Invalid lock file behavior, valid options: " ++
                                List.intercalate ", " (Map.keys m)
 where
  m = Map.fromList $ map (\x -> (render x, x)) [minBound..maxBound]
  render LFBReadWrite = "read-write"
  render LFBReadOnly = "read-only"
  render LFBIgnore = "ignore"
  render LFBErrorOnWrite = "error-on-write"
