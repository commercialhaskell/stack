{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.FileDigestCache
  ( FileDigestCache
  , newFileDigestCache
  , readFileDigest
  ) where

import qualified Data.Map.Strict as Map
import           Stack.Prelude
import qualified Pantry.SHA256 as SHA256

-- | Type synonym representing caches of digests of files.
type FileDigestCache = IORef (Map FilePath SHA256)

newFileDigestCache :: MonadIO m => m FileDigestCache
newFileDigestCache = newIORef Map.empty

readFileDigest :: MonadIO m => FileDigestCache -> FilePath -> m SHA256
readFileDigest cache filePath = do
  digests <- readIORef cache
  case Map.lookup filePath digests of
    Just digest -> pure digest
    Nothing -> do
      sha256 <- SHA256.hashFile filePath
      writeIORef cache $ Map.insert filePath sha256 digests
      pure sha256
