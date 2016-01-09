{-# LANGUAGE DataKinds #-}

-- | Finding files.

module Path.Find
  (findFileUp
  ,findDirUp
  ,findFiles
  ,findInParents)
  where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import System.IO.Error (isPermissionError)
import Data.List
import Path
import Path.IO hiding (findFiles)

-- | Find the location of a file matching the given predicate.
findFileUp :: (MonadIO m,MonadThrow m)
           => Path Abs Dir                -- ^ Start here.
           -> (Path Abs File -> Bool)     -- ^ Predicate to match the file.
           -> Maybe (Path Abs Dir)        -- ^ Do not ascend above this directory.
           -> m (Maybe (Path Abs File))  -- ^ Absolute file path.
findFileUp = findPathUp snd

-- | Find the location of a directory matching the given predicate.
findDirUp :: (MonadIO m,MonadThrow m)
          => Path Abs Dir                -- ^ Start here.
          -> (Path Abs Dir -> Bool)      -- ^ Predicate to match the directory.
          -> Maybe (Path Abs Dir)        -- ^ Do not ascend above this directory.
          -> m (Maybe (Path Abs Dir))   -- ^ Absolute directory path.
findDirUp = findPathUp fst

-- | Find the location of a path matching the given predicate.
findPathUp :: (MonadIO m,MonadThrow m)
           => (([Path Abs Dir],[Path Abs File]) -> [Path Abs t])
              -- ^ Choose path type from pair.
           -> Path Abs Dir                     -- ^ Start here.
           -> (Path Abs t -> Bool)             -- ^ Predicate to match the path.
           -> Maybe (Path Abs Dir)             -- ^ Do not ascend above this directory.
           -> m (Maybe (Path Abs t))           -- ^ Absolute path.
findPathUp pathType dir p upperBound =
  do entries <- listDir dir
     case find p (pathType entries) of
       Just path -> return (Just path)
       Nothing | Just dir == upperBound -> return Nothing
               | parent dir == dir -> return Nothing
               | otherwise -> findPathUp pathType (parent dir) p upperBound

-- | Find files matching predicate below a root directory.
findFiles :: Path Abs Dir            -- ^ Root directory to begin with.
          -> (Path Abs File -> Bool) -- ^ Predicate to match files.
          -> (Path Abs Dir -> Bool)  -- ^ Predicate for which directories to traverse.
          -> IO [Path Abs File]      -- ^ List of matching files.
findFiles dir p traversep =
  do (dirs,files) <- catchJust (\ e -> if isPermissionError e
                                         then Just ()
                                         else Nothing)
                               (listDir dir)
                               (\ _ -> return ([], []))
     subResults <-
       forM dirs
            (\entry ->
               if traversep entry
                  then findFiles entry p traversep
                  else return [])
     return (concat (filter p files : subResults))

-- | @findInParents f path@ applies @f@ to @path@ and its 'parent's until
-- it finds a 'Just' or reaches the root directory.
findInParents :: MonadIO m => (Path Abs Dir -> m (Maybe a)) -> Path Abs Dir -> m (Maybe a)
findInParents f path = do
    mres <- f path
    case mres of
        Just res -> return (Just res)
        Nothing -> do
            let next = parent path
            if next == path
                then return Nothing
                else findInParents f next
