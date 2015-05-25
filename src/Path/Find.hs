{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Finding files.

module Path.Find
  (findFileUp
  ,findDirUp
  ,findFiles)
  where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Path as FL
import System.Directory (getDirectoryContents,doesDirectoryExist)

-- | Find the location of a file matching the given predicate.
findFileUp :: MonadIO m
           => Path Abs Dir                -- ^ Start here.
           -> (Path Abs File -> Bool)     -- ^ Predicate to match the file.
           -> Maybe (Path Abs Dir)        -- ^ Do not ascend above this directory.
           -> m (Maybe (Path Abs File))  -- ^ Absolute file path.
findFileUp s p d =
  liftIO (findPathUp parseAbsFile s p d)

-- | Find the location of a directory matching the given predicate.
findDirUp :: Path Abs Dir                -- ^ Start here.
          -> (Path Abs Dir -> Bool)      -- ^ Predicate to match the directory.
          -> Maybe (Path Abs Dir)        -- ^ Do not ascend above this directory.
          -> IO (Maybe (Path Abs Dir))   -- ^ Absolute directory path.
findDirUp = findPathUp parseAbsDir

-- | Find the location of a path matching the given predicate.
findPathUp :: (FilePath -> Maybe (Path Abs t)) -- ^ Parse FilePath into absolute Loc.
           -> Path Abs Dir                     -- ^ Start here.
           -> (Path Abs t -> Bool)             -- ^ Predicate to match the path.
           -> Maybe (Path Abs Dir)             -- ^ Do not ascend above this directory.
           -> IO (Maybe (Path Abs t))          -- ^ Absolute path.
findPathUp parse dir p upperBound =
  do files <-
       fmap (map (FL.toFilePath dir ++))
            (getDirectoryContents (FL.toFilePath dir))
     case find p (mapMaybe parse files) of
       Just path -> return (Just path)
       Nothing ->
         if Just dir ==
            upperBound
            then return Nothing
            else if FL.parent dir == dir
                    then return Nothing
                    else findPathUp parse
                                    (FL.parent dir)
                                    p
                                    upperBound

-- | Find files matching predicate below a root directory.
findFiles :: Path Abs Dir            -- ^ Root directory to begin with.
          -> (Path Abs File -> Bool) -- ^ Predicate to match files.
          -> (Path Abs Dir -> Bool)  -- ^ Predicate for which directories to traverse.
          -> IO [Path Abs File]      -- ^ List of matching files.
findFiles dir p traverse =
  do entries <-
       fmap (map (FL.toFilePath dir ++))
            (getDirectoryContents (FL.toFilePath dir))
     subResults <-
       forM (mapMaybe parseAbsDir entries)
            (\entry ->
               do isDir <-
                    doesDirectoryExist (FL.toFilePath entry)
                  if isDir && traverse entry
                     then findFiles entry p traverse
                     else return [])
     return (concat (filter p (mapMaybe parseAbsFile entries) :
                     subResults))
