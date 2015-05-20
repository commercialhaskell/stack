{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Finding files.

module Path.Find
  (findFileUp
  ,findDirUp
  ,findFiles
  ,resolveDir
  ,resolveFile)
  where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Data.Typeable (Typeable)
import Path as FL
import System.Directory (getDirectoryContents,doesDirectoryExist,doesFileExist,canonicalizePath)
import qualified System.FilePath as FP

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

data ResolveException
    = ResolveDirFailed (Path Abs Dir) FilePath FilePath
    | ResolveFileFailed (Path Abs Dir) FilePath FilePath
    deriving (Show, Typeable)
instance Exception ResolveException

-- | Appends a stringly-typed relative path to an absolute path, and then
-- canonicalizes it.
resolveDir :: (MonadIO m, MonadThrow m) => Path Abs Dir -> FilePath -> m (Path Abs Dir)
resolveDir x y = do
    let fp = toFilePath x FP.</> y
    exists <- liftIO $ doesDirectoryExist fp
    if exists
        then do
            dir <- liftIO $ canonicalizePath fp
            parseAbsDir dir
        else throwM $ ResolveDirFailed x y fp

-- | Appends a stringly-typed relative path to an absolute path, and then
-- canonicalizes it.
resolveFile :: (MonadIO m, MonadThrow m) => Path Abs Dir -> FilePath -> m (Path Abs File)
resolveFile x y = do
    let fp = toFilePath x FP.</> y
    exists <- liftIO $ doesFileExist fp
    if exists
        then do
            file <- liftIO $ canonicalizePath fp
            parseAbsFile file
        else throwM $ ResolveFileFailed x y fp
