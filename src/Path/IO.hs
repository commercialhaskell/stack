{-# LANGUAGE DeriveDataTypeable #-}

-- | IO actions that might be put in a package at some point.

module Path.IO
  (getWorkingDir
  ,listDirectory
  ,resolveDir
  ,resolveFile
  ,resolveDirMaybe
  ,resolveFileMaybe
  ,ResolveException(..)
  ,removeFileIfExists)
  where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Either
import           Data.Maybe
import           Data.Typeable
import           Path
import           System.Directory
import qualified System.FilePath as FP
import           System.IO.Error

data ResolveException
    = ResolveDirFailed (Path Abs Dir) FilePath FilePath
    | ResolveFileFailed (Path Abs Dir) FilePath FilePath
    deriving (Show, Typeable)
instance Exception ResolveException

-- | Get the current working directory.
getWorkingDir :: (MonadIO m) => m (Path Abs Dir)
getWorkingDir = liftIO (canonicalizePath "." >>= parseAbsDir)

-- | Appends a stringly-typed relative path to an absolute path, and then
-- canonicalizes it.
resolveDir :: (MonadIO m, MonadThrow m) => Path Abs Dir -> FilePath -> m (Path Abs Dir)
resolveDir x y =
  do result <- resolveDirMaybe x y
     case result of
       Nothing ->
         throwM $ ResolveDirFailed x y fp
         where fp = toFilePath x FP.</> y
       Just fp -> return fp

-- | Appends a stringly-typed relative path to an absolute path, and then
-- canonicalizes it.
resolveFile :: (MonadIO m, MonadThrow m) => Path Abs Dir -> FilePath -> m (Path Abs File)
resolveFile x y =
  do result <- resolveFileMaybe x y
     case result of
       Nothing ->
         throwM $
         ResolveFileFailed x y fp
         where fp = toFilePath x FP.</> y
       Just fp -> return fp

-- | Appends a stringly-typed relative path to an absolute path, and then
-- canonicalizes it. If the path doesn't exist (and therefore cannot
-- be canonicalized, 'Nothing' is returned).
resolveDirMaybe :: (MonadIO m,MonadThrow m)
                => Path Abs Dir -> FilePath -> m (Maybe (Path Abs Dir))
resolveDirMaybe x y = do
    let fp = toFilePath x FP.</> y
    exists <- liftIO $ doesDirectoryExist fp
    if exists
        then do
            dir <- liftIO $ canonicalizePath fp
            liftM Just (parseAbsDir dir)
        else return Nothing

-- | Appends a stringly-typed relative path to an absolute path, and then
-- canonicalizes it. If the path doesn't exist (and therefore cannot
-- be canonicalized, 'Nothing' is returned).
resolveFileMaybe :: (MonadIO m,MonadThrow m)
                 => Path Abs Dir -> FilePath -> m (Maybe (Path Abs File))
resolveFileMaybe x y = do
    let fp = toFilePath x FP.</> y
    exists <- liftIO $ doesFileExist fp
    if exists
        then do
            file <- liftIO $ canonicalizePath fp
            liftM Just (parseAbsFile file)
        else return Nothing

-- | List objects in a directory, excluding "@.@" and "@..@".  Entries are not sorted.
listDirectory :: (MonadIO m,MonadThrow m) => Path Abs Dir -> m ([Path Abs Dir],[Path Abs File])
listDirectory dir =
  do entriesFP <- liftIO (getDirectoryContents dirFP)
     maybeEntries <-
       forM (map (dirFP ++) entriesFP)
            (\entryFP ->
               do isDir <- liftIO (doesDirectoryExist entryFP)
                  if isDir
                     then case parseAbsDir entryFP of
                            Nothing -> return Nothing
                            Just entryDir ->
                              if dir `isParentOf` entryDir
                                 then return (Just (Left entryDir))
                                 else return Nothing
                     else case parseAbsFile entryFP of
                            Nothing -> return Nothing
                            Just entryFile -> return (Just (Right entryFile)))
     let entries = catMaybes maybeEntries
     return (lefts entries,rights entries)
  where dirFP = toFilePath dir

-- | Remove the given file. Optimistically assumes it exists. If it
-- doesn't, doesn't complain.
removeFileIfExists :: MonadIO m => Path b File -> m ()
removeFileIfExists fp =
    liftIO (Control.Exception.catch
                (removeFile
                     (toFilePath fp))
                (\e ->
                      if isDoesNotExistError e
                          then return ()
                          else throwIO e))
