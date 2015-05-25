{-# LANGUAGE DeriveDataTypeable #-}

-- | IO actions that might be put in a package at some point.

module Path.IO
  (getWorkingDir
  ,resolveDir
  ,resolveFile
  ,resolveDirMaybe
  ,resolveFileMaybe
  ,ResolveException(..))
  where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Typeable
import           Path
import           System.Directory
import qualified System.FilePath as FP

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
