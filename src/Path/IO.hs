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
  ,removeFileIfExists
  ,removeTree
  ,removeTreeIfExists
  ,fileExists
  ,renameFileIfExists
  ,renameDirIfExists
  ,moveFileIfExists
  ,moveDirIfExists
  ,dirExists
  ,copyDirectoryRecursive
  ,createTree)
  where

import           Control.Exception hiding (catch)
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
    deriving Typeable
instance Exception ResolveException

instance Show ResolveException where
    show (ResolveDirFailed _ _ z) = "Could not resolve directory " ++ z
    show (ResolveFileFailed _ _ z) = "Could not resolve file " ++ z

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
    liftIO (catch
                (removeFile
                     (toFilePath fp))
                (\e ->
                      if isDoesNotExistError e
                          then return ()
                          else throwIO e))

-- | Move the given file. Optimistically assumes it exists. If it
-- doesn't, doesn't complain.
renameFileIfExists :: MonadIO m => Path b File -> Path b File -> m ()
renameFileIfExists from to =
    liftIO
        (catch
             (renameFile (toFilePath from)
                         (toFilePath to))
             (\e ->
                   if isDoesNotExistError e
                       then return ()
                       else throwIO e))

-- | Rename the directory. Optimistically assumes it exists. If it
-- doesn't, doesn't complain.
renameDirIfExists :: MonadIO m => Path b Dir -> Path b Dir -> m ()
renameDirIfExists from to =
    liftIO
        (catch
             (renameDirectory (toFilePath from)
                              (toFilePath to))
             (\e ->
                   if isDoesNotExistError e
                       then return ()
                       else throwIO e))

-- | Make a directory tree, creating parents if needed.
createTree :: MonadIO m => Path b Dir -> m ()
createTree = liftIO . createDirectoryIfMissing True . toFilePath

-- | Move the given file. Optimistically assumes it exists. If it
-- doesn't, doesn't complain.
moveFileIfExists :: MonadIO m => Path b File -> Path b Dir -> m ()
moveFileIfExists from to =
    liftIO
        (catch
             (renameFile (toFilePath from)
                         (toFilePath (to </> filename from)))
             (\e ->
                   if isDoesNotExistError e
                       then return ()
                       else throwIO e))

-- | Move the given dir. Optimistically assumes it exists. If it
-- doesn't, doesn't complain.
moveDirIfExists :: MonadIO m => Path b Dir -> Path b Dir -> m ()
moveDirIfExists from to =
    liftIO
        (catch
             (renameDirectory
                  (toFilePath from)
                  (toFilePath (to </> dirname from)))
             (\e ->
                   if isDoesNotExistError e
                       then return ()
                       else throwIO e))

-- | Remove the given tree. Bails out if the directory doesn't exist.
removeTree :: MonadIO m => Path b Dir -> m ()
removeTree =
    liftIO . removeDirectoryRecursive . toFilePath

-- | Remove tree, don't complain about non-existent directories.
removeTreeIfExists :: MonadIO m => Path b Dir -> m ()
removeTreeIfExists fp = do
    liftIO (catch (removeTree fp)
                  (\e -> if isDoesNotExistError e
                            then return ()
                            else throwIO e))

-- | Does the given file exist?
fileExists :: MonadIO m => Path b File -> m Bool
fileExists =
    liftIO . doesFileExist . toFilePath

-- | Does the given directory exist?
dirExists :: MonadIO m => Path b Dir -> m Bool
dirExists =
    liftIO . doesDirectoryExist . toFilePath

-- | Copy a directory recursively.  This just uses 'copyFile', so it is not smart about symbolic
-- links or other special files.
copyDirectoryRecursive :: (MonadIO m,MonadThrow m)
                       => Path Abs Dir -- ^ Source directory
                       -> Path Abs Dir -- ^ Destination directory
                       -> m ()
copyDirectoryRecursive srcDir destDir =
    do liftIO (createDirectoryIfMissing False (toFilePath destDir))
       (srcSubDirs,srcFiles) <- listDirectory srcDir
       forM_ srcFiles
             (\srcFile ->
                case stripDir srcDir srcFile of
                  Nothing -> return ()
                  Just relFile -> liftIO (copyFile (toFilePath srcFile)
                                                   (toFilePath (destDir </> relFile))))
       forM_ srcSubDirs
             (\srcSubDir ->
                case stripDir srcDir srcSubDir of
                  Nothing -> return ()
                  Just relSubDir -> copyDirectoryRecursive srcSubDir (destDir </> relSubDir))
