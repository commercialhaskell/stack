{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}

-- | IO actions that might be put in a package at some point.

module Path.IO
  (getWorkingDir
  ,parseRelAsAbsDir
  ,parseRelAsAbsFile
  ,listDirectory
  ,resolveDir
  ,resolveFile
  ,resolveDirMaybe
  ,resolveFileMaybe
  ,ResolveException(..)
  ,removeFile
  ,removeFileIfExists
  ,removeTree
  ,removeTreeIfExists
  ,renameFile
  ,renameFileIfExists
  ,renameDir
  ,renameDirIfExists
  ,moveFile
  ,moveFileIfExists
  ,moveDir
  ,moveDirIfExists
  ,fileExists
  ,dirExists
  ,copyFile
  ,copyFileIfExists
  ,copyDirectoryRecursive
  ,createTree
  ,dropRoot
  ,parseCollapsedAbsFile
  ,parseCollapsedAbsDir)
  where

import           Control.Exception hiding (catch)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Either
import           Data.Maybe
import           Data.Typeable
import           Path
import           Path.Internal (Path(..))
import qualified System.Directory as D
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
getWorkingDir = liftIO (D.canonicalizePath "." >>= parseAbsDir)

-- | Parse a directory path. If it's relative, then the absolute version
-- is yielded, based off the working directory.
parseRelAsAbsDir :: (MonadThrow m, MonadIO m) => FilePath -> m (Path Abs Dir)
parseRelAsAbsDir fp = parseAbsDir =<< liftIO (D.canonicalizePath fp)

-- | Parse a file path. If it's relative, then the absolute version is
-- yielded, based off the working directory.
parseRelAsAbsFile :: (MonadThrow m, MonadIO m) => FilePath -> m (Path Abs File)
parseRelAsAbsFile fp = parseAbsFile =<< liftIO (D.canonicalizePath fp)

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

-- Internal helper to define resolveDirMaybe and resolveFileMaybe in one
resolveCheckParse :: (MonadIO m)
                 => (FilePath -> IO Bool) -- check if file/dir does exist
                 -> (FilePath -> m a)     -- parse into absolute file/dir
                 -> Path Abs Dir
                 -> FilePath
                 -> m (Maybe a)
resolveCheckParse check parse x y = do
    let fp = toFilePath x FP.</> y
    exists <- liftIO $ check fp
    if exists
        then do
            canonic <- liftIO $ D.canonicalizePath fp
            liftM Just (parse canonic)
        else return Nothing

-- | Appends a stringly-typed relative path to an absolute path, and then
-- canonicalizes it. If the path doesn't exist (and therefore cannot
-- be canonicalized, 'Nothing' is returned).
resolveDirMaybe :: (MonadIO m,MonadThrow m)
                => Path Abs Dir -> FilePath -> m (Maybe (Path Abs Dir))
resolveDirMaybe = resolveCheckParse D.doesDirectoryExist parseAbsDir

-- | Appends a stringly-typed relative path to an absolute path, and then
-- canonicalizes it. If the path doesn't exist (and therefore cannot
-- be canonicalized, 'Nothing' is returned).
resolveFileMaybe :: (MonadIO m,MonadThrow m)
                 => Path Abs Dir -> FilePath -> m (Maybe (Path Abs File))
resolveFileMaybe = resolveCheckParse D.doesFileExist parseAbsFile

-- | Collapse intermediate "." and ".." directories from path, then parse
-- it with 'parseAbsFile'.
-- (probably should be moved to the Path module)
parseCollapsedAbsFile :: MonadThrow m => FilePath -> m (Path Abs File)
parseCollapsedAbsFile = parseAbsFile . collapseFilePath

-- | Collapse intermediate "." and ".." directories from path, then parse
-- it with 'parseAbsDir'.
-- (probably should be moved to the Path module)
parseCollapsedAbsDir :: MonadThrow m => FilePath -> m (Path Abs Dir)
parseCollapsedAbsDir = parseAbsDir . collapseFilePath

-- | Collapse intermediate "." and ".." directories from a path.
--
-- > collapseFilePath "./foo" == "foo"
-- > collapseFilePath "/bar/../baz" == "/baz"
-- > collapseFilePath "/../baz" == "/../baz"
-- > collapseFilePath "parent/foo/baz/../bar" ==  "parent/foo/bar"
-- > collapseFilePath "parent/foo/baz/../../bar" ==  "parent/bar"
-- > collapseFilePath "parent/foo/.." ==  "parent"
-- > collapseFilePath "/parent/foo/../../bar" ==  "/bar"
--
-- (borrowed from @Text.Pandoc.Shared@)
collapseFilePath :: FilePath -> FilePath
collapseFilePath = FP.joinPath . reverse . foldl go [] . FP.splitDirectories
  where
    go rs "." = rs
    go r@(p:rs) ".." = case p of
                            ".." -> ("..":r)
                            (checkPathSeperator -> Just True) -> ("..":r)
                            _ -> rs
    go _ (checkPathSeperator -> Just True) = [[FP.pathSeparator]]
    go rs x = x:rs
    isSingleton [] = Nothing
    isSingleton [x] = Just x
    isSingleton _ = Nothing
    checkPathSeperator = fmap FP.isPathSeparator . isSingleton

-- | List objects in a directory, excluding "@.@" and "@..@".  Entries are not sorted.
listDirectory :: (MonadIO m,MonadThrow m) => Path Abs Dir -> m ([Path Abs Dir],[Path Abs File])
listDirectory dir =
  do entriesFP <- liftIO (D.getDirectoryContents dirFP)
     maybeEntries <-
       forM (map (dirFP ++) entriesFP)
            (\entryFP ->
               do isDir <- liftIO (D.doesDirectoryExist entryFP)
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

-- | Remove a file. Bails out if it doesn't exist.
removeFile :: MonadIO m => Path b File -> m ()
removeFile = liftIO . D.removeFile . toFilePath

-- | Remove a file. Optimistically assumes it exists. If it doesn't,
-- doesn't complain.
removeFileIfExists :: MonadIO m => Path b File -> m ()
removeFileIfExists = ignoreDoesNotExist . removeFile

-- | Rename a file. Bails out if it doesn't exist.
renameFile :: MonadIO m => Path b1 File -> Path b2 File -> m ()
renameFile from to = liftIO (D.renameFile (toFilePath from) (toFilePath to))

-- | Rename a file. Optimistically assumes it exists. If it doesn't,
-- doesn't complain.
renameFileIfExists :: MonadIO m => Path b1 File -> Path b2 File -> m ()
renameFileIfExists from to = ignoreDoesNotExist (renameFile from to)

renameDir :: MonadIO m => Path b1 Dir -> Path b2 Dir -> m ()
renameDir from to = liftIO (D.renameDirectory (toFilePath from) (toFilePath to))

-- | Rename a directory. Optimistically assumes it exists. If it
-- doesn't, doesn't complain.
renameDirIfExists :: MonadIO m => Path b1 Dir -> Path b2 Dir -> m ()
renameDirIfExists from to = ignoreDoesNotExist (renameDir from to)

-- | Make a directory tree, creating parents if needed.
createTree :: MonadIO m => Path b Dir -> m ()
createTree = liftIO . D.createDirectoryIfMissing True . toFilePath

-- | Move a file. Bails out if it doesn't exist.
moveFile :: MonadIO m => Path b1 File -> Path b2 Dir -> m ()
moveFile from to = renameFile from (to </> filename from)

-- | Move a file. Optimistically assumes it exists. If it doesn't,
-- doesn't complain.
moveFileIfExists :: MonadIO m => Path b1 File -> Path b2 Dir -> m ()
moveFileIfExists from to = ignoreDoesNotExist (moveFile from to)

-- | Move a dir. Bails out if it doesn't exist.
moveDir :: MonadIO m => Path b1 Dir -> Path b2 Dir -> m ()
moveDir from to = renameDir from (to </> dirname from)

-- | Move a dir. Optimistically assumes it exists. If it doesn't,
-- doesn't complain.
moveDirIfExists :: MonadIO m => Path b1 Dir -> Path b2 Dir -> m ()
moveDirIfExists from to = ignoreDoesNotExist (moveDir from to)

-- | Remove a tree. Bails out if it doesn't exist.
removeTree :: MonadIO m => Path b Dir -> m ()
removeTree = liftIO . D.removeDirectoryRecursive . toFilePath

-- | Remove tree, don't complain about non-existent directories.
removeTreeIfExists :: MonadIO m => Path b Dir -> m ()
removeTreeIfExists = ignoreDoesNotExist . removeTree

-- | Does the file exist?
fileExists :: MonadIO m => Path b File -> m Bool
fileExists = liftIO . D.doesFileExist . toFilePath

-- | Does the directory exist?
dirExists :: MonadIO m => Path b Dir -> m Bool
dirExists = liftIO . D.doesDirectoryExist . toFilePath

-- | Copies a file to another path. Bails out if it doesn't exist.
copyFile :: MonadIO m => Path b1 File -> Path b2 File -> m ()
copyFile from to = liftIO (D.copyFile (toFilePath from) (toFilePath to))

-- | Copies a file to another path. Optimistically assumes it exists. If
-- it doesn't, doesn't complain.
copyFileIfExists :: MonadIO m => Path b1 File -> Path b2 File -> m ()
copyFileIfExists from to = ignoreDoesNotExist (copyFile from to)

-- | Copy a directory recursively.  This just uses 'copyFile', so it is not smart about symbolic
-- links or other special files.
copyDirectoryRecursive :: (MonadIO m,MonadThrow m)
                       => Path Abs Dir -- ^ Source directory
                       -> Path Abs Dir -- ^ Destination directory
                       -> m ()
copyDirectoryRecursive srcDir destDir =
    do liftIO (D.createDirectoryIfMissing False (toFilePath destDir))
       (srcSubDirs,srcFiles) <- listDirectory srcDir
       forM_ srcFiles
             (\srcFile ->
                case stripDir srcDir srcFile of
                  Nothing -> return ()
                  Just relFile -> copyFile srcFile (destDir </> relFile))
       forM_ srcSubDirs
             (\srcSubDir ->
                case stripDir srcDir srcSubDir of
                  Nothing -> return ()
                  Just relSubDir -> copyDirectoryRecursive srcSubDir (destDir </> relSubDir))


-- | Drop the root (either @\/@ on POSIX or @C:\\@, @D:\\@, etc. on
-- Windows).
dropRoot :: Path Abs t -> Path Rel t
dropRoot (Path l) = Path (FP.dropDrive l)

-- Utility function for a common pattern of ignoring does-not-exist errors.
ignoreDoesNotExist :: MonadIO m => IO () -> m ()
ignoreDoesNotExist f =
    liftIO $ catch f $ \e -> unless (isDoesNotExistError e) (throwIO e)
