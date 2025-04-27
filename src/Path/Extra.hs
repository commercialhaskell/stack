{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

{-|
Module      : Path.Extra
Description : Extra Path utilities.
License     : BSD-3-Clause
-}

module Path.Extra
  ( toFilePathNoTrailingSep
  , parseCollapsedAbsDir
  , parseCollapsedAbsFile
  , concatAndCollapseAbsDir
  , rejectMissingFile
  , rejectMissingDir
  , pathToByteString
  , pathToLazyByteString
  , pathToText
  , tryGetModificationTime
  , forgivingResolveDir
  , forgivingResolveFile
  , forgivingResolveFile'
  ) where

import           Data.Time ( UTCTime )
import           Path
                   ( Abs, Dir, File, Path, PathException (..), parseAbsDir
                   , parseAbsFile, toFilePath
                   )
import           Path.IO
                   ( doesDirExist, doesFileExist, getCurrentDir
                   , getModificationTime
                   )
import           RIO
import           System.IO.Error ( isDoesNotExistError )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.Directory as D
import qualified System.FilePath as FP

-- | Convert to FilePath but don't add a trailing slash.
toFilePathNoTrailingSep :: Path loc Dir -> FilePath
toFilePathNoTrailingSep = FP.dropTrailingPathSeparator . toFilePath

-- | Collapse intermediate "." and ".." directories from path, then parse
-- it with 'parseAbsDir'.
-- (probably should be moved to the Path module)
parseCollapsedAbsDir :: MonadThrow m => FilePath -> m (Path Abs Dir)
parseCollapsedAbsDir = parseAbsDir . collapseFilePath

-- | Collapse intermediate "." and ".." directories from path, then parse
-- it with 'parseAbsFile'.
-- (probably should be moved to the Path module)
parseCollapsedAbsFile :: MonadThrow m => FilePath -> m (Path Abs File)
parseCollapsedAbsFile = parseAbsFile . collapseFilePath

-- | Add a relative FilePath to the end of a Path
-- We can't parse the FilePath first because we need to account for ".."
-- in the FilePath (#2895)
concatAndCollapseAbsDir ::
     MonadThrow m
  => Path Abs Dir
  -> FilePath
  -> m (Path Abs Dir)
concatAndCollapseAbsDir base rel =
  parseCollapsedAbsDir (toFilePath base FP.</> rel)

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
-- (adapted from @Text.Pandoc.Shared@)
collapseFilePath :: FilePath -> FilePath
collapseFilePath = FP.joinPath . reverse . foldl' go [] . FP.splitDirectories
 where
  go rs "." = rs
  go r@(p:rs) ".." = case p of
                          ".." -> "..":r
                          (checkPathSeparator -> True) -> "..":r
                          _ -> rs
  go _ (checkPathSeparator -> True) = [[FP.pathSeparator]]
  go rs x = x:rs
  checkPathSeparator [x] = FP.isPathSeparator x
  checkPathSeparator _ = False

-- | If given file in 'Maybe' does not exist, ensure we have 'Nothing'. This
-- is to be used in conjunction with 'Path.IO.forgivingAbsence' and
-- 'Path.IO.resolveFile'.
--
-- Previously the idiom @forgivingAbsence (resolveFile …)@ alone was used, which
-- relied on 'Path.IO.canonicalizePath' throwing 'isDoesNotExistError' when path
-- does not exist. As it turns out, this behavior is actually not intentional
-- and unreliable, see <https://github.com/haskell/directory/issues/44>. This
-- was “fixed” in version @1.2.3.0@ of @directory@ package (now it never
-- throws). To make it work with all versions, we need to use the following
-- idiom:
--
-- > forgivingAbsence (resolveFile …) >>= rejectMissingFile

rejectMissingFile ::
     MonadIO m
  => Maybe (Path Abs File)
  -> m (Maybe (Path Abs File))
rejectMissingFile Nothing = pure Nothing
rejectMissingFile (Just p) = bool Nothing (Just p) <$> doesFileExist p

-- | See 'rejectMissingFile'.

rejectMissingDir ::
     MonadIO m
  => Maybe (Path Abs Dir)
  -> m (Maybe (Path Abs Dir))
rejectMissingDir Nothing = pure Nothing
rejectMissingDir (Just p) = bool Nothing (Just p) <$> doesDirExist p

-- | Convert to a lazy ByteString using toFilePath and UTF8.
pathToLazyByteString :: Path b t -> BSL.ByteString
pathToLazyByteString = BSL.fromStrict . pathToByteString

-- | Convert to a ByteString using toFilePath and UTF8.
pathToByteString :: Path b t -> BS.ByteString
pathToByteString = T.encodeUtf8 . pathToText

pathToText :: Path b t -> T.Text
pathToText = T.pack . toFilePath

tryGetModificationTime :: MonadIO m => Path Abs File -> m (Either () UTCTime)
tryGetModificationTime =
  liftIO . tryJust (guard . isDoesNotExistError) . getModificationTime

-- | 'Path.IO.resolveDir' (@path-io@ package) throws 'InvalidAbsDir' (@path@
-- package) if the directory does not exist; this function yields 'Nothing'.
forgivingResolveDir ::
     MonadIO m
  => Path Abs Dir
     -- ^ Base directory
  -> FilePath
     -- ^ Path to resolve
  -> m (Maybe (Path Abs Dir))
forgivingResolveDir b p = liftIO $
  D.canonicalizePath (toFilePath b FP.</> p) >>= \cp ->
    catch
      (Just <$> parseAbsDir cp)
      ( \e -> case e of
          InvalidAbsDir _ -> pure Nothing
          _ -> throwIO e
      )

-- | 'Path.IO.resolveFile' (@path-io@ package) throws 'InvalidAbsFile' (@path@
-- package) if the file does not exist; this function yields 'Nothing'.
forgivingResolveFile ::
     MonadIO m
  => Path Abs Dir
     -- ^ Base directory
  -> FilePath
     -- ^ Path to resolve
  -> m (Maybe (Path Abs File))
forgivingResolveFile b p = liftIO $
  D.canonicalizePath (toFilePath b FP.</> p) >>= \cp ->
    catch
      (Just <$> parseAbsFile cp)
      ( \e -> case e of
          InvalidAbsFile _ -> pure Nothing
          _ -> throwIO e
      )

-- | 'Path.IO.resolveFile'' (@path-io@ package) throws 'InvalidAbsFile' (@path@
-- package) if the file does not exist; this function yields 'Nothing'.
forgivingResolveFile' ::
     MonadIO m
  => FilePath
     -- ^ Path to resolve
  -> m (Maybe (Path Abs File))
forgivingResolveFile' p = getCurrentDir >>= flip forgivingResolveFile p
