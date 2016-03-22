{-# LANGUAGE ViewPatterns #-}

-- | Extra Path utilities.

module Path.Extra
  (toFilePathNoTrailingSep
  ,dropRoot
  ,parseCollapsedAbsDir
  ,parseCollapsedAbsFile
  ,rejectMissingFile
  ,rejectMissingDir
  ) where

import           Control.Monad (liftM)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Bool (bool)
import           Path
import           Path.IO
import           Path.Internal (Path(..))
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
                            ".." -> "..":r
                            (checkPathSeperator -> Just True) -> "..":r
                            _ -> rs
    go _ (checkPathSeperator -> Just True) = [[FP.pathSeparator]]
    go rs x = x:rs
    isSingleton [] = Nothing
    isSingleton [x] = Just x
    isSingleton _ = Nothing
    checkPathSeperator = fmap FP.isPathSeparator . isSingleton

-- | Drop the root (either @\/@ on POSIX or @C:\\@, @D:\\@, etc. on
-- Windows).
dropRoot :: Path Abs t -> Path Rel t
dropRoot (Path l) = Path (FP.dropDrive l)

-- | If given file in 'Maybe' does not exist, ensure we have 'Nothing'. This
-- is to be used in conjunction with 'forgivingAbsence' and
-- 'resolveFile'.
--
-- Previously the idiom @forgivingAbsence (relsoveFile …)@ alone was used,
-- which relied on 'canonicalizePath' throwing 'isDoesNotExistError' when
-- path does not exist. As it turns out, this behavior is actually not
-- intentional and unreliable, see
-- <https://github.com/haskell/directory/issues/44>. This was “fixed” in
-- version @1.2.3.0@ of @directory@ package (now it never throws). To make
-- it work with all versions, we need to use the following idiom:
--
-- > forgivingAbsence (resolveFile …) >>= rejectMissingFile

rejectMissingFile :: MonadIO m
  => Maybe (Path Abs File)
  -> m (Maybe (Path Abs File))
rejectMissingFile Nothing = return Nothing
rejectMissingFile (Just p) = bool Nothing (Just p) `liftM` doesFileExist p

-- | See 'rejectMissingFile'.

rejectMissingDir :: MonadIO m
  => Maybe (Path Abs Dir)
  -> m (Maybe (Path Abs Dir))
rejectMissingDir Nothing = return Nothing
rejectMissingDir (Just p) = bool Nothing (Just p) `liftM` doesDirExist p
