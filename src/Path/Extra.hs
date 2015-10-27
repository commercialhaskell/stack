{-# LANGUAGE ViewPatterns #-}

-- | Extra Path utilities.

module Path.Extra
  (toFilePathNoTrailingSep
  ,dropRoot
  ,parseCollapsedAbsDir
  ,parseCollapsedAbsFile
  ) where

import           Control.Monad.Catch
import           Path
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
