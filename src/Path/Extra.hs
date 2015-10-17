-- | Extra Path utilities.

module Path.Extra where

import Path
import System.FilePath

toFilePathNoTrailingSlash :: Path loc Dir -> FilePath
toFilePathNoTrailingSlash = dropTrailingPathSeparator . toFilePath
