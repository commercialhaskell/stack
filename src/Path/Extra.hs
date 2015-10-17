-- | Extra Path utilities.

module Path.Extra where

import Path
import System.FilePath

toFilePathNoTrailingSep :: Path loc Dir -> FilePath
toFilePathNoTrailingSep = dropTrailingPathSeparator . toFilePath
