{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Default values used in the project.

module Stackage.Build.Defaults where

import Data.Text (Text)
import Filesystem.Path.CurrentOS ()
import Path as FL
import Prelude
import Stackage.Package
import System.Directory

-- | Default name used for config path.
configFileName :: Path Rel File
configFileName = $(mkRelFile "stackage-build.config")

-- | The filename used for completed build indicators.
builtFileFromDir :: Path Abs Dir -> Path Abs File
builtFileFromDir fp =
  distDirFromDir fp </>
  $(mkRelFile "stackage-build.gen")

-- | The filename used for completed build indicators.
builtConfigFileFromDir :: Path Abs Dir -> Path Abs File
builtConfigFileFromDir fp = fp </> builtConfigRelativeFile

-- | Relative location of completed build indicators.
builtConfigRelativeFile :: Path Rel File
builtConfigRelativeFile =
  distRelativeDir </>
  $(mkRelFile "stackage-build.config")

-- | Extensions used for Haskell files.
haskellFileExts :: [Text]
haskellFileExts = ["hs","hsc","lhs"]

-- | Default shake thread count for parallel builds.
defaultShakeThreads :: Int
defaultShakeThreads = 4

-- | Path for the project's build log.
buildLogPath :: Package -> Path Abs File
buildLogPath package =
  stackageBuildDir package </>
  $(mkRelFile "build-log")

-- | Get the build directory.
stackageBuildDir :: Package -> Path Abs Dir
stackageBuildDir package =
  distDirFromDir dir </>
  $(mkRelDir "stackage-build")
  where dir = packageDir package

-- | Hoogle database file.
hoogleDatabaseFile :: Path Abs Dir -> Path Abs File
hoogleDatabaseFile docLoc =
  docLoc </>
  $(mkRelFile "default.hoo")

-- | Extension for hoogle databases.
hoogleDbExtension :: String
hoogleDbExtension = "hoo"

-- | Extension of haddock files
haddockExtension :: String
haddockExtension = "haddock"

-- | User documentation directory.
userDocsDir :: Path Abs Dir -> Path Abs Dir
userDocsDir homeDir = homeDir </> $(mkRelDir ".cabal/doc/")

-- | Package's documentation directory.
packageDocDir :: Package -> Path Abs Dir
packageDocDir package =
  distDirFromDir (packageDir package) </>
  $(mkRelDir "doc/")

-- | Package's build artifacts directory.
distDirFromDir :: Path Abs Dir -> Path Abs Dir
distDirFromDir fp = fp </> distRelativeDir

-- | Relative location of build artifacts.
distRelativeDir :: Path Rel Dir
distRelativeDir = $(mkRelDir "dist/")

-- | Get the package index directory.
getIndexDir :: IO (Path Abs Dir)
getIndexDir =
  do homeDir <- getHomeDirectory >>= parseAbsDir
     return (homeDir </>
             $(mkRelDir ".stackage/pkg-index"))
