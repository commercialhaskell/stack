{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Constants used throughout the project.

module Stack.Constants
    (builtConfigFileFromDir
    ,builtFileFromDir
    ,configuredFileFromDir
    ,defaultShakeThreads
    ,distDirFromDir
    ,distRelativeDir
    ,haskellFileExts
    ,packageDownloadPrefix
    ,projectDockerSandboxDir
    ,rawGithubUrl
    ,stackDotYaml
    ,stackRootEnvVar
    ,userDocsDir
    )
    where

import Control.Monad (liftM)
import Control.Monad.Catch (MonadThrow)
import Data.Text (Text)
import qualified Data.Text as T
import Path as FL
import Prelude
import Stack.Types.Config
import Stack.Types.PackageIdentifier
import Stack.Types.Version

-- | Extensions used for Haskell files.
haskellFileExts :: [Text]
haskellFileExts = ["hs","hsc","lhs"]

-- | The filename used for completed build indicators.
builtFileFromDir :: MonadThrow m
                 => PackageIdentifier -- ^ Cabal version
                 -> Path Abs Dir
                 -> m (Path Abs File)
builtFileFromDir cabalPkgVer fp = do
  dist <- distDirFromDir cabalPkgVer fp
  return (dist </> $(mkRelFile "stack.gen"))

-- | The filename used for completed configure indicators.
configuredFileFromDir :: MonadThrow m
                      => PackageIdentifier -- ^ Cabal version
                      -> Path Abs Dir
                      -> m (Path Abs File)
configuredFileFromDir cabalPkgVer fp = do
  dist <- distDirFromDir cabalPkgVer fp
  return (dist </> $(mkRelFile "setup-config"))

-- | The filename used for completed build indicators.
builtConfigFileFromDir :: MonadThrow m
                       => PackageIdentifier -- ^ Cabal version
                       -> Path Abs Dir
                       -> m (Path Abs File)
builtConfigFileFromDir cabalPkgVer fp =
    liftM (fp </>) (builtConfigRelativeFile cabalPkgVer)

-- | Relative location of completed build indicators.
builtConfigRelativeFile :: MonadThrow m
                        => PackageIdentifier -- ^ Cabal version
                        -> m (Path Rel File)
builtConfigRelativeFile cabalPkgVer = do
  dist <- distRelativeDir cabalPkgVer
  return (dist </> $(mkRelFile "stack.config"))

-- | Default shake thread count for parallel builds.
defaultShakeThreads :: Int
defaultShakeThreads = 4

-- -- | Hoogle database file.
-- hoogleDatabaseFile :: Path Abs Dir -> Path Abs File
-- hoogleDatabaseFile docLoc =
--   docLoc </>
--   $(mkRelFile "default.hoo")

-- -- | Extension for hoogle databases.
-- hoogleDbExtension :: String
-- hoogleDbExtension = "hoo"

-- -- | Extension of haddock files
-- haddockExtension :: String
-- haddockExtension = "haddock"

-- | User documentation directory.
userDocsDir :: Config -> Path Abs Dir
userDocsDir config = configStackRoot config </> $(mkRelDir "doc/")

-- | Package's build artifacts directory.
distDirFromDir :: MonadThrow m
               => PackageIdentifier -- ^ Cabal version
               -> Path Abs Dir
               -> m (Path Abs Dir)
distDirFromDir cabalPkgVersion fp =
    liftM (fp </>) (distRelativeDir cabalPkgVersion)

-- | Relative location of build artifacts.
distRelativeDir :: MonadThrow m
                => PackageIdentifier -- ^ Cabal version
                -> m (Path Rel Dir)
distRelativeDir cabalPkgVer = do
    cabal <- parseRelDir $ "Cabal-" ++
             versionString (packageIdentifierVersion cabalPkgVer)
    return $ $(mkRelDir "dist-stack/") </> cabal

-- pkgIndexDir :: Config -> Path Abs Dir
-- pkgIndexDir config =
--   configStackRoot config </>
--   $(mkRelDir "package-index")

-- pkgIndexFile :: Config -> Path Abs File
-- pkgIndexFile config =
--   pkgIndexDir config </>
--   $(mkRelFile "00-index.tar")

-- | URL prefix for downloading packages
packageDownloadPrefix :: Text
packageDownloadPrefix = "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"

-- | Get a URL for a raw file on Github
rawGithubUrl :: Text -- ^ user/org name
             -> Text -- ^ repo name
             -> Text -- ^ branch name
             -> Text -- ^ filename
             -> Text
rawGithubUrl org repo branch file = T.concat
    [ "https://raw.githubusercontent.com/"
    , org
    , "/"
    , repo
    , "/"
    , branch
    , "/"
    , file
    ]

-- -- | Hoogle database file.
-- hoogleDatabaseFile :: Path Abs Dir -> Path Abs File
-- hoogleDatabaseFile docLoc =
--   docLoc </>
--   $(mkRelFile "default.hoo")

-- -- | Extension for hoogle databases.
-- hoogleDbExtension :: String
-- hoogleDbExtension = "hoo"

-- -- | Extension of haddock files
-- haddockExtension :: String
-- haddockExtension = "haddock"

-- | Docker sandbox from project root.
projectDockerSandboxDir :: Path Abs Dir -> Path Abs Dir
projectDockerSandboxDir projectRoot = projectRoot </> $(mkRelDir ".docker-sandbox/")

-- | The filename used for the stack config file.
stackDotYaml :: Path Rel File
stackDotYaml = $(mkRelFile "stack.yaml")

-- | Environment variable used to override the '~/.stack' location.
stackRootEnvVar :: String
stackRootEnvVar = "STACK_ROOT"
