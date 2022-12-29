{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stack.Constants.Config
  ( distDirFromDir
  , rootDistDirFromDir
  , setupConfigFromDir
  , workDirFromDir
  , distRelativeDir
  , imageStagingDir
  , projectDockerSandboxDir
  , configCabalMod
  , configSetupConfigMod
  , configPackageProjectRoot
  , buildCachesDir
  , testSuccessFile
  , testBuiltFile
  , hpcRelativeDir
  , hpcDirFromDir
  , objectInterfaceDirL
  , ghciDirL
  , templatesDir
  ) where

import           Path
import           Stack.Constants
import           Stack.Prelude
import           Stack.Types.Config

-- | Output .o/.hi directory.
objectInterfaceDirL :: HasBuildConfig env => Getting r env (Path Abs Dir)
objectInterfaceDirL = to $ \env -> -- FIXME is this idiomatic lens code?
  let workDir = view workDirL env
      root = view projectRootL env
  in  root </> workDir </> $(mkRelDir "odir/")

-- | GHCi files directory.
ghciDirL :: HasBuildConfig env => Getting r env (Path Abs Dir)
ghciDirL = to $ \env -> -- FIXME is this idiomatic lens code?
  let workDir = view workDirL env
      root = view projectRootL env
  in  root </> workDir </> $(mkRelDir "ghci/")

-- | The directory containing the files used for dirtiness check of source files.
buildCachesDir :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
               => Path Abs Dir      -- ^ Package directory.
               -> m (Path Abs Dir)
buildCachesDir dir =
  fmap
    (</> $(mkRelDir "stack-build-caches"))
    (distDirFromDir dir)

-- | The filename used to mark tests as having succeeded
testSuccessFile :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
                => Path Abs Dir -- ^ Package directory
                -> m (Path Abs File)
testSuccessFile dir =
  fmap
    (</> $(mkRelFile "stack-test-success"))
    (distDirFromDir dir)

-- | The filename used to mark tests as having built
testBuiltFile :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
              => Path Abs Dir -- ^ Package directory
              -> m (Path Abs File)
testBuiltFile dir =
  fmap
    (</> $(mkRelFile "stack-test-built"))
    (distDirFromDir dir)

-- | The filename used for modification check of .cabal
configCabalMod :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
               => Path Abs Dir      -- ^ Package directory.
               -> m (Path Abs File)
configCabalMod dir =
  fmap
    (</> $(mkRelFile "stack-cabal-mod"))
    (distDirFromDir dir)

-- | The filename used for modification check of setup-config
configSetupConfigMod :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
                     => Path Abs Dir      -- ^ Package directory.
                     -> m (Path Abs File)
configSetupConfigMod dir =
  fmap
    (</> $(mkRelFile "stack-setup-config-mod"))
    (distDirFromDir dir)

-- | The filename used for the project root from the last build of a package
configPackageProjectRoot :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
                     => Path Abs Dir      -- ^ Package directory.
                     -> m (Path Abs File)
configPackageProjectRoot dir =
  fmap
    (</> $(mkRelFile "stack-project-root"))
    (distDirFromDir dir)

-- | Directory for HPC work.
hpcDirFromDir ::
       (MonadThrow m, MonadReader env m, HasEnvConfig env)
    => Path Abs Dir  -- ^ Package directory.
    -> m (Path Abs Dir)
hpcDirFromDir fp =
  fmap (fp </>) hpcRelativeDir

-- | Relative location of directory for HPC work.
hpcRelativeDir :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
               => m (Path Rel Dir)
hpcRelativeDir =
  fmap (</> $(mkRelDir "hpc")) distRelativeDir

-- | Package's setup-config storing Cabal configuration
setupConfigFromDir :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
                   => Path Abs Dir
                   -> m (Path Abs File)
setupConfigFromDir fp = do
    dist <- distDirFromDir fp
    pure $ dist </> $(mkRelFile "setup-config")

-- | Package's build artifacts directory.
distDirFromDir :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
               => Path Abs Dir
               -> m (Path Abs Dir)
distDirFromDir fp =
  fmap (fp </>) distRelativeDir

-- | The directory containing all dist directories, including all
-- different GHC/Cabal combos.
rootDistDirFromDir ::
     (MonadReader env m, HasConfig env)
  => Path Abs Dir
  -> m (Path Abs Dir)
rootDistDirFromDir fp =
  fmap (fp </>) rootDistRelativeDir

-- | Relative directory to the top dist directory, containing
-- individual GHC/Cabal combo as subdirs.
rootDistRelativeDir ::
     (MonadReader env m, HasConfig env)
  => m (Path Rel Dir)
rootDistRelativeDir = do
    workDir <- view workDirL
    pure $ workDir </> $(mkRelDir "dist")

-- | Package's working directory.
workDirFromDir :: (MonadReader env m, HasConfig env)
               => Path Abs Dir
               -> m (Path Abs Dir)
workDirFromDir fp = view $ workDirL.to (fp </>)

-- | Directory for project templates.
templatesDir :: Config -> Path Abs Dir
templatesDir config = view stackRootL config </> $(mkRelDir "templates")

-- | Relative location of build artifacts.
distRelativeDir :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
                => m (Path Rel Dir)
distRelativeDir = do
    cabalPkgVer <- view cabalVersionL
    platform <- platformGhcRelDir
    -- Cabal version
    envDir <-
        parseRelDir $
        packageIdentifierString $
        PackageIdentifier cabalPackageName cabalPkgVer
    platformAndCabal <- useShaPathOnWindows (platform </> envDir)
    allDist <- rootDistRelativeDir
    pure $ allDist </> platformAndCabal

-- | Docker sandbox from project root.
projectDockerSandboxDir :: (MonadReader env m, HasConfig env)
  => Path Abs Dir      -- ^ Project root
  -> m (Path Abs Dir)  -- ^ Docker sandbox
projectDockerSandboxDir projectRoot = do
  workDir <- view workDirL
  pure $ projectRoot </> workDir </> $(mkRelDir "docker/")

-- | Image staging dir from project root.
imageStagingDir :: (MonadReader env m, HasConfig env, MonadThrow m)
  => Path Abs Dir      -- ^ Project root
  -> Int               -- ^ Index of image
  -> m (Path Abs Dir)  -- ^ Docker sandbox
imageStagingDir projectRoot imageIdx = do
  workDir <- view workDirL
  idxRelDir <- parseRelDir (show imageIdx)
  pure $ projectRoot </> workDir </> $(mkRelDir "image") </> idxRelDir
