{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stack.Constants.Config
  ( buildCachesDir
  , configCabalMod
  , configPackageProjectRoot
  , configSetupConfigMod
  , distDirFromDir
  , distRelativeDir
  , ghciDirL
  , hpcDirFromDir
  , hpcRelativeDir
  , imageStagingDir
  , objectInterfaceDirL
  , projectDockerSandboxDir
  , rootDistDirFromDir
  , setupConfigFromDir
  , templatesDir
  , testBuiltFile
  , testSuccessFile
  , workDirFromDir
  ) where

import           Path ( (</>), mkRelDir, mkRelFile, parseRelDir )
import           Stack.Constants ( relDirDist, relDirGhci, relDirHpc )
import           Stack.Prelude
import           Stack.Types.BuildConfig ( HasBuildConfig, configFileRootL )
import           Stack.Types.Compiler ( compilerVersionString )
import           Stack.Types.CompilerPaths ( compilerVersionL )
import           Stack.Types.Config ( Config, HasConfig, stackRootL, workDirL )
import           Stack.Types.EnvConfig
                   ( HasEnvConfig, platformGhcRelDir, useShaPathOnWindows )

-- | Output .o/.hi directory.
objectInterfaceDirL :: HasBuildConfig env => Getting r env (Path Abs Dir)
objectInterfaceDirL = to $ \env -> -- FIXME is this idiomatic lens code?
  let workDir = view workDirL env
      configFileRoot = view configFileRootL env
  in  configFileRoot </> workDir </> $(mkRelDir "odir/")

-- | GHCi files directory.
ghciDirL :: HasBuildConfig env => Getting r env (Path Abs Dir)
ghciDirL = to $ \env -> -- FIXME is this idiomatic lens code?
  let workDir = view workDirL env
      configFileRoot = view configFileRootL env
  in  configFileRoot </> workDir </> relDirGhci

-- | The directory containing the files used for dirtiness check of source
-- files.
buildCachesDir ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => Path Abs Dir  -- ^ Package directory.
  -> m (Path Abs Dir)
buildCachesDir dir =
  fmap
    (</> $(mkRelDir "stack-build-caches"))
    (distDirFromDir dir)

-- | The filename used to mark tests as having succeeded.
testSuccessFile ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => Path Abs Dir  -- ^ Package directory
  -> m (Path Abs File)
testSuccessFile dir =
  fmap
    (</> $(mkRelFile "stack-test-success"))
    (distDirFromDir dir)

-- | The filename used to mark tests as having built.
testBuiltFile ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => Path Abs Dir  -- ^ Package directory
  -> m (Path Abs File)
testBuiltFile dir =
  fmap
    (</> $(mkRelFile "stack-test-built"))
    (distDirFromDir dir)

-- | The filename used for modification check of a Cabal file.
configCabalMod ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => Path Abs Dir  -- ^ Package directory.
  -> m (Path Abs File)
configCabalMod dir =
  fmap
    (</> $(mkRelFile "stack-cabal-mod"))
    (distDirFromDir dir)

-- | The filename used for modification check of setup-config.
configSetupConfigMod ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => Path Abs Dir  -- ^ Package directory.
  -> m (Path Abs File)
configSetupConfigMod dir =
  fmap
    (</> $(mkRelFile "stack-setup-config-mod"))
    (distDirFromDir dir)

-- | The filename used for the project root from the last build of a package.
configPackageProjectRoot ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => Path Abs Dir  -- ^ Package directory.
  -> m (Path Abs File)
configPackageProjectRoot dir =
  fmap
    (</> $(mkRelFile "stack-project-root"))
    (distDirFromDir dir)

-- | Directory for HPC work.
hpcDirFromDir ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => Path Abs Dir  -- ^ Package directory.
  -> m (Path Abs Dir)
hpcDirFromDir fp =
  fmap (fp </>) hpcRelativeDir

-- | Relative location of directory for HPC work.
hpcRelativeDir ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => m (Path Rel Dir)
hpcRelativeDir =
  fmap (</> relDirHpc) distRelativeDir

-- | Package's setup-config storing Cabal configuration.
setupConfigFromDir ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => Path Abs Dir
  -> m (Path Abs File)
setupConfigFromDir fp = do
  dist <- distDirFromDir fp
  pure $ dist </> $(mkRelFile "setup-config")

-- | Package's build artifacts directory.
distDirFromDir ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => Path Abs Dir
  -> m (Path Abs Dir)
distDirFromDir fp =
  fmap (fp </>) distRelativeDir

-- | The directory containing all dist directories, including all
-- different platform/compiler combinations.
rootDistDirFromDir ::
     (HasConfig env, MonadReader env m)
  => Path Abs Dir
  -> m (Path Abs Dir)
rootDistDirFromDir fp =
  fmap (fp </>) rootDistRelativeDir

-- | Relative directory to the top dist directory, containing
-- individual platform/compiler combinations as subdirs.
rootDistRelativeDir ::
     (HasConfig env, MonadReader env m)
  => m (Path Rel Dir)
rootDistRelativeDir = do
  workDir <- view workDirL
  pure $ workDir </> relDirDist

-- | Package's working directory.
workDirFromDir ::
     (HasConfig env, MonadReader env m)
  => Path Abs Dir
  -> m (Path Abs Dir)
workDirFromDir fp = view $ workDirL.to (fp </>)

-- | Directory for project templates.
templatesDir :: Config -> Path Abs Dir
templatesDir config = view stackRootL config </> $(mkRelDir "templates")

-- | Relative location of build artifacts.
distRelativeDir ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => m (Path Rel Dir)
distRelativeDir = do
  compilerVer <- view compilerVersionL
  platform <- platformGhcRelDir
  -- Compiler version: allows build artefacts to be distinguished by compiler
  -- version, which will also distinguish one Cabal version from another.
  compilerDir <- parseRelDir $ compilerVersionString compilerVer
  platformAndCompiler <- useShaPathOnWindows (platform </> compilerDir)
  allDist <- rootDistRelativeDir
  pure $ allDist </> platformAndCompiler

-- | Docker sandbox from project root.
projectDockerSandboxDir :: (HasConfig env, MonadReader env m)
  => Path Abs Dir      -- ^ Project root
  -> m (Path Abs Dir)  -- ^ Docker sandbox
projectDockerSandboxDir projectRoot = do
  workDir <- view workDirL
  pure $ projectRoot </> workDir </> $(mkRelDir "docker/")

-- | Image staging dir from project root.
imageStagingDir ::
     (HasConfig env, MonadReader env m, MonadThrow m)
  => Path Abs Dir      -- ^ Project root
  -> Int               -- ^ Index of image
  -> m (Path Abs Dir)  -- ^ Docker sandbox
imageStagingDir projectRoot imageIdx = do
  workDir <- view workDirL
  idxRelDir <- parseRelDir (show imageIdx)
  pure $ projectRoot </> workDir </> $(mkRelDir "image") </> idxRelDir
