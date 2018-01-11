{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Stack.Constants.Config
  ( distDirFromDir
  , workDirFromDir
  , distRelativeDir
  , imageStagingDir
  , projectDockerSandboxDir
  , configCacheFile
  , configCabalMod
  , buildCachesDir
  , testSuccessFile
  , testBuiltFile
  , hpcRelativeDir
  , hpcDirFromDir
  , objectInterfaceDirL
  , templatesDir
  ) where

import Stack.Prelude
import Stack.Constants
import Stack.Types.Compiler
import Stack.Types.Config
import Stack.Types.PackageIdentifier
import Path

-- | Output .o/.hi directory.
objectInterfaceDirL :: HasBuildConfig env => Getting r env (Path Abs Dir)
objectInterfaceDirL = to $ \env -> -- FIXME is this idomatic lens code?
  let workDir = view workDirL env
      root = view projectRootL env
   in root </> workDir </> $(mkRelDir "odir/")

-- | The directory containing the files used for dirtiness check of source files.
buildCachesDir :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
               => Path Abs Dir      -- ^ Package directory.
               -> m (Path Abs Dir)
buildCachesDir dir = 
    liftM
        (</> $(mkRelDir "stack-build-caches"))
        (distDirFromDir dir)

-- | The filename used to mark tests as having succeeded
testSuccessFile :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
                => Path Abs Dir -- ^ Package directory
                -> m (Path Abs File)
testSuccessFile dir =
    liftM
        (</> $(mkRelFile "stack-test-success"))
        (distDirFromDir dir)

-- | The filename used to mark tests as having built
testBuiltFile :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
              => Path Abs Dir -- ^ Package directory
              -> m (Path Abs File)
testBuiltFile dir =
    liftM
        (</> $(mkRelFile "stack-test-built"))
        (distDirFromDir dir)

-- | The filename used for dirtiness check of config.
configCacheFile :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
                => Path Abs Dir      -- ^ Package directory.
                -> m (Path Abs File)
configCacheFile dir =
    liftM
        (</> $(mkRelFile "stack-config-cache"))
        (distDirFromDir dir)

-- | The filename used for modification check of .cabal
configCabalMod :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
               => Path Abs Dir      -- ^ Package directory.
               -> m (Path Abs File)
configCabalMod dir =
    liftM
        (</> $(mkRelFile "stack-cabal-mod"))
        (distDirFromDir dir)

-- | Directory for HPC work.
hpcDirFromDir
    :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
    => Path Abs Dir  -- ^ Package directory.
    -> m (Path Abs Dir)
hpcDirFromDir fp =
    liftM (fp </>) hpcRelativeDir

-- | Relative location of directory for HPC work.
hpcRelativeDir :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
               => m (Path Rel Dir)
hpcRelativeDir =
    liftM (</> $(mkRelDir "hpc")) distRelativeDir

-- | Package's build artifacts directory.
distDirFromDir :: (MonadThrow m, MonadReader env m, HasEnvConfig env)
               => Path Abs Dir
               -> m (Path Abs Dir)
distDirFromDir fp =
    liftM (fp </>) distRelativeDir

-- | Package's working directory.
workDirFromDir :: (MonadReader env m, HasEnvConfig env)
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
    wc <- view $ actualCompilerVersionL.to whichCompiler
    -- Cabal version, suffixed with "_ghcjs" if we're using GHCJS.
    envDir <-
        parseRelDir $
        (if wc == Ghcjs then (++ "_ghcjs") else id) $
        packageIdentifierString $
        PackageIdentifier cabalPackageName cabalPkgVer
    platformAndCabal <- useShaPathOnWindows (platform </> envDir)
    workDir <- view workDirL
    return $
        workDir </>
        $(mkRelDir "dist") </>
        platformAndCabal

-- | Docker sandbox from project root.
projectDockerSandboxDir :: (MonadReader env m, HasConfig env)
  => Path Abs Dir      -- ^ Project root
  -> m (Path Abs Dir)  -- ^ Docker sandbox
projectDockerSandboxDir projectRoot = do
  workDir <- view workDirL
  return $ projectRoot </> workDir </> $(mkRelDir "docker/")

-- | Image staging dir from project root.
imageStagingDir :: (MonadReader env m, HasConfig env, MonadThrow m)
  => Path Abs Dir      -- ^ Project root
  -> Int               -- ^ Index of image
  -> m (Path Abs Dir)  -- ^ Docker sandbox
imageStagingDir projectRoot imageIdx = do
  workDir <- view workDirL
  idxRelDir <- parseRelDir (show imageIdx)
  return $ projectRoot </> workDir </> $(mkRelDir "image") </> idxRelDir
