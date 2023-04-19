{-# LANGUAGE NoImplicitPrelude #-}

module Stack.Types.EnvConfig
  ( EnvConfig (..)
  , HasEnvConfig (..)
  , HasSourceMap (..)
  , actualCompilerVersionL
  , appropriateGhcColorFlag
  , bindirCompilerTools
  , compilerVersionDir
  , extraBinDirs
  , hoogleDatabasePath
  , hoogleRoot
  , hpcReportDir
  , installationRootDeps
  , installationRootLocal
  , packageDatabaseDeps
  , packageDatabaseExtra
  , packageDatabaseLocal
  , platformGhcRelDir
  , platformGhcVerOnlyRelDir
  , platformSnapAndCompilerRel
  , shouldForceGhcColorFlag
  , snapshotsDir
  ) where

import qualified Distribution.Text ( display )
import           Distribution.Version ( mkVersion )
import           Path ( (</>), parseRelDir )
import           RIO.Process ( HasProcessContext (..) )
import           Stack.Constants
                   ( bindirSuffix, ghcColorForceFlag, relDirCompilerTools
                   , relDirHoogle, relDirHpc, relDirInstall, relDirPkgdb
                   , relDirSnapshots, relFileDatabaseHoo
                   )
import           Stack.Prelude
import           Stack.Types.Compiler
                   ( ActualCompiler (..), compilerVersionString, getGhcVersion )
import           Stack.Types.CompilerBuild ( compilerBuildSuffix )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), HasCompiler (..) )
import           Stack.Types.Config
                    ( BuildConfig (..), HasBuildConfig (..), HasConfig (..)
                    , HasGHCVariant (..), HasPlatform (..), getProjectWorkDir
                    , stackRootL, useShaPathOnWindows
                    )
import           Stack.Types.Config.Build ( BuildOptsCLI )
import           Stack.Types.GHCVariant ( ghcVariantSuffix )
import           Stack.Types.PlatformVariant ( platformVariantSuffix )
import           Stack.Types.Runner ( HasRunner (..) )
import           Stack.Types.SourceMap
                   ( SourceMap (..), SourceMapHash, smRelDir )

-- | Configuration after the environment has been setup.
data EnvConfig = EnvConfig
  { envConfigBuildConfig :: !BuildConfig
  , envConfigBuildOptsCLI :: !BuildOptsCLI
  , envConfigSourceMap :: !SourceMap
  , envConfigSourceMapHash :: !SourceMapHash
  , envConfigCompilerPaths :: !CompilerPaths
  }

instance HasConfig EnvConfig where
  configL = buildConfigL.lens bcConfig (\x y -> x { bcConfig = y })
  {-# INLINE configL #-}

instance HasBuildConfig EnvConfig where
  buildConfigL = envConfigL.lens
    envConfigBuildConfig
    (\x y -> x { envConfigBuildConfig = y })

instance HasPlatform EnvConfig

instance HasGHCVariant EnvConfig

instance HasProcessContext EnvConfig where
  processContextL = configL.processContextL

instance HasPantryConfig EnvConfig where
  pantryConfigL = configL.pantryConfigL

instance HasCompiler EnvConfig where
  compilerPathsL = to envConfigCompilerPaths

instance HasRunner EnvConfig where
  runnerL = configL.runnerL

instance HasLogFunc EnvConfig where
  logFuncL = runnerL.logFuncL

instance HasStylesUpdate EnvConfig where
  stylesUpdateL = runnerL.stylesUpdateL

instance HasTerm EnvConfig where
  useColorL = runnerL.useColorL
  termWidthL = runnerL.termWidthL

class (HasBuildConfig env, HasSourceMap env, HasCompiler env) => HasEnvConfig env where
  envConfigL :: Lens' env EnvConfig

instance HasEnvConfig EnvConfig where
  envConfigL = id
  {-# INLINE envConfigL #-}

class HasSourceMap env where
  sourceMapL :: Lens' env SourceMap

instance HasSourceMap EnvConfig where
  sourceMapL = lens envConfigSourceMap (\x y -> x { envConfigSourceMap = y })

shouldForceGhcColorFlag ::
     (HasEnvConfig env, HasRunner env)
  => RIO env Bool
shouldForceGhcColorFlag = do
  canDoColor <- (>= mkVersion [8, 2, 1]) . getGhcVersion
            <$> view actualCompilerVersionL
  shouldDoColor <- view useColorL
  pure $ canDoColor && shouldDoColor

appropriateGhcColorFlag ::
     (HasEnvConfig env, HasRunner env)
  => RIO env (Maybe String)
appropriateGhcColorFlag = f <$> shouldForceGhcColorFlag
 where
  f True = Just ghcColorForceFlag
  f False = Nothing

-- | Directory containing snapshots
snapshotsDir ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => m (Path Abs Dir)
snapshotsDir = do
  root <- view stackRootL
  platform <- platformGhcRelDir
  pure $ root </> relDirSnapshots </> platform

-- | Installation root for dependencies
installationRootDeps :: HasEnvConfig env => RIO env (Path Abs Dir)
installationRootDeps = do
  root <- view stackRootL
  -- TODO: also useShaPathOnWindows here, once #1173 is resolved.
  psc <- platformSnapAndCompilerRel
  pure $ root </> relDirSnapshots </> psc

-- | Installation root for locals
installationRootLocal :: HasEnvConfig env => RIO env (Path Abs Dir)
installationRootLocal = do
  workDir <- getProjectWorkDir
  psc <- useShaPathOnWindows =<< platformSnapAndCompilerRel
  pure $ workDir </> relDirInstall </> psc

-- | Get the hoogle database path.
hoogleDatabasePath :: HasEnvConfig env => RIO env (Path Abs File)
hoogleDatabasePath = do
  dir <- hoogleRoot
  pure (dir </> relFileDatabaseHoo)

-- | Path for platform followed by snapshot name followed by compiler
-- name.
platformSnapAndCompilerRel :: HasEnvConfig env => RIO env (Path Rel Dir)
platformSnapAndCompilerRel = do
  platform <- platformGhcRelDir
  smh <- view $ envConfigL.to envConfigSourceMapHash
  name <- smRelDir smh
  ghc <- compilerVersionDir
  useShaPathOnWindows (platform </> name </> ghc)

-- | Relative directory for the platform and GHC identifier
platformGhcRelDir ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => m (Path Rel Dir)
platformGhcRelDir = do
  cp <- view compilerPathsL
  let cbSuffix = compilerBuildSuffix $ cpBuild cp
  verOnly <- platformGhcVerOnlyRelDirStr
  parseRelDir (mconcat [ verOnly, cbSuffix ])

-- | Installation root for compiler tools
bindirCompilerTools ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => m (Path Abs Dir)
bindirCompilerTools = do
  config <- view configL
  platform <- platformGhcRelDir
  compilerVersion <- view actualCompilerVersionL
  compiler <- parseRelDir $ compilerVersionString compilerVersion
  pure $
    view stackRootL config </>
    relDirCompilerTools </>
    platform </>
    compiler </>
    bindirSuffix

-- | Hoogle directory.
hoogleRoot :: HasEnvConfig env => RIO env (Path Abs Dir)
hoogleRoot = do
  workDir <- getProjectWorkDir
  psc <- useShaPathOnWindows =<< platformSnapAndCompilerRel
  pure $ workDir </> relDirHoogle </> psc

compilerVersionDir ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => m (Path Rel Dir)
compilerVersionDir = do
  compilerVersion <- view actualCompilerVersionL
  parseRelDir $ case compilerVersion of
    ACGhc version -> versionString version
    ACGhcGit {} -> compilerVersionString compilerVersion

-- | Package database for installing dependencies into
packageDatabaseDeps :: HasEnvConfig env => RIO env (Path Abs Dir)
packageDatabaseDeps = do
  root <- installationRootDeps
  pure $ root </> relDirPkgdb

-- | Package database for installing local packages into
packageDatabaseLocal :: HasEnvConfig env => RIO env (Path Abs Dir)
packageDatabaseLocal = do
  root <- installationRootLocal
  pure $ root </> relDirPkgdb

-- | Extra package databases
packageDatabaseExtra ::
     (HasEnvConfig env, MonadReader env m)
  => m [Path Abs Dir]
packageDatabaseExtra = view $ buildConfigL.to bcExtraPackageDBs

-- | Where HPC reports and tix files get stored.
hpcReportDir :: HasEnvConfig env => RIO env (Path Abs Dir)
hpcReportDir = do
  root <- installationRootLocal
  pure $ root </> relDirHpc

-- | Get the extra bin directories (for the PATH). Puts more local first
--
-- Bool indicates whether or not to include the locals
extraBinDirs :: HasEnvConfig env => RIO env (Bool -> [Path Abs Dir])
extraBinDirs = do
  deps <- installationRootDeps
  local' <- installationRootLocal
  tools <- bindirCompilerTools
  pure $ \locals -> if locals
    then [local' </> bindirSuffix, deps </> bindirSuffix, tools]
    else [deps </> bindirSuffix, tools]

-- | The version of the compiler which will actually be used. May be different
-- than that specified in the 'SnapshotDef' and returned by
-- 'wantedCompilerVersionL'.
actualCompilerVersionL :: HasSourceMap env => SimpleGetter env ActualCompiler
actualCompilerVersionL = sourceMapL.to smCompiler

-- | Relative directory for the platform and GHC identifier without GHC bindist
-- build
platformGhcVerOnlyRelDir ::
     (HasGHCVariant env, HasPlatform env, MonadReader env m, MonadThrow m)
  => m (Path Rel Dir)
platformGhcVerOnlyRelDir =
  parseRelDir =<< platformGhcVerOnlyRelDirStr

-- | Relative directory for the platform and GHC identifier without GHC bindist
-- build (before parsing into a Path)
platformGhcVerOnlyRelDirStr ::
     (HasGHCVariant env, HasPlatform env, MonadReader env m)
  => m FilePath
platformGhcVerOnlyRelDirStr = do
  platform <- view platformL
  platformVariant <- view platformVariantL
  ghcVariant <- view ghcVariantL
  pure $ mconcat [ Distribution.Text.display platform
                   , platformVariantSuffix platformVariant
                   , ghcVariantSuffix ghcVariant ]
