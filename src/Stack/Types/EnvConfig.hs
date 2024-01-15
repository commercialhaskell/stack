{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
  , useShaPathOnWindows
  , shaPathForBytes
  ) where

import           Crypto.Hash ( SHA1 (..), hashWith )
import qualified Data.ByteArray.Encoding as Mem ( Base(Base16), convertToBase )
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import qualified Distribution.Text ( display )
import           Distribution.Version ( mkVersion )
import           Path
                   ( (</>), parseAbsDir, parseAbsFile, parseRelDir
                   , parseRelFile
                   )
import           RIO.Process ( HasProcessContext (..) )
import           Stack.Constants
                   ( bindirSuffix, ghcColorForceFlag, osIsWindows, relDirCompilerTools
                   , relDirHoogle, relDirHpc, relDirInstall, relDirPkgdb
                   , relDirSnapshots, relFileDatabaseHoo
                   )
import           Stack.Prelude
import           Stack.Types.BuildConfig
                    ( BuildConfig (..), HasBuildConfig (..), getProjectWorkDir )
import           Stack.Types.BuildOptsCLI ( BuildOptsCLI )
import           Stack.Types.Compiler
                   ( ActualCompiler (..), compilerVersionString, getGhcVersion )
import           Stack.Types.CompilerBuild ( compilerBuildSuffix )
import           Stack.Types.CompilerPaths
                   ( CompilerPaths (..), HasCompiler (..) )
import           Stack.Types.Config ( HasConfig (..), stackRootL )
import           Stack.Types.FileDigestCache ( FileDigestCache )
import           Stack.Types.GHCVariant ( HasGHCVariant (..), ghcVariantSuffix )
import           Stack.Types.Platform
                   ( HasPlatform (..), platformVariantSuffix )
import           Stack.Types.Runner ( HasRunner (..) )
import           Stack.Types.SourceMap
                   ( SourceMap (..), SourceMapHash, smRelDir )

-- | Configuration after the environment has been setup.
data EnvConfig = EnvConfig
  { buildConfig :: !BuildConfig
  , buildOptsCLI :: !BuildOptsCLI
  , fileDigestCache :: !FileDigestCache
  , sourceMap :: !SourceMap
  , sourceMapHash :: !SourceMapHash
  , compilerPaths :: !CompilerPaths
  }

instance HasConfig EnvConfig where
  configL = buildConfigL . lens (.config) (\x y -> x { config = y })
  {-# INLINE configL #-}

instance HasBuildConfig EnvConfig where
  buildConfigL = envConfigL . lens
    (.buildConfig)
    (\x y -> x { buildConfig = y })

instance HasPlatform EnvConfig where
  platformL = configL . platformL
  {-# INLINE platformL #-}
  platformVariantL = configL . platformVariantL
  {-# INLINE platformVariantL #-}

instance HasGHCVariant EnvConfig where
  ghcVariantL = configL . ghcVariantL
  {-# INLINE ghcVariantL #-}

instance HasProcessContext EnvConfig where
  processContextL = configL . processContextL

instance HasPantryConfig EnvConfig where
  pantryConfigL = configL . pantryConfigL

instance HasCompiler EnvConfig where
  compilerPathsL = to (.compilerPaths)

instance HasRunner EnvConfig where
  runnerL = configL . runnerL

instance HasLogFunc EnvConfig where
  logFuncL = runnerL . logFuncL

instance HasStylesUpdate EnvConfig where
  stylesUpdateL = runnerL . stylesUpdateL

instance HasTerm EnvConfig where
  useColorL = runnerL . useColorL
  termWidthL = runnerL . termWidthL

class (HasBuildConfig env, HasSourceMap env, HasCompiler env) => HasEnvConfig env where
  envConfigL :: Lens' env EnvConfig

instance HasEnvConfig EnvConfig where
  envConfigL = id
  {-# INLINE envConfigL #-}

class HasSourceMap env where
  sourceMapL :: Lens' env SourceMap

instance HasSourceMap EnvConfig where
  sourceMapL = lens (.sourceMap) (\x y -> x { sourceMap = y })

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
  smh <- view $ envConfigL . to (.sourceMapHash)
  name <- smRelDir smh
  ghc <- compilerVersionDir
  useShaPathOnWindows (platform </> name </> ghc)

-- | Relative directory for the platform and GHC identifier
platformGhcRelDir ::
     (HasEnvConfig env, MonadReader env m, MonadThrow m)
  => m (Path Rel Dir)
platformGhcRelDir = do
  cp <- view compilerPathsL
  let cbSuffix = compilerBuildSuffix cp.cpBuild
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
packageDatabaseExtra = view $ buildConfigL . to (.extraPackageDBs)

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
actualCompilerVersionL = sourceMapL . to (.compiler)

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

-- | This is an attempt to shorten Stack paths on Windows to decrease our
-- chances of hitting 260 symbol path limit. The idea is to calculate
-- SHA1 hash of the path used on other architectures, encode with base
-- 16 and take first 8 symbols of it.
useShaPathOnWindows :: MonadThrow m => Path Rel Dir -> m (Path Rel Dir)
useShaPathOnWindows
  | osIsWindows = shaPath
  | otherwise = pure

shaPath :: (IsPath Rel t, MonadThrow m) => Path Rel t -> m (Path Rel t)
shaPath = shaPathForBytes . encodeUtf8 . T.pack . toFilePath

shaPathForBytes :: (IsPath Rel t, MonadThrow m) => ByteString -> m (Path Rel t)
shaPathForBytes
  = parsePath . S8.unpack . S8.take 8
  . Mem.convertToBase Mem.Base16 . hashWith SHA1

-- TODO: Move something like this into the path package. Consider
-- subsuming path-io's 'AnyPath'?
class IsPath b t where
  parsePath :: MonadThrow m => FilePath -> m (Path b t)

instance IsPath Abs Dir where
  parsePath = parseAbsDir

instance IsPath Rel Dir where
  parsePath = parseRelDir

instance IsPath Abs File where
  parsePath = parseAbsFile

instance IsPath Rel File where
  parsePath = parseRelFile
