{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}

module Stack.Types.BuildConfig
  ( BuildConfig (..)
  , HasBuildConfig (..)
  , stackYamlL
  , projectRootL
  , getProjectWorkDir
  , wantedCompilerVersionL
  ) where

import           Path ( (</>), parent )
import           RIO.Process ( HasProcessContext (..) )
import           Stack.Prelude
import           Stack.Types.Config ( Config, HasConfig (..), workDirL )
import           Stack.Types.Curator ( Curator )
import           Stack.Types.GHCVariant ( HasGHCVariant (..) )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Runner ( HasRunner (..) )
import           Stack.Types.SourceMap ( SMWanted (..) )
import           Stack.Types.Storage ( ProjectStorage )

-- | A superset of 'Config' adding information on how to build code. The reason
-- for this breakdown is because we will need some of the information from
-- 'Config' in order to determine the values here.
--
-- These are the components which know nothing about local configuration.
data BuildConfig = BuildConfig
  { bcConfig     :: !Config
  , bcSMWanted :: !SMWanted
  , bcExtraPackageDBs :: ![Path Abs Dir]
    -- ^ Extra package databases
  , bcStackYaml  :: !(Path Abs File)
    -- ^ Location of the stack.yaml file.
    --
    -- Note: if the STACK_YAML environment variable is used, this may be
    -- different from projectRootL </> "stack.yaml" if a different file
    -- name is used.
  , bcProjectStorage :: !ProjectStorage
  -- ^ Database connection pool for project Stack database
  , bcCurator :: !(Maybe Curator)
  }

instance HasPlatform BuildConfig where
  platformL = configL.platformL
  {-# INLINE platformL #-}
  platformVariantL = configL.platformVariantL
  {-# INLINE platformVariantL #-}

instance HasGHCVariant BuildConfig where
  ghcVariantL = configL.ghcVariantL
  {-# INLINE ghcVariantL #-}

instance HasProcessContext BuildConfig where
  processContextL = configL.processContextL

instance HasPantryConfig BuildConfig where
  pantryConfigL = configL.pantryConfigL

instance HasConfig BuildConfig where
  configL = lens bcConfig (\x y -> x { bcConfig = y })

instance HasRunner BuildConfig where
  runnerL = configL.runnerL

instance HasLogFunc BuildConfig where
  logFuncL = runnerL.logFuncL

instance HasStylesUpdate BuildConfig where
  stylesUpdateL = runnerL.stylesUpdateL

instance HasTerm BuildConfig where
  useColorL = runnerL.useColorL
  termWidthL = runnerL.termWidthL

class HasConfig env => HasBuildConfig env where
  buildConfigL :: Lens' env BuildConfig

instance HasBuildConfig BuildConfig where
  buildConfigL = id
  {-# INLINE buildConfigL #-}

stackYamlL :: HasBuildConfig env => Lens' env (Path Abs File)
stackYamlL = buildConfigL.lens bcStackYaml (\x y -> x { bcStackYaml = y })

-- | Directory containing the project's stack.yaml file
projectRootL :: HasBuildConfig env => Getting r env (Path Abs Dir)
projectRootL = stackYamlL.to parent

-- | Per-project work dir
getProjectWorkDir :: (HasBuildConfig env, MonadReader env m) => m (Path Abs Dir)
getProjectWorkDir = do
  root    <- view projectRootL
  workDir <- view workDirL
  pure (root </> workDir)

-- | The compiler specified by the @SnapshotDef@. This may be different from the
-- actual compiler used!
wantedCompilerVersionL :: HasBuildConfig s => Getting r s WantedCompiler
wantedCompilerVersionL = buildConfigL.to (smwCompiler . bcSMWanted)
