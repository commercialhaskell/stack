{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies        #-}

{-|
Module      : Stack.Types.BuildConfig
License     : BSD-3-Clause
-}

module Stack.Types.BuildConfig
  ( BuildConfig (..)
  , HasBuildConfig (..)
  , configFileL
  , configFileRootL
  , getWorkDir
  , wantedCompilerVersionL
  ) where

import qualified Data.Either.Extra as EE
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
  { config     :: !Config
  , smWanted :: !SMWanted
  , extraPackageDBs :: ![Path Abs Dir]
    -- ^ Extra package databases
  , configFile :: !(Either (Path Abs File) (Path Abs File))
    -- ^ Either (Left) the location of the user-specific global configuration
    -- file or, in most cases, (Right) the location of the project-level
    -- coniguration file (stack.yaml, by default).
    --
    -- Note: if the STACK_YAML environment variable is used, the location of the
    -- project-level configuration file may be different from
    -- projectRootL </> "stack.yaml" if a different file name is used.
  , projectStorage :: !ProjectStorage
  -- ^ Database connection pool for project Stack database
  , curator :: !(Maybe Curator)
  }

instance HasPlatform BuildConfig where
  platformL = configL . platformL
  {-# INLINE platformL #-}
  platformVariantL = configL . platformVariantL
  {-# INLINE platformVariantL #-}

instance HasGHCVariant BuildConfig where
  ghcVariantL = configL . ghcVariantL
  {-# INLINE ghcVariantL #-}

instance HasProcessContext BuildConfig where
  processContextL = configL . processContextL

instance HasPantryConfig BuildConfig where
  pantryConfigL = configL . pantryConfigL

instance HasConfig BuildConfig where
  configL = lens (.config) (\x y -> x { config = y })

instance HasRunner BuildConfig where
  runnerL = configL . runnerL

instance HasLogFunc BuildConfig where
  logFuncL = runnerL . logFuncL

instance HasStylesUpdate BuildConfig where
  stylesUpdateL = runnerL . stylesUpdateL

instance HasTerm BuildConfig where
  useColorL = runnerL . useColorL
  termWidthL = runnerL . termWidthL

class HasConfig env => HasBuildConfig env where
  buildConfigL :: Lens' env BuildConfig

instance HasBuildConfig BuildConfig where
  buildConfigL = id
  {-# INLINE buildConfigL #-}

configFileL ::
     HasBuildConfig env
  => Lens' env (Either (Path Abs File) (Path Abs File))
configFileL = buildConfigL . lens (.configFile) (\x y -> x { configFile = y })

-- | Directory containing the configuration file.
configFileRootL :: HasBuildConfig env => Getting r env (Path Abs Dir)
configFileRootL = configFileL . to EE.fromEither . to parent

-- | Work directory in the directory of the configuration file (global or
-- project-level).
getWorkDir :: (HasBuildConfig env, MonadReader env m) => m (Path Abs Dir)
getWorkDir = do
  configFileRoot <- view configFileRootL
  workDir <- view workDirL
  pure (configFileRoot </> workDir)

-- | The compiler specified by the @SnapshotDef@. This may be different from the
-- actual compiler used!
wantedCompilerVersionL :: HasBuildConfig s => Getting r s WantedCompiler
wantedCompilerVersionL = buildConfigL . to (.smWanted.compiler)
