{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

{-|
Module      : Stack.Types.DotConfig
License     : BSD-3-Clause
-}

module Stack.Types.DotConfig
  ( DotConfig (..)
  ) where

import           RIO.Process ( HasProcessContext (..) )
import           Stack.Prelude hiding ( Display (..), pkgName, loadPackage )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..) )
import           Stack.Types.Config ( HasConfig (..) )
import           Stack.Types.DumpPackage ( DumpPackage (..) )
import           Stack.Types.EnvConfig ( HasSourceMap (..) )
import           Stack.Types.GHCVariant ( HasGHCVariant (..) )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Runner ( HasRunner (..) )
import           Stack.Types.SourceMap ( SourceMap (..) )

-- | Type representing configurations for the creation of a dependency graph.
data DotConfig = DotConfig
  { buildConfig :: !BuildConfig
  , sourceMap :: !SourceMap
  , globalDump :: ![DumpPackage]
  }

instance HasLogFunc DotConfig where
  logFuncL = runnerL . logFuncL

instance HasPantryConfig DotConfig where
  pantryConfigL = configL . pantryConfigL

instance HasTerm DotConfig where
  useColorL = runnerL . useColorL
  termWidthL = runnerL . termWidthL

instance HasStylesUpdate DotConfig where
  stylesUpdateL = runnerL . stylesUpdateL

instance HasGHCVariant DotConfig where
  ghcVariantL = configL . ghcVariantL
  {-# INLINE ghcVariantL #-}

instance HasPlatform DotConfig where
  platformL = configL . platformL
  {-# INLINE platformL #-}
  platformVariantL = configL . platformVariantL
  {-# INLINE platformVariantL #-}

instance HasRunner DotConfig where
  runnerL = configL . runnerL

instance HasProcessContext DotConfig where
  processContextL = runnerL . processContextL

instance HasConfig DotConfig where
  configL = buildConfigL . lens (.config) (\x y -> x { config = y })
  {-# INLINE configL #-}

instance HasBuildConfig DotConfig where
  buildConfigL = lens (.buildConfig) (\x y -> x { buildConfig = y })

instance HasSourceMap DotConfig where
  sourceMapL = lens (.sourceMap) (\x y -> x { sourceMap = y })
