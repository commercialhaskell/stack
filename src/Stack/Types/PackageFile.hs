{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Stack.Types.PackageFile
License     : BSD-3-Clause

The facility for retrieving all files from the main Stack 'Stack.Types.Package'
type. This was moved into its own module to allow component-level file-gathering
without circular dependency at the Package level.
-}

module Stack.Types.PackageFile
  ( GetPackageFileContext (..)
  , DotCabalPath (..)
  , DotCabalDescriptor (..)
  , PackageWarning (..)
  , StackPackageFile (..)
  , PackageComponentFile (..)
  ) where

import           Distribution.ModuleName ( ModuleName )
import           RIO.Process ( HasProcessContext (..) )
import           Stack.Prelude
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..) )
import           Stack.Types.Config ( HasConfig (..) )
import           Stack.Types.GHCVariant ( HasGHCVariant (..) )
import           Stack.Types.NamedComponent ( NamedComponent )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Runner ( HasRunner (..) )

-- | Type representing environments in which Stack gets all files referenced by
-- a package.
data GetPackageFileContext = GetPackageFileContext
  { file :: !(Path Abs File)
  , distDir :: !(Path Abs Dir)
  , buildConfig :: !BuildConfig
  , cabalVer :: !Version
  }

instance HasPlatform GetPackageFileContext where
  platformL = configL . platformL
  {-# INLINE platformL #-}
  platformVariantL = configL . platformVariantL
  {-# INLINE platformVariantL #-}

instance HasGHCVariant GetPackageFileContext where
  ghcVariantL = configL . ghcVariantL
  {-# INLINE ghcVariantL #-}

instance HasLogFunc GetPackageFileContext where
  logFuncL = configL . logFuncL

instance HasRunner GetPackageFileContext where
  runnerL = configL . runnerL

instance HasStylesUpdate GetPackageFileContext where
  stylesUpdateL = runnerL . stylesUpdateL

instance HasTerm GetPackageFileContext where
  useColorL = runnerL . useColorL
  termWidthL = runnerL . termWidthL

instance HasConfig GetPackageFileContext where
  configL = buildConfigL . lens (.config) (\x y -> x { config = y })
  {-# INLINE configL #-}

instance HasBuildConfig GetPackageFileContext where
  buildConfigL = lens (.buildConfig) (\x y -> x { buildConfig = y })

instance HasPantryConfig GetPackageFileContext where
  pantryConfigL = configL . pantryConfigL

instance HasProcessContext GetPackageFileContext where
  processContextL = configL . processContextL

-- | A path resolved from the Cabal file, which is either main-is or
-- an exposed/internal/referenced module.
data DotCabalPath
  = DotCabalModulePath !(Path Abs File)
  | DotCabalMainPath !(Path Abs File)
  | DotCabalFilePath !(Path Abs File)
  | DotCabalCFilePath !(Path Abs File)
  deriving (Eq, Ord, Show)

-- | A descriptor from a Cabal file indicating one of the following:
--
-- exposed-modules: Foo
-- other-modules: Foo
-- or
-- main-is: Foo.hs
--
data DotCabalDescriptor
  = DotCabalModule !ModuleName
  | DotCabalMain !FilePath
  | DotCabalFile !FilePath
  | DotCabalCFile !FilePath
  deriving (Eq, Ord, Show)

-- | Warning generated when reading a package
data PackageWarning
  = UnlistedModulesWarning NamedComponent [ModuleName]
    -- ^ Modules found that are not listed in Cabal file
  -- TODO: bring this back - see
  -- https://github.com/commercialhaskell/stack/issues/2649
  {-
  | MissingModulesWarning (Path Abs File) (Maybe String) [ModuleName]
    -- ^ Modules not found in file system, which are listed in Cabal file
  -}

-- | This is the information from Cabal we need at the package level to track
-- files.
data StackPackageFile = StackPackageFile
  { extraSrcFiles :: [FilePath]
  , dataDir :: FilePath
  , dataFiles :: [FilePath]
  }
  deriving (Show, Typeable)

-- | Files that the package depends on, relative to package directory.
data PackageComponentFile = PackageComponentFile
  { modulePathMap :: Map NamedComponent (Map ModuleName (Path Abs File))
  , cabalFileMap :: !(Map NamedComponent [DotCabalPath])
  , packageExtraFile :: Set (Path Abs File)
  , warnings :: [PackageWarning]
  }

instance Semigroup PackageComponentFile where
  PackageComponentFile x1 x2 x3 x4 <> PackageComponentFile y1 y2 y3 y4 =
    PackageComponentFile (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4)

instance Monoid PackageComponentFile where
  mempty = PackageComponentFile mempty mempty mempty mempty
