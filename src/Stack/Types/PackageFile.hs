{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DataKinds                  #-}

-- | The facility for retrieving all files from the main Stack
-- 'Stack.Types.Package' type. This was moved into its own module to allow
-- component-level file-gathering without circular dependency at the Package
-- level.
module Stack.Types.PackageFile
  ( GetPackageFileContext (..)
  , DotCabalPath (..)
  , DotCabalDescriptor (..)
  , GetPackageFiles (..)
  , PackageWarning (..)
  ) where

import           Distribution.ModuleName ( ModuleName )
import           RIO.Process ( HasProcessContext (..) )
import           Stack.Prelude
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig (..) )
import           Stack.Types.Config ( HasConfig (..) )
import           Stack.Types.EnvConfig ( HasEnvConfig )
import           Stack.Types.GHCVariant ( HasGHCVariant (..) )
import           Stack.Types.NamedComponent ( NamedComponent )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Runner ( HasRunner (..) )

data GetPackageFileContext = GetPackageFileContext
  { ctxFile :: !(Path Abs File)
  , ctxDistDir :: !(Path Abs Dir)
  , ctxBuildConfig :: !BuildConfig
  , ctxCabalVer :: !Version
  }

instance HasPlatform GetPackageFileContext where
  platformL = configL.platformL
  {-# INLINE platformL #-}
  platformVariantL = configL.platformVariantL
  {-# INLINE platformVariantL #-}

instance HasGHCVariant GetPackageFileContext where
  ghcVariantL = configL.ghcVariantL
  {-# INLINE ghcVariantL #-}

instance HasLogFunc GetPackageFileContext where
  logFuncL = configL.logFuncL

instance HasRunner GetPackageFileContext where
  runnerL = configL.runnerL

instance HasStylesUpdate GetPackageFileContext where
  stylesUpdateL = runnerL.stylesUpdateL

instance HasTerm GetPackageFileContext where
  useColorL = runnerL.useColorL
  termWidthL = runnerL.termWidthL

instance HasConfig GetPackageFileContext where
  configL = buildConfigL.lens bcConfig (\x y -> x { bcConfig = y })
  {-# INLINE configL #-}

instance HasBuildConfig GetPackageFileContext where
  buildConfigL = lens ctxBuildConfig (\x y -> x { ctxBuildConfig = y })

instance HasPantryConfig GetPackageFileContext where
  pantryConfigL = configL.pantryConfigL

instance HasProcessContext GetPackageFileContext where
  processContextL = configL.processContextL

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

-- | Files that the package depends on, relative to package directory.
-- Argument is the location of the Cabal file
newtype GetPackageFiles = GetPackageFiles
  { getPackageFiles :: forall env. HasEnvConfig env
                    => Path Abs File
                    -> RIO env
                         ( Map NamedComponent (Map ModuleName (Path Abs File))
                         , Map NamedComponent [DotCabalPath]
                         , Set (Path Abs File)
                         , [PackageWarning]
                         )
  }
instance Show GetPackageFiles where
  show _ = "<GetPackageFiles>"

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
