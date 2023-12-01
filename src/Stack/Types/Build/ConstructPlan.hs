{-# LANGUAGE NoImplicitPrelude #-}

-- | A module providing types and related helper functions used in module
-- "Stack.Build.ConstructPlan".
module Stack.Types.Build.ConstructPlan
  ( PackageInfo (..)
  , CombinedMap
  , M
  , W (..)
  , AddDepRes (..)
  , toTask
  , adrVersion
  , adrHasLibrary
  , Ctx (..)
  , UnregisterState (..)
  , ToolWarning (..)
  ) where

import           Generics.Deriving.Monoid ( mappenddefault, memptydefault )
import           RIO.Process ( HasProcessContext (..) )
import           RIO.State ( StateT )
import           RIO.Writer ( WriterT (..) )
import           Stack.Package ( hasBuildableMainLibrary )
import           Stack.Prelude hiding ( loadPackage )
import           Stack.Types.Build
                    ( Task (..), TaskType (..), taskProvides )
import           Stack.Types.Build.Exception ( ConstructPlanException )
import           Stack.Types.BuildConfig
                   ( BuildConfig (..), HasBuildConfig(..) )
import           Stack.Types.CompilerPaths ( HasCompiler (..) )
import           Stack.Types.Config ( HasConfig (..) )
import           Stack.Types.ConfigureOpts ( BaseConfigOpts )
import           Stack.Types.Curator ( Curator )
import           Stack.Types.DumpPackage ( DumpPackage )
import           Stack.Types.EnvConfig
                   ( EnvConfig (..), HasEnvConfig (..), HasSourceMap (..) )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.GHCVariant ( HasGHCVariant (..) )
import           Stack.Types.Package
                   ( ExeName (..), InstallLocation, Installed (..)
                   , LocalPackage (..), Package (..), PackageSource (..)
                   , installedVersion
                   )
import           Stack.Types.ParentMap ( ParentMap )
import           Stack.Types.Platform ( HasPlatform (..) )
import           Stack.Types.Runner ( HasRunner (..) )

-- | Type representing information about packages, namely information about
-- whether or not a package is already installed and, unless the package is not
-- to be built (global packages), where its source code is located.
data PackageInfo
  = PIOnlyInstalled InstallLocation Installed
    -- ^ This indicates that the package is already installed, and that we
    -- shouldn't build it from source. This is only the case for global
    -- packages.
  | PIOnlySource PackageSource
    -- ^ This indicates that the package isn't installed, and we know where to
    -- find its source.
  | PIBoth PackageSource Installed
    -- ^ This indicates that the package is installed and we know where to find
    -- its source. We may want to reinstall from source.
  deriving Show

-- | A type synonym representing dictionaries of package names, and combined
-- information about the package in respect of whether or not it is already
-- installed and, unless the package is not to be built (global packages), where
-- its source code is located.
type CombinedMap = Map PackageName PackageInfo

-- | Type synonym representing values used during the construction of a build
-- plan. The type is an instance of 'Monad', hence its name.
type M =
  WriterT
    W
    -- ^ The output to be collected
    ( StateT
        (Map PackageName (Either ConstructPlanException AddDepRes))
        -- ^ Library map
        (RIO Ctx)
    )

-- | Type representing values used as the output to be collected during the
-- construction of a build plan.
data W = W
  { wFinals :: !(Map PackageName (Either ConstructPlanException Task))
    -- ^ A dictionary of package names, and either a final task to perform when
    -- building the package or an exception.
  , wInstall :: !(Map Text InstallLocation)
    -- ^ A dictionary of executables to be installed, and location where the
    -- executable's binary is placed.
  , wDirty :: !(Map PackageName Text)
    -- ^ A dictionary of local packages, and the reason why the local package is
    -- considered dirty.
  , wWarnings :: !([StyleDoc] -> [StyleDoc])
    -- ^ Warnings.
  , wParents :: !ParentMap
    -- ^ A dictionary of package names, and a list of pairs of the identifier
    -- of a package depending on the package and the version range specified for
    -- the dependency by that package. Used in the reporting of failure to
    -- construct a build plan.
  }
  deriving Generic

instance Semigroup W where
  (<>) = mappenddefault

instance Monoid W where
  mempty = memptydefault
  mappend = (<>)

-- | Type representing results of 'addDep'.
data AddDepRes
  = ADRToInstall Task
    -- ^ A task must be performed to provide the package name.
  | ADRFound InstallLocation Installed
    -- ^ An existing installation provides the package name.
  deriving Show

toTask :: AddDepRes -> Maybe Task
toTask (ADRToInstall task) = Just task
toTask (ADRFound _ _) = Nothing

adrVersion :: AddDepRes -> Version
adrVersion (ADRToInstall task) = pkgVersion $ taskProvides task
adrVersion (ADRFound _ installed) = installedVersion installed

adrHasLibrary :: AddDepRes -> Bool
adrHasLibrary (ADRToInstall task) = case taskType task of
  TTLocalMutable lp -> packageHasLibrary $ lpPackage lp
  TTRemotePackage _ p _ -> packageHasLibrary p
 where
  -- make sure we consider sub-libraries as libraries too
  packageHasLibrary :: Package -> Bool
  packageHasLibrary p =
    hasBuildableMainLibrary p || not (null (packageSubLibraries p))
adrHasLibrary (ADRFound _ Library{}) = True
adrHasLibrary (ADRFound _ Executable{}) = False

-- | Type representing values used as the environment to be read from during the
-- construction of a build plan (the \'context\').
data Ctx = Ctx
  { baseConfigOpts :: !BaseConfigOpts
    -- ^ Basic information used to determine configure options
  , loadPackage    :: !(  PackageLocationImmutable
                       -> Map FlagName Bool
                       -> [Text]
                          -- ^ GHC options.
                       -> [Text]
                          -- ^ Cabal configure options.
                       -> M Package
                       )
  , combinedMap    :: !CombinedMap
    -- ^ A dictionary of package names, and combined information about the
    -- package in respect of whether or not it is already installed and, unless
    -- the package is not to be built (global packages), where its source code
    -- is located.
  , ctxEnvConfig   :: !EnvConfig
    -- ^ Configuration after the environment has been setup.
  , callStack      :: ![PackageName]
  , wanted         :: !(Set PackageName)
  , localNames     :: !(Set PackageName)
  , mcurator       :: !(Maybe Curator)
  , pathEnvVar     :: !Text
  }

instance HasPlatform Ctx where
  platformL = configL.platformL
  {-# INLINE platformL #-}
  platformVariantL = configL.platformVariantL
  {-# INLINE platformVariantL #-}

instance HasGHCVariant Ctx where
  ghcVariantL = configL.ghcVariantL
  {-# INLINE ghcVariantL #-}

instance HasLogFunc Ctx where
  logFuncL = configL.logFuncL

instance HasRunner Ctx where
  runnerL = configL.runnerL

instance HasStylesUpdate Ctx where
  stylesUpdateL = runnerL.stylesUpdateL

instance HasTerm Ctx where
  useColorL = runnerL.useColorL
  termWidthL = runnerL.termWidthL

instance HasConfig Ctx where
  configL = buildConfigL.lens bcConfig (\x y -> x { bcConfig = y })
  {-# INLINE configL #-}

instance HasPantryConfig Ctx where
  pantryConfigL = configL.pantryConfigL

instance HasProcessContext Ctx where
  processContextL = configL.processContextL

instance HasBuildConfig Ctx where
  buildConfigL = envConfigL.lens
    envConfigBuildConfig
    (\x y -> x { envConfigBuildConfig = y })

instance HasSourceMap Ctx where
  sourceMapL = envConfigL.sourceMapL

instance HasCompiler Ctx where
  compilerPathsL = envConfigL.compilerPathsL

instance HasEnvConfig Ctx where
  envConfigL = lens ctxEnvConfig (\x y -> x { ctxEnvConfig = y })

-- | State to be maintained during the calculation of local packages
-- to unregister.
data UnregisterState = UnregisterState
  { usToUnregister :: !(Map GhcPkgId (PackageIdentifier, Text))
  , usKeep :: ![DumpPackage]
  , usAnyAdded :: !Bool
  }

-- | Warn about tools in the snapshot definition. States the tool name
-- expected and the package name using it.
data ToolWarning
  = ToolWarning ExeName PackageName
  deriving Show
