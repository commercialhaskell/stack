{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Stack.Types.Package
  ( BioInput (..)
  , BuildInfoOpts (..)
  , ExeName (..)
  , FileCacheInfo (..)
  , InstallLocation (..)
  , Installed (..)
  , InstalledLibraryInfo (..)
  , InstalledPackageLocation (..)
  , LocalPackage (..)
  , MemoizedWith (..)
  , Package (..)
  , PackageConfig (..)
  , PackageDatabase (..)
  , PackageDbVariety (..)
  , PackageException (..)
  , PackageSource (..)
  , dotCabalCFilePath
  , dotCabalGetPath
  , dotCabalMain
  , dotCabalMainPath
  , dotCabalModule
  , dotCabalModulePath
  , installedMapGhcPkgId
  , installedPackageToGhcPkgId
  , lpFiles
  , lpFilesForComponents
  , memoizeRefWith
  , packageDefinedFlags
  , packageIdentifier
  , psVersion
  , runMemoizedWith
  , simpleInstalledLib
  , toCabalMungedPackageName
  , toPackageDbVariety
  ) where

import           Data.Aeson
                   ( ToJSON (..), FromJSON (..), (.=), (.:), object, withObject
                   )
import qualified Data.Map as M
import qualified Data.Set as Set
import           Distribution.CabalSpecVersion
import           Distribution.Parsec ( PError (..), PWarning (..), showPos )
import qualified Distribution.SPDX.License as SPDX
import           Distribution.License ( License )
import           Distribution.ModuleName ( ModuleName )
import           Distribution.PackageDescription ( BuildType )
import           Distribution.System ( Platform (..) )
import           Distribution.Types.MungedPackageName
                   ( encodeCompatPackageName )
import qualified RIO.Text as T
import           Stack.Prelude
import           Stack.Types.CompCollection ( CompCollection )
import           Stack.Types.Compiler ( ActualCompiler )
import           Stack.Types.Component
                   ( StackBenchmark, StackBuildInfo, StackExecutable
                   , StackForeignLibrary, StackLibrary, StackTestSuite
                   , StackUnqualCompName
                   )
import           Stack.Types.ComponentUtils (toCabalName)
import           Stack.Types.Dependency ( DepValue )
import           Stack.Types.EnvConfig ( EnvConfig, HasEnvConfig (..) )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.Installed
                   ( InstallLocation (..), InstallMap, Installed (..)
                   , InstalledLibraryInfo (..), InstalledMap
                   , InstalledPackageLocation (..), PackageDatabase (..)
                   , PackageDbVariety(..), simpleInstalledLib
                   , toPackageDbVariety
                   )
import           Stack.Types.NamedComponent ( NamedComponent )
import           Stack.Types.PackageFile
                   ( DotCabalDescriptor (..), DotCabalPath (..)
                   , StackPackageFile
                   )
import           Stack.Types.SourceMap ( CommonPackage, FromSnapshot )

-- | Type representing exceptions thrown by functions exported by the
-- "Stack.Package" module.
data PackageException
  = PackageInvalidCabalFile
      !(Either PackageIdentifierRevision (Path Abs File))
      !(Maybe Version)
      ![PError]
      ![PWarning]
  | MismatchedCabalIdentifier !PackageIdentifierRevision !PackageIdentifier
  | CabalFileNameParseFail FilePath
  | CabalFileNameInvalidPackageName FilePath
  | ComponentNotParsedBug String
  deriving (Show, Typeable)

instance Exception PackageException where
  displayException (PackageInvalidCabalFile loc _mversion errs warnings) = concat
    [ "Error: [S-8072]\n"
    , "Unable to parse Cabal file "
    , case loc of
        Left pir -> "for " ++ T.unpack (utf8BuilderToText (display pir))
        Right fp -> toFilePath fp
    {-
     Not actually needed, the errors will indicate if a newer version exists.
     Also, it seems that this is set to Just the version even if we support it.
    , case mversion of
        Nothing -> ""
        Just version -> "\nRequires newer Cabal file parser version: " ++
                        versionString version
    -}
    , "\n\n"
    , unlines $ map
        (\(PError pos msg) -> concat
            [ "- "
            , showPos pos
            , ": "
            , msg
            ])
        errs
    , unlines $ map
        (\(PWarning _ pos msg) -> concat
            [ "- "
            , showPos pos
            , ": "
            , msg
            ])
        warnings
    ]
  displayException (MismatchedCabalIdentifier pir ident) = concat
    [ "Error: [S-5394]\n"
    , "Mismatched package identifier."
    , "\nFound:    "
    , packageIdentifierString ident
    , "\nExpected: "
    , T.unpack $ utf8BuilderToText $ display pir
    ]
  displayException (CabalFileNameParseFail fp) = concat
    [ "Error: [S-2203]\n"
    , "Invalid file path for Cabal file, must have a .cabal extension: "
    , fp
    ]
  displayException (CabalFileNameInvalidPackageName fp) = concat
    [ "Error: [S-8854]\n"
    , "Cabal file names must use valid package names followed by a .cabal \
      \extension, the following is invalid: "
    , fp
    ]
  displayException (ComponentNotParsedBug name) = bugReport "[S-4623]"
    (  "Component names should always parse as directory names. The component \
       \name without a directory is '"
    <> name
    <> "'."
    )

-- | Name of an executable.
newtype ExeName
  = ExeName { exeName :: Text }
  deriving (Data, Eq, Generic, Hashable, IsString, NFData, Ord, Show, Typeable)

-- | Some package info.
data Package = Package
  { name :: !PackageName
    -- ^ Name of the package.
  , version :: !Version
    -- ^ Version of the package
  , license :: !(Either SPDX.License License)
    -- ^ The license the package was released under.
  , ghcOptions :: ![Text]
    -- ^ Ghc options used on package.
  , cabalConfigOpts :: ![Text]
    -- ^ Additional options passed to ./Setup.hs configure
  , flags :: !(Map FlagName Bool)
    -- ^ Flags used on package.
  , defaultFlags :: !(Map FlagName Bool)
    -- ^ Defaults for unspecified flags.
  , library :: !(Maybe StackLibrary)
    -- ^ Does the package have a buildable main library stanza?
  , subLibraries :: !(CompCollection StackLibrary)
    -- ^ The sub-libraries of the package.
  , foreignLibraries :: !(CompCollection StackForeignLibrary)
    -- ^ The foreign libraries of the package.
  , testSuites :: !(CompCollection StackTestSuite)
    -- ^ The test suites of the package.
  , benchmarks :: !(CompCollection StackBenchmark)
    -- ^ The benchmarks of the package.
  , executables :: !(CompCollection StackExecutable)
    -- ^ The executables of the package.
  , buildType :: !BuildType
    -- ^ Package build-type.
  , setupDeps :: !(Maybe (Map PackageName DepValue))
    -- ^ If present: custom-setup dependencies
  , cabalSpec :: !CabalSpecVersion
    -- ^ Cabal spec range
  , file :: StackPackageFile
    -- ^ The Cabal sourced files related to the package at the package level
    -- The components may have file information in their own types
  , testEnabled :: Bool
    -- ^ This is a requirement because when tests are not enabled, Stack's
    -- package dependencies should ignore test dependencies. Directly set from
    -- 'packageConfigEnableTests'.
  , benchmarkEnabled :: Bool
    -- ^ This is a requirement because when benchmark are not enabled, Stack's
    -- package dependencies should ignore benchmark dependencies. Directly set
    -- from 'packageConfigEnableBenchmarks'.
  }
  deriving (Show, Typeable)

packageIdentifier :: Package -> PackageIdentifier
packageIdentifier p = PackageIdentifier p.name p.version

packageDefinedFlags :: Package -> Set FlagName
packageDefinedFlags = M.keysSet . (.defaultFlags)

-- | GHC options based on cabal information and ghc-options.
data BuildInfoOpts = BuildInfoOpts
  { opts :: [String]
  , oneWordOpts :: [String]
  , packageFlags :: [String]
    -- ^ These options can safely have 'nubOrd' applied to them, as there are no
    -- multi-word options (see
    -- https://github.com/commercialhaskell/stack/issues/1255)
  , cabalMacros :: Path Abs File
  }
  deriving Show

-- | Package build configuration
data PackageConfig = PackageConfig
  { enableTests :: !Bool
    -- ^ Are tests enabled?
  , enableBenchmarks :: !Bool
    -- ^ Are benchmarks enabled?
  , flags :: !(Map FlagName Bool)
    -- ^ Configured flags.
  , ghcOptions :: ![Text]
    -- ^ Configured ghc options.
  , cabalConfigOpts :: ![Text]
    -- ^ ./Setup.hs configure options
  , compilerVersion :: ActualCompiler
    -- ^ GHC version
  , platform :: !Platform
    -- ^ host platform
  }
 deriving (Show, Typeable)

-- | Compares the package name.
instance Ord Package where
  compare = on compare (.name)

-- | Compares the package name.
instance Eq Package where
  (==) = on (==) (.name)

-- | Where the package's source is located: local directory or package index
data PackageSource
  = PSFilePath LocalPackage
    -- ^ Package which exist on the filesystem
  | PSRemote PackageLocationImmutable Version FromSnapshot CommonPackage
    -- ^ Package which is downloaded remotely.

instance Show PackageSource where
  show (PSFilePath lp) = concat ["PSFilePath (", show lp, ")"]
  show (PSRemote pli v fromSnapshot _) =
    concat
      [ "PSRemote"
      , "(", show pli, ")"
      , "(", show v, ")"
      , show fromSnapshot
      , "<CommonPackage>"
      ]

psVersion :: PackageSource -> Version
psVersion (PSFilePath lp) = lp.package.version
psVersion (PSRemote _ v _ _) = v

-- | Information on a locally available package of source code.
data LocalPackage = LocalPackage
  { package       :: !Package
     -- ^ The @Package@ info itself, after resolution with package flags, with
     -- tests and benchmarks disabled
  , components    :: !(Set NamedComponent)
    -- ^ Components to build, not including the library component.
  , unbuildable   :: !(Set NamedComponent)
    -- ^ Components explicitly requested for build, that are marked
    -- "buildable: false".
  , wanted        :: !Bool -- FIXME Should completely drop this "wanted"
                             -- terminology, it's unclear
    -- ^ Whether this package is wanted as a target.
  , testBench     :: !(Maybe Package)
    -- ^ This stores the 'Package' with tests and benchmarks enabled, if either
    -- is asked for by the user.
  , cabalFP     :: !(Path Abs File)
    -- ^ Absolute path to the Cabal file.
  , buildHaddocks :: !Bool
    -- ^ Is Haddock documentation being built for this package?
  , forceDirty    :: !Bool
  , dirtyFiles    :: !(MemoizedWith EnvConfig (Maybe (Set FilePath)))
    -- ^ Nothing == not dirty, Just == dirty. Note that the Set may be empty if
    -- we forced the build to treat packages as dirty. Also, the Set may not
    -- include all modified files.
  , newBuildCaches :: !( MemoizedWith
                             EnvConfig
                             (Map NamedComponent (Map FilePath FileCacheInfo))
                         )
    -- ^ current state of the files
  , componentFiles :: !( MemoizedWith
                             EnvConfig
                             (Map NamedComponent (Set (Path Abs File)))
                         )
    -- ^ all files used by this package
  }
  deriving Show

newtype MemoizedWith env a
  = MemoizedWith { memoizedWith :: RIO env a }
  deriving (Applicative, Functor, Monad)

memoizeRefWith :: MonadIO m => RIO env a -> m (MemoizedWith env a)
memoizeRefWith action = do
  ref <- newIORef Nothing
  pure $ MemoizedWith $ do
    mres <- readIORef ref
    res <-
      case mres of
        Just res -> pure res
        Nothing -> do
          res <- tryAny action
          writeIORef ref $ Just res
          pure res
    either throwIO pure res

runMemoizedWith ::
     (HasEnvConfig env, MonadReader env m, MonadIO m)
  => MemoizedWith EnvConfig a
  -> m a
runMemoizedWith (MemoizedWith action) = do
  envConfig <- view envConfigL
  runRIO envConfig action

instance Show (MemoizedWith env a) where
  show _ = "<<MemoizedWith>>"

lpFiles :: HasEnvConfig env => LocalPackage -> RIO env (Set.Set (Path Abs File))
lpFiles = runMemoizedWith . fmap (Set.unions . M.elems) . (.componentFiles)

lpFilesForComponents :: HasEnvConfig env
                     => Set NamedComponent
                     -> LocalPackage
                     -> RIO env (Set.Set (Path Abs File))
lpFilesForComponents components lp = runMemoizedWith $ do
  componentFiles <- lp.componentFiles
  pure $ mconcat (M.elems (M.restrictKeys componentFiles components))

newtype FileCacheInfo = FileCacheInfo
  { hash :: SHA256
  }
  deriving (Eq, Generic, Show, Typeable)

instance NFData FileCacheInfo

-- Provided for storing the BuildCache values in a file. But maybe JSON/YAML
-- isn't the right choice here, worth considering.
instance ToJSON FileCacheInfo where
  toJSON (FileCacheInfo hash') = object
    [ "hash" .= hash'
    ]
instance FromJSON FileCacheInfo where
  parseJSON = withObject "FileCacheInfo" $ \o -> FileCacheInfo
    <$> o .: "hash"

-- | Maybe get the module name from the .cabal descriptor.
dotCabalModule :: DotCabalDescriptor -> Maybe ModuleName
dotCabalModule (DotCabalModule m) = Just m
dotCabalModule _ = Nothing

-- | Maybe get the main name from the .cabal descriptor.
dotCabalMain :: DotCabalDescriptor -> Maybe FilePath
dotCabalMain (DotCabalMain m) = Just m
dotCabalMain _ = Nothing

-- | Get the module path.
dotCabalModulePath :: DotCabalPath -> Maybe (Path Abs File)
dotCabalModulePath (DotCabalModulePath fp) = Just fp
dotCabalModulePath _ = Nothing

-- | Get the main path.
dotCabalMainPath :: DotCabalPath -> Maybe (Path Abs File)
dotCabalMainPath (DotCabalMainPath fp) = Just fp
dotCabalMainPath _ = Nothing

-- | Get the c file path.
dotCabalCFilePath :: DotCabalPath -> Maybe (Path Abs File)
dotCabalCFilePath (DotCabalCFilePath fp) = Just fp
dotCabalCFilePath _ = Nothing

-- | Get the path.
dotCabalGetPath :: DotCabalPath -> Path Abs File
dotCabalGetPath dcp =
  case dcp of
    DotCabalModulePath fp -> fp
    DotCabalMainPath fp -> fp
    DotCabalFilePath fp -> fp
    DotCabalCFilePath fp -> fp

-- | Gathers all the GhcPkgId provided by a library into a map
installedMapGhcPkgId ::
     PackageIdentifier
  -> InstalledLibraryInfo
  -> Map PackageIdentifier GhcPkgId
installedMapGhcPkgId pkgId@(PackageIdentifier pkgName version) installedLib =
  finalMap
 where
  finalMap = M.insert pkgId installedLib.ghcPkgId baseMap
  baseMap =
    M.mapKeysMonotonic
      (toCabalMungedPackageIdentifier pkgName version)
      installedLib.subLib

installedPackageToGhcPkgId ::
     PackageIdentifier
  -> Installed
  -> Map PackageIdentifier GhcPkgId
installedPackageToGhcPkgId ident (Library ident' libInfo) =
  assert (ident == ident') (installedMapGhcPkgId ident libInfo)
installedPackageToGhcPkgId _ (Executable _) = mempty

-- | Creates a 'MungedPackageName' identifier.
toCabalMungedPackageIdentifier ::
     PackageName
  -> Version
  -> StackUnqualCompName
  -> PackageIdentifier
toCabalMungedPackageIdentifier pkgName version = flip PackageIdentifier version
  . encodeCompatPackageName . toCabalMungedPackageName pkgName

toCabalMungedPackageName ::
     PackageName
  -> StackUnqualCompName
  -> MungedPackageName
toCabalMungedPackageName pkgName =
  MungedPackageName pkgName . LSubLibName . toCabalName

-- | Type representing inputs to 'Stack.Package.generateBuildInfoOpts'.
data BioInput = BioInput
  { installMap :: !InstallMap
  , installedMap :: !InstalledMap
  , cabalDir :: !(Path Abs Dir)
  , distDir :: !(Path Abs Dir)
  , omitPackages :: ![PackageName]
  , addPackages :: ![PackageName]
  , buildInfo :: !StackBuildInfo
  , dotCabalPaths :: ![DotCabalPath]
  , configLibDirs :: ![FilePath]
  , configIncludeDirs :: ![FilePath]
  , componentName :: !NamedComponent
  , cabalVersion :: !Version
  }
