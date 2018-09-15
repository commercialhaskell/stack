{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
module Stack.Types.Package where

import           Stack.Prelude
import qualified RIO.Text as T
import qualified Data.Map as M
import qualified Data.Set as Set
import           Data.IORef.RunOnce
import           Data.Store.Version (VersionConfig)
import           Data.Store.VersionTagged (storeVersionConfig)
import           Distribution.Parsec.Common (PError (..), PWarning (..), showPos)
import qualified Distribution.SPDX.License as SPDX
import           Distribution.License (License)
import           Distribution.ModuleName (ModuleName)
import           Distribution.PackageDescription (TestSuiteInterface, BuildType)
import           Distribution.System (Platform (..))
import           Stack.Types.BuildPlan (ExeName)
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.GhcPkgId
import           Stack.Types.NamedComponent
import           Stack.Types.Version

-- | All exceptions thrown by the library.
data PackageException
  = PackageInvalidCabalFile
      !(Either PackageIdentifierRevision (Path Abs File))
      !(Maybe Version)
      ![PError]
      ![PWarning]
  | MismatchedCabalIdentifier !PackageIdentifierRevision !PackageIdentifier
  deriving Typeable
instance Exception PackageException
instance Show PackageException where
    show (PackageInvalidCabalFile loc _mversion errs warnings) = concat
        [ "Unable to parse cabal file "
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
    show (MismatchedCabalIdentifier pir ident) = concat
        [ "Mismatched package identifier."
        , "\nFound:    "
        , packageIdentifierString ident
        , "\nExpected: "
        , T.unpack $ utf8BuilderToText $ display pir
        ]

-- | Libraries in a package. Since Cabal 2.0, internal libraries are a
-- thing.
data PackageLibraries
  = NoLibraries
  | HasLibraries !(Set Text) -- ^ the foreign library names, sub libraries get built automatically without explicit component name passing
 deriving (Show,Typeable)

-- | Some package info.
data Package =
  Package {packageName :: !PackageName                    -- ^ Name of the package.
          ,packageVersion :: !Version                     -- ^ Version of the package
          ,packageLicense :: !(Either SPDX.License License) -- ^ The license the package was released under.
          ,packageFiles :: !GetPackageFiles               -- ^ Get all files of the package.
          ,packageDeps :: !(Map PackageName DepValue)     -- ^ Packages that the package depends on, both as libraries and build tools.
          ,packageUnknownTools :: !(Set ExeName)          -- ^ Build tools specified in the legacy manner (build-tools:) that failed the hard-coded lookup.
          ,packageAllDeps :: !(Set PackageName)           -- ^ Original dependencies (not sieved).
          ,packageGhcOptions :: ![Text]                   -- ^ Ghc options used on package.
          ,packageFlags :: !(Map FlagName Bool)           -- ^ Flags used on package.
          ,packageDefaultFlags :: !(Map FlagName Bool)    -- ^ Defaults for unspecified flags.
          ,packageLibraries :: !PackageLibraries          -- ^ does the package have a buildable library stanza?
          ,packageInternalLibraries :: !(Set Text)        -- ^ names of internal libraries
          ,packageTests :: !(Map Text TestSuiteInterface) -- ^ names and interfaces of test suites
          ,packageBenchmarks :: !(Set Text)               -- ^ names of benchmarks
          ,packageExes :: !(Set Text)                     -- ^ names of executables
          ,packageOpts :: !GetPackageOpts                 -- ^ Args to pass to GHC.
          ,packageHasExposedModules :: !Bool              -- ^ Does the package have exposed modules?
          ,packageBuildType :: !BuildType                 -- ^ Package build-type.
          ,packageSetupDeps :: !(Maybe (Map PackageName VersionRange))
                                                          -- ^ If present: custom-setup dependencies
          }
 deriving (Show,Typeable)

packageIdent :: Package -> PackageIdentifier
packageIdent p = PackageIdentifier (packageName p) (packageVersion p)

-- | The value for a map from dependency name. This contains both the
-- version range and the type of dependency, and provides a semigroup
-- instance.
data DepValue = DepValue
  { dvVersionRange :: !VersionRange
  , dvType :: !DepType
  }
  deriving (Show,Typeable)
instance Semigroup DepValue where
  DepValue a x <> DepValue b y = DepValue (intersectVersionRanges a b) (x <> y)

-- | Is this package being used as a library, or just as a build tool?
-- If the former, we need to ensure that a library actually
-- exists. See
-- <https://github.com/commercialhaskell/stack/issues/2195>
data DepType = AsLibrary | AsBuildTool
  deriving (Show, Eq)
instance Semigroup DepType where
  AsLibrary <> _ = AsLibrary
  AsBuildTool <> x = x

packageIdentifier :: Package -> PackageIdentifier
packageIdentifier pkg =
    PackageIdentifier (packageName pkg) (packageVersion pkg)

packageDefinedFlags :: Package -> Set FlagName
packageDefinedFlags = M.keysSet . packageDefaultFlags

-- | Files that the package depends on, relative to package directory.
-- Argument is the location of the .cabal file
newtype GetPackageOpts = GetPackageOpts
    { getPackageOpts :: forall env. HasEnvConfig env
                     => SourceMap
                     -> InstalledMap
                     -> [PackageName]
                     -> [PackageName]
                     -> Path Abs File
                     -> RIO env
                          (Map NamedComponent (Map ModuleName (Path Abs File))
                          ,Map NamedComponent [DotCabalPath]
                          ,Map NamedComponent BuildInfoOpts)
    }
instance Show GetPackageOpts where
    show _ = "<GetPackageOpts>"

-- | GHC options based on cabal information and ghc-options.
data BuildInfoOpts = BuildInfoOpts
    { bioOpts :: [String]
    , bioOneWordOpts :: [String]
    , bioPackageFlags :: [String]
    -- ^ These options can safely have 'nubOrd' applied to them, as
    -- there are no multi-word options (see
    -- https://github.com/commercialhaskell/stack/issues/1255)
    , bioCabalMacros :: Path Abs File
    } deriving Show

-- | Files to get for a cabal package.
data CabalFileType
    = AllFiles
    | Modules

-- | Files that the package depends on, relative to package directory.
-- Argument is the location of the .cabal file
newtype GetPackageFiles = GetPackageFiles
    { getPackageFiles :: forall env. HasEnvConfig env
                      => Path Abs File
                      -> RIO env
                           (Map NamedComponent (Map ModuleName (Path Abs File))
                           ,Map NamedComponent [DotCabalPath]
                           ,Set (Path Abs File)
                           ,[PackageWarning])
    }
instance Show GetPackageFiles where
    show _ = "<GetPackageFiles>"

-- | Warning generated when reading a package
data PackageWarning
    = UnlistedModulesWarning NamedComponent [ModuleName]
      -- ^ Modules found that are not listed in cabal file

    -- TODO: bring this back - see
    -- https://github.com/commercialhaskell/stack/issues/2649
    {-
    | MissingModulesWarning (Path Abs File) (Maybe String) [ModuleName]
      -- ^ Modules not found in file system, which are listed in cabal file
    -}

-- | Package build configuration
data PackageConfig =
  PackageConfig {packageConfigEnableTests :: !Bool                -- ^ Are tests enabled?
                ,packageConfigEnableBenchmarks :: !Bool           -- ^ Are benchmarks enabled?
                ,packageConfigFlags :: !(Map FlagName Bool)       -- ^ Configured flags.
                ,packageConfigGhcOptions :: ![Text]               -- ^ Configured ghc options.
                ,packageConfigCompilerVersion :: ActualCompiler   -- ^ GHC version
                ,packageConfigPlatform :: !Platform               -- ^ host platform
                }
 deriving (Show,Typeable)

-- | Compares the package name.
instance Ord Package where
  compare = on compare packageName

-- | Compares the package name.
instance Eq Package where
  (==) = on (==) packageName

type SourceMap = Map PackageName PackageSource

-- | Where the package's source is located: local directory or package index
data PackageSource
  = PSFilePath LocalPackage InstallLocation
  -- ^ Package which exist on the filesystem
  | PSRemote InstallLocation (Map FlagName Bool) [Text] PackageLocationImmutable PackageIdentifier
  -- ^ Package which is downloaded remotely.
    deriving Show

piiVersion :: PackageSource -> Version
piiVersion (PSFilePath lp _) = packageVersion $ lpPackage lp
piiVersion (PSRemote _ _ _ _ (PackageIdentifier _ v)) = v

piiLocation :: PackageSource -> InstallLocation
piiLocation (PSFilePath _ loc) = loc
piiLocation (PSRemote loc _ _ _ _) = loc

-- | Information on a locally available package of source code
data LocalPackage = LocalPackage
    { lpPackage       :: !Package
    -- ^ The @Package@ info itself, after resolution with package flags,
    -- with tests and benchmarks disabled
    , lpComponents    :: !(Set NamedComponent)
    -- ^ Components to build, not including the library component.
    , lpUnbuildable   :: !(Set NamedComponent)
    -- ^ Components explicitly requested for build, that are marked
    -- "buildable: false".
    , lpWanted        :: !Bool -- FIXME Should completely drop this "wanted" terminology, it's unclear
    -- ^ Whether this package is wanted as a target.
    , lpTestDeps      :: !(Map PackageName VersionRange)
    -- ^ Used for determining if we can use --enable-tests in a normal build.
    , lpBenchDeps     :: !(Map PackageName VersionRange)
    -- ^ Used for determining if we can use --enable-benchmarks in a normal
    -- build.
    , lpTestBench     :: !(Maybe Package)
    -- ^ This stores the 'Package' with tests and benchmarks enabled, if
    -- either is asked for by the user.
    , lpCabalFile     :: !(Path Abs File)
    -- ^ The .cabal file
    , lpForceDirty    :: !Bool
    , lpDirtyFiles    :: !(IOThunk (Maybe (Set FilePath)))
    -- ^ Nothing == not dirty, Just == dirty. Note that the Set may be empty if
    -- we forced the build to treat packages as dirty. Also, the Set may not
    -- include all modified files.
    , lpNewBuildCaches :: !(IOThunk (Map NamedComponent (Map FilePath FileCacheInfo)))
    -- ^ current state of the files
    , lpComponentFiles :: !(IOThunk (Map NamedComponent (Set (Path Abs File))))
    -- ^ all files used by this package
    }
    deriving Show

newtype IOThunk a = IOThunk (IO a)
  deriving (Functor, Applicative, Monad)
instance Show (IOThunk a) where
  show _ = "<<IOThunk>>"

runIOThunk :: MonadIO m => IOThunk a -> m a
runIOThunk (IOThunk m) = liftIO m

mkIOThunk :: MonadUnliftIO m => m a -> m (IOThunk a)
mkIOThunk m = IOThunk <$> runOnce m

lpFiles :: MonadIO m => LocalPackage -> m (Set.Set (Path Abs File))
lpFiles = runIOThunk . fmap (Set.unions . M.elems) . lpComponentFiles

-- | A location to install a package into, either snapshot or local
data InstallLocation = Snap | Local
    deriving (Show, Eq)
instance Semigroup InstallLocation where
    Local <> _ = Local
    _ <> Local = Local
    Snap <> Snap = Snap
instance Monoid InstallLocation where
    mempty = Snap
    mappend = (<>)

data InstalledPackageLocation = InstalledTo InstallLocation | ExtraGlobal
    deriving (Show, Eq)

data FileCacheInfo = FileCacheInfo
    { fciModTime :: !ModTime
    , fciSize :: !Word64
    , fciHash :: !SHA256
    }
    deriving (Generic, Show, Eq, Data, Typeable)
instance Store FileCacheInfo
instance NFData FileCacheInfo

-- | Used for storage and comparison.
newtype ModTime = ModTime (Integer,Rational)
  deriving (Ord, Show, Generic, Eq, NFData, Store, Data, Typeable)

modTimeVC :: VersionConfig ModTime
modTimeVC = storeVersionConfig "mod-time-v1" "UBECpUI0JvM_SBOnRNdaiF9_yOU="

testSuccessVC :: VersionConfig Bool
testSuccessVC = storeVersionConfig "test-v1" "jC_GB0SGtbpRQbDlm7oQJP7thu8="

-- | A descriptor from a .cabal file indicating one of the following:
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
    deriving (Eq,Ord,Show)

-- | Maybe get the module name from the .cabal descriptor.
dotCabalModule :: DotCabalDescriptor -> Maybe ModuleName
dotCabalModule (DotCabalModule m) = Just m
dotCabalModule _ = Nothing

-- | Maybe get the main name from the .cabal descriptor.
dotCabalMain :: DotCabalDescriptor -> Maybe FilePath
dotCabalMain (DotCabalMain m) = Just m
dotCabalMain _ = Nothing

-- | A path resolved from the .cabal file, which is either main-is or
-- an exposed/internal/referenced module.
data DotCabalPath
    = DotCabalModulePath !(Path Abs File)
    | DotCabalMainPath !(Path Abs File)
    | DotCabalFilePath !(Path Abs File)
    | DotCabalCFilePath !(Path Abs File)
    deriving (Eq,Ord,Show)

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

type InstalledMap = Map PackageName (InstallLocation, Installed)

data Installed
    = Library PackageIdentifier GhcPkgId (Maybe (Either SPDX.License License))
    | Executable PackageIdentifier
    deriving (Show, Eq)

installedPackageIdentifier :: Installed -> PackageIdentifier
installedPackageIdentifier (Library pid _ _) = pid
installedPackageIdentifier (Executable pid) = pid

-- | Get the installed Version.
installedVersion :: Installed -> Version
installedVersion i =
  let PackageIdentifier _ version = installedPackageIdentifier i
   in version
