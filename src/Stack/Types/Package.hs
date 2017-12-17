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
import qualified Data.ByteString as S
import           Data.List
import qualified Data.Map as M
import qualified Data.Set as Set
import           Data.Store.Version (VersionConfig)
import           Data.Store.VersionTagged (storeVersionConfig)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Distribution.InstalledPackageInfo (PError)
import           Distribution.License (License)
import           Distribution.ModuleName (ModuleName)
import           Distribution.PackageDescription (TestSuiteInterface, BuildType)
import           Distribution.System (Platform (..))
import           Path as FL
import           Stack.Types.BuildPlan (PackageLocation, PackageLocationIndex (..), ExeName)
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.FlagName
import           Stack.Types.GhcPkgId
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version

-- | All exceptions thrown by the library.
data PackageException
  = PackageInvalidCabalFile (Either PackageIdentifierRevision (Path Abs File)) PError
  | PackageNoCabalFileFound (Path Abs Dir)
  | PackageMultipleCabalFilesFound (Path Abs Dir) [Path Abs File]
  | MismatchedCabalName (Path Abs File) PackageName
  | MismatchedCabalIdentifier !PackageIdentifierRevision !PackageIdentifier
  deriving Typeable
instance Exception PackageException
instance Show PackageException where
    show (PackageInvalidCabalFile loc err) = concat
        [ "Unable to parse cabal file "
        , case loc of
            Left pir -> "for " ++ packageIdentifierRevisionString pir
            Right fp -> toFilePath fp
        , ": "
        , show err
        ]
    show (PackageNoCabalFileFound dir) = concat
        [ "Stack looks for packages in the directories configured in"
        , " the 'packages' variable defined in your stack.yaml\n"
        , "The current entry points to " ++ toFilePath dir ++
          " but no .cabal file could be found there."
        ]
    show (PackageMultipleCabalFilesFound dir files) =
        "Multiple .cabal files found in directory " ++
        toFilePath dir ++
        ": " ++
        intercalate ", " (map (toFilePath . filename) files)
    show (MismatchedCabalName fp name) = concat
        [ "cabal file path "
        , toFilePath fp
        , " does not match the package name it defines.\n"
        , "Please rename the file to: "
        , packageNameString name
        , ".cabal\n"
        , "For more information, see: https://github.com/commercialhaskell/stack/issues/317"
        ]
    show (MismatchedCabalIdentifier pir ident) = concat
        [ "Mismatched package identifier."
        , "\nFound:    "
        , packageIdentifierString ident
        , "\nExpected: "
        , packageIdentifierRevisionString pir
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
          ,packageLicense :: !License                     -- ^ The license the package was released under.
          ,packageFiles :: !GetPackageFiles               -- ^ Get all files of the package.
          ,packageDeps :: !(Map PackageName VersionRange) -- ^ Packages that the package depends on.
          ,packageTools :: !(Map ExeName VersionRange)    -- ^ A build tool name.
          ,packageAllDeps :: !(Set PackageName)           -- ^ Original dependencies (not sieved).
          ,packageGhcOptions :: ![Text]                   -- ^ Ghc options used on package.
          ,packageFlags :: !(Map FlagName Bool)           -- ^ Flags used on package.
          ,packageDefaultFlags :: !(Map FlagName Bool)    -- ^ Defaults for unspecified flags.
          ,packageLibraries :: !PackageLibraries          -- ^ does the package have a buildable library stanza?
          ,packageTests :: !(Map Text TestSuiteInterface) -- ^ names and interfaces of test suites
          ,packageBenchmarks :: !(Set Text)               -- ^ names of benchmarks
          ,packageExes :: !(Set Text)                     -- ^ names of executables
          ,packageOpts :: !GetPackageOpts                 -- ^ Args to pass to GHC.
          ,packageHasExposedModules :: !Bool              -- ^ Does the package have exposed modules?
          ,packageBuildType :: !(Maybe BuildType)         -- ^ Package build-type.
          ,packageSetupDeps :: !(Maybe (Map PackageName VersionRange))
                                                          -- ^ If present: custom-setup dependencies
          }
 deriving (Show,Typeable)

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
                          (Map NamedComponent (Set ModuleName)
                          ,Map NamedComponent (Set DotCabalPath)
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
    , bioCabalMacros :: Maybe (Path Abs File)
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
                           (Map NamedComponent (Set ModuleName)
                           ,Map NamedComponent (Set DotCabalPath)
                           ,Set (Path Abs File)
                           ,[PackageWarning])
    }
instance Show GetPackageFiles where
    show _ = "<GetPackageFiles>"

-- | Warning generated when reading a package
data PackageWarning
    = UnlistedModulesWarning (Maybe String) [ModuleName]
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
                ,packageConfigCompilerVersion
                                  :: !(CompilerVersion 'CVActual) -- ^ GHC version
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
  = PSFiles LocalPackage InstallLocation
  -- ^ Package which exist on the filesystem (as opposed to an index tarball)
  | PSIndex InstallLocation (Map FlagName Bool) [Text] PackageIdentifierRevision
  -- ^ Package which is in an index, and the files do not exist on the
  -- filesystem yet.
    deriving Show

piiVersion :: PackageSource -> Version
piiVersion (PSFiles lp _) = packageVersion $ lpPackage lp
piiVersion (PSIndex _ _ _ (PackageIdentifierRevision (PackageIdentifier _ v) _)) = v

piiLocation :: PackageSource -> InstallLocation
piiLocation (PSFiles _ loc) = loc
piiLocation (PSIndex loc _ _ _) = loc

piiPackageLocation :: PackageSource -> PackageLocationIndex FilePath
piiPackageLocation (PSFiles lp _) = PLOther (lpLocation lp)
piiPackageLocation (PSIndex _ _ _ pir) = PLIndex pir

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
    , lpDir           :: !(Path Abs Dir)
    -- ^ Directory of the package.
    , lpCabalFile     :: !(Path Abs File)
    -- ^ The .cabal file
    , lpForceDirty    :: !Bool
    , lpDirtyFiles    :: !(Maybe (Set FilePath))
    -- ^ Nothing == not dirty, Just == dirty. Note that the Set may be empty if
    -- we forced the build to treat packages as dirty. Also, the Set may not
    -- include all modified files.
    , lpNewBuildCache :: !(Map FilePath FileCacheInfo)
    -- ^ current state of the files
    , lpFiles         :: !(Set (Path Abs File))
    -- ^ all files used by this package
    , lpLocation      :: !(PackageLocation FilePath)
    -- ^ Where this source code came from
    }
    deriving Show

renderComponent :: NamedComponent -> S.ByteString
renderComponent CLib = "lib"
renderComponent (CExe x) = "exe:" <> encodeUtf8 x
renderComponent (CTest x) = "test:" <> encodeUtf8 x
renderComponent (CBench x) = "bench:" <> encodeUtf8 x

renderPkgComponents :: [(PackageName, NamedComponent)] -> Text
renderPkgComponents = T.intercalate " " . map renderPkgComponent

renderPkgComponent :: (PackageName, NamedComponent) -> Text
renderPkgComponent (pkg, comp) = packageNameText pkg <> ":" <> decodeUtf8 (renderComponent comp)

exeComponents :: Set NamedComponent -> Set Text
exeComponents = Set.fromList . mapMaybe mExeName . Set.toList
  where
    mExeName (CExe name) = Just name
    mExeName _ = Nothing

testComponents :: Set NamedComponent -> Set Text
testComponents = Set.fromList . mapMaybe mTestName . Set.toList
  where
    mTestName (CTest name) = Just name
    mTestName _ = Nothing

benchComponents :: Set NamedComponent -> Set Text
benchComponents = Set.fromList . mapMaybe mBenchName . Set.toList
  where
    mBenchName (CBench name) = Just name
    mBenchName _ = Nothing

isCLib :: NamedComponent -> Bool
isCLib CLib{} = True
isCLib _ = False

isCExe :: NamedComponent -> Bool
isCExe CExe{} = True
isCExe _ = False

isCTest :: NamedComponent -> Bool
isCTest CTest{} = True
isCTest _ = False

isCBench :: NamedComponent -> Bool
isCBench CBench{} = True
isCBench _ = False

-- | A location to install a package into, either snapshot or local
data InstallLocation = Snap | Local
    deriving (Show, Eq)
instance Monoid InstallLocation where
    mempty = Snap
    mappend Local _ = Local
    mappend _ Local = Local
    mappend Snap Snap = Snap

data InstalledPackageLocation = InstalledTo InstallLocation | ExtraGlobal
    deriving (Show, Eq)

data FileCacheInfo = FileCacheInfo
    { fciModTime :: !ModTime
    , fciSize :: !Word64
    , fciHash :: !S.ByteString
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
    = Library PackageIdentifier GhcPkgId (Maybe License)
    | Executable PackageIdentifier
    deriving (Show, Eq)

installedPackageIdentifier :: Installed -> PackageIdentifier
installedPackageIdentifier (Library pid _ _) = pid
installedPackageIdentifier (Executable pid) = pid

-- | Get the installed Version.
installedVersion :: Installed -> Version
installedVersion = packageIdentifierVersion . installedPackageIdentifier
