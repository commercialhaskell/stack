{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- |

module Stack.Types.Package where

import           Control.DeepSeq
import           Control.Exception hiding (try,catch)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger (MonadLogger)
import           Control.Monad.Reader
import           Data.Binary
import           Data.Binary.VersionTagged
import qualified Data.ByteString as S
import           Data.Data
import           Data.Function
import           Data.List
import qualified Data.Map as M
import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Distribution.InstalledPackageInfo (PError)
import           Distribution.ModuleName (ModuleName)
import           Distribution.Package hiding (Package,PackageName,packageName,packageVersion,PackageIdentifier)
import           Distribution.PackageDescription (TestSuiteInterface)
import           Distribution.System (Platform (..))
import           Distribution.Text (display)
import           GHC.Generics (Generic)
import           Path as FL
import           Prelude
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.FlagName
import           Stack.Types.GhcPkgId
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version

-- | All exceptions thrown by the library.
data PackageException
  = PackageInvalidCabalFile (Maybe (Path Abs File)) PError
  | PackageNoCabalFileFound (Path Abs Dir)
  | PackageMultipleCabalFilesFound (Path Abs Dir) [Path Abs File]
  | MismatchedCabalName (Path Abs File) PackageName
  deriving Typeable
instance Exception PackageException
instance Show PackageException where
    show (PackageInvalidCabalFile mfile err) =
        "Unable to parse cabal file" ++
        (case mfile of
            Nothing -> ""
            Just file -> ' ' : toFilePath file) ++
        ": " ++
        show err
    show (PackageNoCabalFileFound dir) =
        "No .cabal file found in directory " ++
        toFilePath dir
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

-- | Some package info.
data Package =
  Package {packageName :: !PackageName                    -- ^ Name of the package.
          ,packageVersion :: !Version                     -- ^ Version of the package
          ,packageFiles :: !GetPackageFiles               -- ^ Get all files of the package.
          ,packageDeps :: !(Map PackageName VersionRange) -- ^ Packages that the package depends on.
          ,packageTools :: ![Dependency]                  -- ^ A build tool name.
          ,packageAllDeps :: !(Set PackageName)           -- ^ Original dependencies (not sieved).
          ,packageFlags :: !(Map FlagName Bool)           -- ^ Flags used on package.
          ,packageDefaultFlags :: !(Map FlagName Bool)    -- ^ Defaults for unspecified flags.
          ,packageHasLibrary :: !Bool                     -- ^ does the package have a buildable library stanza?
          ,packageTests :: !(Map Text TestSuiteInterface) -- ^ names and interfaces of test suites
          ,packageBenchmarks :: !(Set Text)               -- ^ names of benchmarks
          ,packageExes :: !(Set Text)                     -- ^ names of executables
          ,packageOpts :: !GetPackageOpts                 -- ^ Args to pass to GHC.
          ,packageHasExposedModules :: !Bool              -- ^ Does the package have exposed modules?
          ,packageSimpleType :: !Bool                     -- ^ Does the package of build-type: Simple
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
    { getPackageOpts :: forall env m. (MonadIO m,HasEnvConfig env, HasPlatform env, MonadThrow m, MonadReader env m, MonadLogger m, MonadCatch m)
                     => SourceMap
                     -> InstalledMap
                     -> [PackageName]
                     -> [PackageName]
                     -> Path Abs File
                     -> m (Map NamedComponent (Set ModuleName)
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
    { getPackageFiles :: forall m env. (MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m, MonadReader env m, HasPlatform env, HasEnvConfig env)
                      => Path Abs File
                      -> m (Map NamedComponent (Set ModuleName)
                           ,Map NamedComponent (Set DotCabalPath)
                           ,Set (Path Abs File)
                           ,[PackageWarning])
    }
instance Show GetPackageFiles where
    show _ = "<GetPackageFiles>"

-- | Warning generated when reading a package
data PackageWarning
    = UnlistedModulesWarning (Path Abs File) (Maybe String) [ModuleName]
      -- ^ Modules found that are not listed in cabal file
    | MissingModulesWarning (Path Abs File) (Maybe String) [ModuleName]
      -- ^ Modules not found in file system, which are listed in cabal file
instance Show PackageWarning where
    show (UnlistedModulesWarning cabalfp component [unlistedModule]) =
        concat
            [ "module not listed in "
            , toFilePath (filename cabalfp)
            , case component of
                   Nothing -> " for library"
                   Just c -> " for '" ++ c ++ "'"
            , " component (add to other-modules): "
            , display unlistedModule]
    show (UnlistedModulesWarning cabalfp component unlistedModules) =
        concat
            [ "modules not listed in "
            , toFilePath (filename cabalfp)
            , case component of
                   Nothing -> " for library"
                   Just c -> " for '" ++ c ++ "'"
            , " component (add to other-modules):\n    "
            , intercalate "\n    " (map display unlistedModules)]
    show (MissingModulesWarning cabalfp component [missingModule]) =
        concat
            [ "module listed in "
            , toFilePath (filename cabalfp)
            , case component of
                   Nothing -> " for library"
                   Just c -> " for '" ++ c ++ "'"
            , " component not found in filesystem: "
            , display missingModule]
    show (MissingModulesWarning cabalfp component missingModules) =
        concat
            [ "modules listed in "
            , toFilePath (filename cabalfp)
            , case component of
                   Nothing -> " for library"
                   Just c -> " for '" ++ c ++ "'"
            , " component not found in filesystem:\n    "
            , intercalate "\n    " (map display missingModules)]


-- | Package build configuration
data PackageConfig =
  PackageConfig {packageConfigEnableTests :: !Bool                -- ^ Are tests enabled?
                ,packageConfigEnableBenchmarks :: !Bool           -- ^ Are benchmarks enabled?
                ,packageConfigFlags :: !(Map FlagName Bool)       -- ^ Package config flags.
                ,packageConfigCompilerVersion :: !CompilerVersion -- ^ GHC version
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
    = PSLocal LocalPackage
    | PSUpstream Version InstallLocation (Map FlagName Bool)
    -- ^ Upstream packages could be installed in either local or snapshot
    -- databases; this is what 'InstallLocation' specifies.
    deriving Show

instance PackageInstallInfo PackageSource where
    piiVersion (PSLocal lp) = packageVersion $ lpPackage lp
    piiVersion (PSUpstream v _ _) = v

    piiLocation (PSLocal _) = Local
    piiLocation (PSUpstream _ loc _) = loc

-- | Datatype which tells how which version of a package to install and where
-- to install it into
class PackageInstallInfo a where
    piiVersion :: a -> Version
    piiLocation :: a -> InstallLocation

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
    , lpWanted        :: !Bool
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
    }
    deriving Show

-- | A single, fully resolved component of a package
data NamedComponent
    = CLib
    | CExe !Text
    | CTest !Text
    | CBench !Text
    deriving (Show, Eq, Ord)

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
    deriving (Generic, Show)
instance Binary FileCacheInfo
instance HasStructuralInfo FileCacheInfo
instance NFData FileCacheInfo

-- | Used for storage and comparison.
newtype ModTime = ModTime (Integer,Rational)
  deriving (Ord,Show,Generic,Eq,NFData,Binary)

instance HasStructuralInfo ModTime
instance HasSemanticVersion ModTime

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

data Installed = Library PackageIdentifier GhcPkgId | Executable PackageIdentifier
    deriving (Show, Eq, Ord)

installedPackageIdentifier :: Installed -> PackageIdentifier
installedPackageIdentifier (Library pid _) = pid
installedPackageIdentifier (Executable pid) = pid

-- | Get the installed Version.
installedVersion :: Installed -> Version
installedVersion = packageIdentifierVersion . installedPackageIdentifier
