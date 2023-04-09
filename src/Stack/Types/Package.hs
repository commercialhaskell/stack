{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}

module Stack.Types.Package
  ( BuildInfoOpts (..)
  , ExeName (..)
  , FileCacheInfo (..)
  , GetPackageOpts (..)
  , InstallLocation (..)
  , InstallMap
  , Installed (..)
  , InstalledPackageLocation (..)
  , InstalledMap
  , LocalPackage (..)
  , MemoizedWith (..)
  , Package (..)
  , PackageConfig (..)
  , PackageDatabase (..)
  , PackageDbVariety (..)
  , PackageException (..)
  , PackageLibraries (..)
  , PackageSource (..)
  , dotCabalCFilePath
  , dotCabalGetPath
  , dotCabalMain
  , dotCabalMainPath
  , dotCabalModule
  , dotCabalModulePath
  , installedPackageIdentifier
  , installedVersion
  , lpFiles
  , lpFilesForComponents
  , memoizeRefWith
  , packageDefinedFlags
  , packageIdentifier
  , psVersion
  , runMemoizedWith
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
import           Distribution.PackageDescription
                   ( BuildType )
import           Distribution.System ( Platform (..) )
import qualified RIO.Text as T
import           Stack.Prelude
import           Stack.Types.Compiler ( ActualCompiler )
import           Stack.Types.Dependency ( DepValue )
import           Stack.Types.EnvConfig ( EnvConfig, HasEnvConfig (..) )
import           Stack.Types.GhcPkgId ( GhcPkgId )
import           Stack.Types.NamedComponent ( NamedComponent )
import           Stack.Types.PackageFile
                   ( GetPackageFiles (..), DotCabalDescriptor (..)
                   , DotCabalPath (..)
                   )
import           Stack.Types.SourceMap ( CommonPackage, FromSnapshot )
import           Stack.Types.Version ( VersionRange )
import           Stack.Types.CompCollection ( CompCollection )
import           Stack.Types.Component ( StackLibrary, StackForeignLibrary, StackTest, StackBenchmark, StackExecutable )

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
  | ComponentNotParsedBug
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
  displayException ComponentNotParsedBug = bugReport "[S-4623]"
    "Component names should always parse as directory names."

-- | Libraries in a package. Since Cabal 2.0, sub-libraries are a thing.
data PackageLibraries
  = NoLibraries
  | HasLibraries !(Set Text)
    -- ^ the foreign library names, sub-libraries get built automatically
    -- without explicit component name passing
 deriving (Show, Typeable)

-- | Name of an executable.
newtype ExeName
  = ExeName { unExeName :: Text }
  deriving (Data, Eq, Generic, Hashable, IsString, NFData, Ord, Show, Typeable)

-- | Some package info.
data Package = Package
  { packageName :: !PackageName
    -- ^ Name of the package.
  , packageVersion :: !Version
    -- ^ Version of the package
  , packageLicense :: !(Either SPDX.License License)
    -- ^ The license the package was released under.
  , packageFiles :: !GetPackageFiles
    -- ^ Get all files of the package.
  , packageDeps :: !(Map PackageName DepValue)
    -- ^ Packages that the package depends on, both as libraries and build tools.
  , packageAllDeps :: !(Set PackageName)
    -- ^ Original dependencies (not sieved).
  , packageSubLibDeps :: !(Map MungedPackageName DepValue)
    -- ^ Original sub-library dependencies (not sieved).
  , packageGhcOptions :: ![Text]
    -- ^ Ghc options used on package.
  , packageCabalConfigOpts :: ![Text]
    -- ^ Additional options passed to ./Setup.hs configure
  , packageFlags :: !(Map FlagName Bool)
    -- ^ Flags used on package.
  , packageDefaultFlags :: !(Map FlagName Bool)
    -- ^ Defaults for unspecified flags.
  , packageLibrary :: Maybe StackLibrary
  , packageSubLibraries :: CompCollection StackLibrary
  , packageForeignLibraries :: CompCollection StackForeignLibrary
  , packageTestSuites :: CompCollection StackTest
  , packageBenchmarkSuites :: CompCollection StackBenchmark
  , packageExecutables :: CompCollection StackExecutable
    -- ^ does the package have a buildable library stanza?
  , packageOpts :: !GetPackageOpts
    -- ^ Args to pass to GHC.
  , packageBuildType :: !BuildType
    -- ^ Package build-type.
  , packageSetupDeps :: !(Maybe (Map PackageName VersionRange))
    -- ^ If present: custom-setup dependencies
  , packageCabalSpec :: !CabalSpecVersion
    -- ^ Cabal spec range
  }
  deriving (Show, Typeable)

packageIdentifier :: Package -> PackageIdentifier
packageIdentifier p = PackageIdentifier (packageName p) (packageVersion p)

packageDefinedFlags :: Package -> Set FlagName
packageDefinedFlags = M.keysSet . packageDefaultFlags

-- | Type synonym representing dictionaries of package names for a project's
-- packages and dependencies, and pairs of their relevant database (write-only
-- or mutable) and package versions.
type InstallMap = Map PackageName (InstallLocation, Version)

-- | Files that the package depends on, relative to package directory.
-- Argument is the location of the Cabal file
newtype GetPackageOpts = GetPackageOpts
  { getPackageOpts :: forall env. HasEnvConfig env
                   => InstallMap
                   -> InstalledMap
                   -> [PackageName]
                   -> [PackageName]
                   -> Path Abs File
                   -> RIO env
                        ( Map NamedComponent (Map ModuleName (Path Abs File))
                        , Map NamedComponent [DotCabalPath]
                        , Map NamedComponent BuildInfoOpts
                        )
  }

instance Show GetPackageOpts where
  show _ = "<GetPackageOpts>"

-- | GHC options based on cabal information and ghc-options.
data BuildInfoOpts = BuildInfoOpts
  { bioOpts :: [String]
  , bioOneWordOpts :: [String]
  , bioPackageFlags :: [String]
    -- ^ These options can safely have 'nubOrd' applied to them, as there are no
    -- multi-word options (see
    -- https://github.com/commercialhaskell/stack/issues/1255)
  , bioCabalMacros :: Path Abs File
  }
  deriving Show

-- | Package build configuration
data PackageConfig = PackageConfig
  { packageConfigEnableTests :: !Bool
    -- ^ Are tests enabled?
  , packageConfigEnableBenchmarks :: !Bool
    -- ^ Are benchmarks enabled?
  , packageConfigFlags :: !(Map FlagName Bool)
    -- ^ Configured flags.
  , packageConfigGhcOptions :: ![Text]
    -- ^ Configured ghc options.
  , packageConfigCabalConfigOpts :: ![Text]
    -- ^ ./Setup.hs configure options
  , packageConfigCompilerVersion :: ActualCompiler
    -- ^ GHC version
  , packageConfigPlatform :: !Platform
    -- ^ host platform
  }
 deriving (Show, Typeable)

-- | Compares the package name.
instance Ord Package where
  compare = on compare packageName

-- | Compares the package name.
instance Eq Package where
  (==) = on (==) packageName

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
psVersion (PSFilePath lp) = packageVersion $ lpPackage lp
psVersion (PSRemote _ v _ _) = v

-- | Information on a locally available package of source code.
data LocalPackage = LocalPackage
  { lpPackage       :: !Package
     -- ^ The @Package@ info itself, after resolution with package flags, with
     -- tests and benchmarks disabled
  , lpComponents    :: !(Set NamedComponent)
    -- ^ Components to build, not including the library component.
  , lpUnbuildable   :: !(Set NamedComponent)
    -- ^ Components explicitly requested for build, that are marked
    -- "buildable: false".
  , lpWanted        :: !Bool -- FIXME Should completely drop this "wanted"
                             -- terminology, it's unclear
    -- ^ Whether this package is wanted as a target.
  , lpTestBench     :: !(Maybe Package)
    -- ^ This stores the 'Package' with tests and benchmarks enabled, if either
    -- is asked for by the user.
  , lpCabalFile     :: !(Path Abs File)
    -- ^ The Cabal file
  , lpBuildHaddocks :: !Bool
    -- ^ Is Haddock documentation being built for this package?
  , lpForceDirty    :: !Bool
  , lpDirtyFiles    :: !(MemoizedWith EnvConfig (Maybe (Set FilePath)))
    -- ^ Nothing == not dirty, Just == dirty. Note that the Set may be empty if
    -- we forced the build to treat packages as dirty. Also, the Set may not
    -- include all modified files.
  , lpNewBuildCaches :: !( MemoizedWith
                             EnvConfig
                             (Map NamedComponent (Map FilePath FileCacheInfo))
                         )
    -- ^ current state of the files
  , lpComponentFiles :: !( MemoizedWith
                             EnvConfig
                             (Map NamedComponent (Set (Path Abs File)))
                         )
    -- ^ all files used by this package
  }
  deriving Show

newtype MemoizedWith env a
  = MemoizedWith { unMemoizedWith :: RIO env a }
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
lpFiles = runMemoizedWith . fmap (Set.unions . M.elems) . lpComponentFiles

lpFilesForComponents :: HasEnvConfig env
                     => Set NamedComponent
                     -> LocalPackage
                     -> RIO env (Set.Set (Path Abs File))
lpFilesForComponents components lp = runMemoizedWith $ do
  componentFiles <- lpComponentFiles lp
  pure $ mconcat (M.elems (M.restrictKeys componentFiles components))

-- | Type representing user package databases that packages can be installed
-- into.
data InstallLocation
  = Snap
    -- ^ The write-only package database, formerly known as the snapshot
    -- database.
  | Local
    -- ^ The mutable package database, formerly known as the local database.
  deriving (Eq, Show)

instance Semigroup InstallLocation where
  Local <> _ = Local
  _ <> Local = Local
  Snap <> Snap = Snap

instance Monoid InstallLocation where
  mempty = Snap
  mappend = (<>)

-- | Type representing user (non-global) package databases that can provide
-- installed packages.
data InstalledPackageLocation
  = InstalledTo InstallLocation
    -- ^ A package database that a package can be installed into.
  | ExtraPkgDb
    -- ^ An \'extra\' package database, specified by @extra-package-dbs@.
  deriving (Eq, Show)

-- | Type representing package databases that can provide installed packages.
data PackageDatabase
  = GlobalPkgDb
    -- ^ GHC's global package database.
  | UserPkgDb InstalledPackageLocation (Path Abs Dir)
    -- ^ A user package database.
  deriving (Eq, Show)

-- | Type representing varieties of package databases that can provide
-- installed packages.
data PackageDbVariety
  = GlobalDb
    -- ^ GHC's global package database.
  | ExtraDb
    -- ^ An \'extra\' package database, specified by @extra-package-dbs@.
  | WriteOnlyDb
    -- ^ The write-only package database, for immutable packages.
  | MutableDb
    -- ^ The mutable package database.
  deriving (Eq, Show)

-- | A function to yield the variety of package database for a given
-- package database that can provide installed packages.
toPackageDbVariety :: PackageDatabase -> PackageDbVariety
toPackageDbVariety GlobalPkgDb = GlobalDb
toPackageDbVariety (UserPkgDb ExtraPkgDb _) = ExtraDb
toPackageDbVariety (UserPkgDb (InstalledTo Snap) _) = WriteOnlyDb
toPackageDbVariety (UserPkgDb (InstalledTo Local) _) = MutableDb

newtype FileCacheInfo = FileCacheInfo
  { fciHash :: SHA256
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

-- | Type synonym representing dictionaries of package names, and a pair of in
-- which package database the package is installed (write-only or mutable) and
-- information about what is installed.
type InstalledMap = Map PackageName (InstallLocation, Installed)

-- | Type representing information about what is installed.
data Installed
  = Library PackageIdentifier GhcPkgId (Maybe (Either SPDX.License License))
    -- ^ A library, including its installed package id and, optionally, its
    -- license.
  | Executable PackageIdentifier
    -- ^ An executable.
  deriving (Eq, Show)

installedPackageIdentifier :: Installed -> PackageIdentifier
installedPackageIdentifier (Library pid _ _) = pid
installedPackageIdentifier (Executable pid) = pid

-- | Get the installed Version.
installedVersion :: Installed -> Version
installedVersion i =
  let PackageIdentifier _ version = installedPackageIdentifier i
  in  version
