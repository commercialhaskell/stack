{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
-- | Shared types for various stackage packages.
module Stack.Types.BuildPlan
    ( -- * Types
      SnapshotDef (..)
    , snapshotDefVC
    , sdRawPathName
    , PackageLocation (..)
    , PackageLocationIndex (..)
    , RepoType (..)
    , Subdirs (..)
    , Repo (..)
    , Archive (..)
    , ExeName (..)
    , LoadedSnapshot (..)
    , loadedSnapshotVC
    , LoadedPackageInfo (..)
    , ModuleName (..)
    , fromCabalModuleName
    , ModuleInfo (..)
    , moduleInfoVC
    , setCompilerVersion
    , sdWantedCompilerVersion
    ) where

import           Data.Aeson (ToJSON (..), FromJSON (..), withText, object, (.=))
import           Data.Aeson.Extended (WithJSONWarnings (..), (..:), (..:?), withObjectWarnings, noJSONWarnings, (..!=))
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Store.Version
import           Data.Store.VersionTagged
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Distribution.ModuleName as C
import qualified Distribution.Version as C
import           Network.HTTP.Client (parseRequest)
import           Stack.Prelude
import           Stack.Types.Compiler
import           Stack.Types.FlagName
import           Stack.Types.GhcPkgId
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Resolver
import           Stack.Types.Version
import           Stack.Types.VersionIntervals

-- | A definition of a snapshot. This could be a Stackage snapshot or
-- something custom. It does not include information on the global
-- package database, this is added later.
--
-- It may seem more logic to attach flags, options, etc, directly with
-- the desired package. However, this isn't possible yet: our
-- definition may contain tarballs or Git repos, and we don't actually
-- know the package names contained there. Therefore, we capture all
-- of this additional information by package name, and later in the
-- snapshot load step we will resolve the contents of tarballs and
-- repos, figure out package names, and assigned values appropriately.
data SnapshotDef = SnapshotDef
    { sdParent :: !(Either (CompilerVersion 'CVWanted) SnapshotDef)
    -- ^ The snapshot to extend from. This is either a specific
    -- compiler, or a @SnapshotDef@ which gives us more information
    -- (like packages). Ultimately, we'll end up with a
    -- @CompilerVersion@.
    , sdResolver        :: !LoadedResolver
    -- ^ The resolver that provides this definition.
    , sdResolverName    :: !Text
    -- ^ A user-friendly way of referring to this resolver.
    , sdLocations :: ![PackageLocationIndex Subdirs]
    -- ^ Where to grab all of the packages from.
    , sdDropPackages :: !(Set PackageName)
    -- ^ Packages present in the parent which should not be included
    -- here.
    , sdFlags :: !(Map PackageName (Map FlagName Bool))
    -- ^ Flag values to override from the defaults
    , sdHidden :: !(Map PackageName Bool)
    -- ^ Packages which should be hidden when registering. This will
    -- affect, for example, the import parser in the script
    -- command. We use a 'Map' instead of just a 'Set' to allow
    -- overriding the hidden settings in a parent snapshot.
    , sdGhcOptions :: !(Map PackageName [Text])
    -- ^ GHC options per package
    , sdGlobalHints :: !(Map PackageName (Maybe Version))
    -- ^ Hints about which packages are available globally. When
    -- actually building code, we trust the package database provided
    -- by GHC itself, since it may be different based on platform or
    -- GHC install. However, when we want to check the compatibility
    -- of a snapshot with some codebase without installing GHC (e.g.,
    -- during stack init), we would use this field.
    }
    deriving (Show, Eq, Data, Generic, Typeable)
instance Store SnapshotDef
instance NFData SnapshotDef

snapshotDefVC :: VersionConfig SnapshotDef
snapshotDefVC = storeVersionConfig "sd-v1" "CKo7nln8EXkw07Gq-4ATxszNZiE="

-- | A relative file path including a unique string for the given
-- snapshot.
sdRawPathName :: SnapshotDef -> String
sdRawPathName sd =
    T.unpack $ go $ sdResolver sd
  where
    go (ResolverStackage name) = renderSnapName name
    go (ResolverCompiler version) = compilerVersionText version
    go (ResolverCustom _ hash) = "custom-" <> sdResolverName sd <> "-" <> trimmedSnapshotHash hash

-- | Modify the wanted compiler version in this snapshot. This is used
-- when overriding via the `compiler` value in a custom snapshot or
-- stack.yaml file. We do _not_ need to modify the snapshot's hash for
-- this: all binary caches of a snapshot are stored in a filepath that
-- encodes the actual compiler version in addition to the
-- hash. Therefore, modifications here will not lead to any invalid
-- data.
setCompilerVersion :: CompilerVersion 'CVWanted -> SnapshotDef -> SnapshotDef
setCompilerVersion cv =
    go
  where
    go sd =
      case sdParent sd of
        Left _ -> sd { sdParent = Left cv }
        Right sd' -> sd { sdParent = Right $ go sd' }

-- | Where to get the contents of a package (including cabal file
-- revisions) from.
--
-- A GADT may be more logical than the index parameter, but this plays
-- more nicely with Generic deriving.
data PackageLocation subdirs
  = PLFilePath !FilePath
    -- ^ Note that we use @FilePath@ and not @Path@s. The goal is: first parse
    -- the value raw, and then use @canonicalizePath@ and @parseAbsDir@.
  | PLArchive !(Archive subdirs)
  | PLRepo !(Repo subdirs)
  -- ^ Stored in a source control repository
    deriving (Generic, Show, Eq, Ord, Data, Typeable, Functor)
instance (Store a) => Store (PackageLocation a)
instance (NFData a) => NFData (PackageLocation a)

-- | Add in the possibility of getting packages from the index
-- (including cabal file revisions). We have special handling of this
-- case in many places in the codebase, and therefore represent it
-- with a separate data type from 'PackageLocation'.
data PackageLocationIndex subdirs
  = PLIndex !PackageIdentifierRevision
    -- ^ Grab the package from the package index with the given
    -- version and (optional) cabal file info to specify the correct
    -- revision.
  | PLOther !(PackageLocation subdirs)
    deriving (Generic, Show, Eq, Ord, Data, Typeable, Functor)
instance (Store a) => Store (PackageLocationIndex a)
instance (NFData a) => NFData (PackageLocationIndex a)

-- | A package archive, could be from a URL or a local file
-- path. Local file path archives are assumed to be unchanging
-- over time, and so are allowed in custom snapshots.
data Archive subdirs = Archive
  { archiveUrl :: !Text
  , archiveSubdirs :: !subdirs
  , archiveHash :: !(Maybe StaticSHA256)
  }
    deriving (Generic, Show, Eq, Ord, Data, Typeable, Functor)
instance Store a => Store (Archive a)
instance NFData a => NFData (Archive a)

-- | The type of a source control repository.
data RepoType = RepoGit | RepoHg
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Store RepoType
instance NFData RepoType

data Subdirs
  = DefaultSubdirs
  | ExplicitSubdirs ![FilePath]
    deriving (Generic, Show, Eq, Data, Typeable)
instance Store Subdirs
instance NFData Subdirs
instance FromJSON Subdirs where
  parseJSON = fmap ExplicitSubdirs . parseJSON

-- | Information on packages stored in a source control repository.
data Repo subdirs = Repo
    { repoUrl :: !Text
    , repoCommit :: !Text
    , repoType :: !RepoType
    , repoSubdirs :: !subdirs
    }
    deriving (Generic, Show, Eq, Ord, Data, Typeable, Functor)
instance Store a => Store (Repo a)
instance NFData a => NFData (Repo a)

instance subdirs ~ Subdirs => ToJSON (PackageLocationIndex subdirs) where
    toJSON (PLIndex ident) = toJSON ident
    toJSON (PLOther loc) = toJSON loc

instance subdirs ~ Subdirs => ToJSON (PackageLocation subdirs) where
    toJSON (PLFilePath fp) = toJSON fp
    toJSON (PLArchive (Archive t DefaultSubdirs Nothing)) = toJSON t
    toJSON (PLArchive (Archive t subdirs msha)) = object $ concat
        [ ["location" .= t]
        , case subdirs of
            DefaultSubdirs    -> []
            ExplicitSubdirs x -> ["subdirs" .= x]
        , case msha of
            Nothing -> []
            Just sha -> ["sha256" .= staticSHA256ToText sha]
        ]
    toJSON (PLRepo (Repo url commit typ subdirs)) = object $ concat
        [ case subdirs of
            DefaultSubdirs -> []
            ExplicitSubdirs x -> ["subdirs" .= x]
        , [urlKey .= url]
        , ["commit" .= commit]
        ]
      where
        urlKey =
          case typ of
            RepoGit -> "git"
            RepoHg  -> "hg"

instance subdirs ~ Subdirs => FromJSON (WithJSONWarnings (PackageLocationIndex subdirs)) where
    parseJSON v
        = (noJSONWarnings . PLIndex <$> parseJSON v)
      <|> (fmap PLOther <$> parseJSON v)

instance subdirs ~ Subdirs => FromJSON (WithJSONWarnings (PackageLocation subdirs)) where
    parseJSON v
        = (noJSONWarnings <$> withText "PackageLocation" (\t -> http t <|> file t) v)
        <|> repo v
        <|> archiveObject v
        <|> github v
      where
        file t = pure $ PLFilePath $ T.unpack t
        http t =
            case parseRequest $ T.unpack t of
                Left  _ -> fail $ "Could not parse URL: " ++ T.unpack t
                Right _ -> return $ PLArchive $ Archive t DefaultSubdirs Nothing

        repo = withObjectWarnings "PLRepo" $ \o -> do
          (repoType, repoUrl) <-
            ((RepoGit, ) <$> o ..: "git") <|>
            ((RepoHg, ) <$> o ..: "hg")
          repoCommit <- o ..: "commit"
          repoSubdirs <- o ..:? "subdirs" ..!= DefaultSubdirs
          return $ PLRepo Repo {..}

        archiveObject = withObjectWarnings "PLArchive" $ \o -> do
          url <- o ..: "archive"
          subdirs <- o ..:? "subdirs" ..!= DefaultSubdirs
          msha <- o ..:? "sha256"
          msha' <-
            case msha of
              Nothing -> return Nothing
              Just t ->
                case mkStaticSHA256FromText t of
                  Left e -> fail $ "Invalid SHA256: " ++ T.unpack t ++ ", " ++ show e
                  Right x -> return $ Just x
          return $ PLArchive Archive
            { archiveUrl = url
            , archiveSubdirs = subdirs :: Subdirs
            , archiveHash = msha'
            }

        github = withObjectWarnings "PLArchive:github" $ \o -> do
          GitHubRepo ghRepo <- o ..: "github"
          commit <- o ..: "commit"
          subdirs <- o ..:? "subdirs" ..!= DefaultSubdirs
          return $ PLArchive Archive
            { archiveUrl = "https://github.com/" <> ghRepo <> "/archive/" <> commit <> ".tar.gz"
            , archiveSubdirs = subdirs
            , archiveHash = Nothing
            }

-- An unexported newtype wrapper to hang a 'FromJSON' instance off of. Contains
-- a GitHub user and repo name separated by a forward slash, e.g. "foo/bar".
newtype GitHubRepo = GitHubRepo Text

instance FromJSON GitHubRepo where
    parseJSON = withText "GitHubRepo" $ \s -> do
        case T.split (== '/') s of
            [x, y] | not (T.null x || T.null y) -> return (GitHubRepo s)
            _ -> fail "expecting \"user/repo\""

-- | Name of an executable.
newtype ExeName = ExeName { unExeName :: Text }
    deriving (Show, Eq, Ord, Hashable, IsString, Generic, Store, NFData, Data, Typeable)

-- | A fully loaded snapshot combined , including information gleaned from the
-- global database and parsing cabal files.
--
-- Invariant: a global package may not depend upon a snapshot package,
-- a snapshot may not depend upon a local or project, and all
-- dependencies must be satisfied.
data LoadedSnapshot = LoadedSnapshot
  { lsCompilerVersion :: !(CompilerVersion 'CVActual)
  , lsGlobals         :: !(Map PackageName (LoadedPackageInfo GhcPkgId))
  , lsPackages        :: !(Map PackageName (LoadedPackageInfo (PackageLocationIndex FilePath)))
  }
    deriving (Generic, Show, Data, Eq, Typeable)
instance Store LoadedSnapshot
instance NFData LoadedSnapshot

loadedSnapshotVC :: VersionConfig LoadedSnapshot
loadedSnapshotVC = storeVersionConfig "ls-v4" "a_ljrJRo8hA_-gcIDP9c6NXJ2pE="

-- | Information on a single package for the 'LoadedSnapshot' which
-- can be installed.
--
-- Note that much of the information below (such as the package
-- dependencies or exposed modules) can be conditional in the cabal
-- file, which means it will vary based on flags, arch, and OS.
data LoadedPackageInfo loc = LoadedPackageInfo
    { lpiVersion :: !Version
    -- ^ This /must/ match the version specified within 'rpiDef'.
    , lpiLocation :: !loc
    -- ^ Where to get the package from. This could be a few different
    -- things:
    --
    -- * For a global package, it will be the @GhcPkgId@. (If we end
    -- up needing to rebuild this because we've changed a
    -- dependency, we will take it from the package index with no
    -- @CabalFileInfo@.
    --
    -- * For a dependency, it will be a @PackageLocation@.
    --
    -- * For a project package, it will be a @Path Abs Dir@.
    , lpiFlags :: !(Map FlagName Bool)
    -- ^ Flags to build this package with.
    , lpiGhcOptions :: ![Text]
    -- ^ GHC options to use when building this package.
    , lpiPackageDeps :: !(Map PackageName VersionIntervals)
    -- ^ All packages which must be built/copied/registered before
    -- this package.
    , lpiProvidedExes :: !(Set ExeName)
    -- ^ The names of executables provided by this package, for
    -- performing build tool lookups.
    , lpiNeededExes :: !(Map ExeName VersionIntervals)
    -- ^ Executables needed by this package.
    , lpiExposedModules :: !(Set ModuleName)
    -- ^ Modules exposed by this package's library
    , lpiHide :: !Bool
    -- ^ Should this package be hidden in the database. Affects the
    -- script interpreter's module name import parser.
    }
    deriving (Generic, Show, Eq, Data, Typeable, Functor)
instance Store a => Store (LoadedPackageInfo a)
instance NFData a => NFData (LoadedPackageInfo a)

data DepInfo = DepInfo
    { _diComponents :: !(Set Component)
    , _diRange      :: !VersionIntervals
    }
    deriving (Generic, Show, Eq, Data, Typeable)
instance Store DepInfo
instance NFData DepInfo

instance Monoid DepInfo where
    mempty = DepInfo mempty (fromVersionRange C.anyVersion)
    DepInfo a x `mappend` DepInfo b y = DepInfo
        (mappend a b)
        (intersectVersionIntervals x y)

data Component = CompLibrary
               | CompExecutable
               | CompTestSuite
               | CompBenchmark
    deriving (Generic, Show, Eq, Ord, Data, Typeable, Enum, Bounded)
instance Store Component
instance NFData Component

newtype ModuleName = ModuleName { unModuleName :: ByteString }
  deriving (Show, Eq, Ord, Generic, Store, NFData, Typeable, Data)

fromCabalModuleName :: C.ModuleName -> ModuleName
fromCabalModuleName = ModuleName . encodeUtf8 . T.intercalate "." . map T.pack . C.components

newtype ModuleInfo = ModuleInfo
    { miModules      :: Map ModuleName (Set PackageName)
    }
  deriving (Show, Eq, Ord, Generic, Typeable, Data)
instance Store ModuleInfo
instance NFData ModuleInfo

instance Monoid ModuleInfo where
  mempty = ModuleInfo mempty
  mappend (ModuleInfo x) (ModuleInfo y) =
    ModuleInfo (Map.unionWith Set.union x y)

moduleInfoVC :: VersionConfig ModuleInfo
moduleInfoVC = storeVersionConfig "mi-v2" "8ImAfrwMVmqoSoEpt85pLvFeV3s="

-- | Determined the desired compiler version for this 'SnapshotDef'.
sdWantedCompilerVersion :: SnapshotDef -> CompilerVersion 'CVWanted
sdWantedCompilerVersion = either id sdWantedCompilerVersion . sdParent
