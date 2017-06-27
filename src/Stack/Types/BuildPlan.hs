{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}

{-# LANGUAGE DataKinds                  #-}
-- | Shared types for various stackage packages.
module Stack.Types.BuildPlan
    ( -- * Types
      SnapshotDef (..)
    , PackageDef (..)
    , PackageLocation (..)
    , CabalFileInfo (..)
    , RemotePackageType (..)
    , StackageSnapshotDef (..)
    , StackagePackageDef (..)
    , ExeName (..)
    , LoadedSnapshot (..)
    , loadedSnapshotVC
    , LoadedPackageInfo (..)
    , GitSHA1 (..)
    , ModuleName (..)
    , ModuleInfo (..)
    , moduleInfoVC
    ) where

import           Control.Applicative
import           Control.DeepSeq (NFData)
import           Data.Aeson (ToJSON (..), FromJSON (..), withObject, withText, (.!=), (.:), (.:?), Value (Object), object, (.=))
import           Data.Aeson.Extended (WithJSONWarnings (..), (..:), (..:?), withObjectWarnings, noJSONWarnings)
import           Data.ByteString (ByteString)
import           Data.Data
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable (Hashable)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store (Store)
import           Data.Store.Version
import           Data.Store.VersionTagged
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Traversable (forM)
import qualified Distribution.Version as C
import           GHC.Generics (Generic)
import           Network.HTTP.Client (parseRequest)
import           Prelude -- Fix AMP warning
import           Stack.Types.Compiler
import           Stack.Types.FlagName
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Resolver
import           Stack.Types.Version
import           Stack.Types.VersionIntervals

-- | A definition of a snapshot. This could be a Stackage snapshot or
-- something custom. It does not include information on the global
-- package database, this is added later.
data SnapshotDef = SnapshotDef
    { sdCompilerVersion :: !CompilerVersion
    -- ^ The compiler version used for this snapshot.
    , sdPackages        :: !(Map PackageName PackageDef)
    -- ^ Packages included in this snapshot.
    , sdResolver        :: !LoadedResolver
    -- ^ The resolver that provides this definition.
    }
    deriving (Show, Eq)

-- | A definition for how to install a single package within a
-- snapshot.
data PackageDef = PackageDef
    { pdLocation :: !PackageLocation
    -- ^ Where to get the package contents from
    , pdFlags    :: !(Map FlagName Bool)
    -- ^ Flag values to override from the defaults
    , pdHide             :: !Bool
    -- ^ Should this package be registered hidden in the package
    -- database? For example, affects parser importer in script
    -- command.
    , pdGhcOptions :: ![Text]
    -- ^ GHC options to be passed to this package
    }
    deriving (Generic, Show, Eq, Data, Typeable)
instance Store PackageDef
instance NFData PackageDef

-- | Where to get the contents of a package (including cabal file
-- revisions) from.
data PackageLocation
  = PLIndex !PackageIdentifier !(Maybe CabalFileInfo)
    -- ^ Grab the package from the package index with the given
    -- version and (optional) cabal file info to specify the correct
    -- revision.
  | PLFilePath !FilePath
    -- ^ Note that we use @FilePath@ and not @Path@s. The goal is: first parse
    -- the value raw, and then use @canonicalizePath@ and @parseAbsDir@.
  | PLRemote !Text !RemotePackageType
    -- ^ URL and further details
    deriving (Generic, Show, Eq, Data, Typeable)
instance Store PackageLocation
instance NFData PackageLocation

instance ToJSON PackageLocation where
    toJSON (PLIndex ident mcfi) =
        object $ addCFI mcfi ["ident" .= ident]
      where
        addCFI Nothing x = x
        addCFI (Just (CabalFileInfo size (GitSHA1 gitsha1))) x =
          ("cabal-file" .= object
            [ "size" .= size
            , "gitsha1" .= decodeUtf8 gitsha1
            ]) : x
    toJSON (PLFilePath fp) = toJSON fp
    toJSON (PLRemote t RPTHttp) = toJSON t
    toJSON (PLRemote x (RPTGit y)) = object [("git", toJSON x), ("commit", toJSON y)]
    toJSON (PLRemote x (RPTHg  y)) = object [( "hg", toJSON x), ("commit", toJSON y)]

instance FromJSON (WithJSONWarnings PackageLocation) where
    parseJSON v
        = (noJSONWarnings <$> withText "PackageLocation" (\t -> http t <|> file t) v)
        <|> git v
        <|> hg  v
        <|> index v
      where
        file t = pure $ PLFilePath $ T.unpack t
        http t =
            case parseRequest $ T.unpack t of
                Left  _ -> fail $ "Could not parse URL: " ++ T.unpack t
                Right _ -> return $ PLRemote t RPTHttp

        git = withObjectWarnings "PackageGitLocation" $ \o -> PLRemote
            <$> o ..: "git"
            <*> (RPTGit <$> o ..: "commit")
        hg  = withObjectWarnings "PackageHgLocation"  $ \o -> PLRemote
            <$> o ..: "hg"
            <*> (RPTHg  <$> o ..: "commit")
        index = withObjectWarnings "PackageIndexLocation" $ \o -> PLIndex
            <$> o ..: "ident"
            <*> (do
                    mcfi <- o ..:? "cabal-file"
                    case mcfi of
                      Nothing -> return Nothing
                      Just (Object cfi) -> Just <$> cabalFile cfi
                      Just _ -> fail "Invalid cabal-file, requires an object")
        cabalFile o = CabalFileInfo
            <$> o ..: "size"
            <*> ((GitSHA1 . encodeUtf8) <$> o ..: "gitsha1")

-- | What kind of remote package location we're dealing with.
data RemotePackageType
    = RPTHttp
    | RPTGit !Text -- ^ Commit
    | RPTHg  !Text -- ^ Commit
    deriving (Generic, Show, Eq, Data, Typeable)
instance Store RemotePackageType
instance NFData RemotePackageType

-- | Newtype wrapper to help parse a 'SnapshotDef' from the Stackage
-- YAML files.
newtype StackageSnapshotDef = StackageSnapshotDef (SnapName -> SnapshotDef)

-- | Newtype wrapper to help parse a 'PackageDef' from the Stackage
-- YAML files.
newtype StackagePackageDef = StackagePackageDef { unStackagePackageDef :: PackageName -> PackageDef }

instance FromJSON StackageSnapshotDef where
    parseJSON = withObject "StackageSnapshotDef" $ \o -> do
        Object si <- o .: "system-info"
        ghcVersion <- si .:? "ghc-version"
        compilerVersion <- si .:? "compiler-version"
        sdCompilerVersion <-
            case (ghcVersion, compilerVersion) of
                (Just _, Just _) -> fail "can't have both compiler-version and ghc-version fields"
                (Just ghc, _) -> return (GhcVersion ghc)
                (_, Just compiler) -> return compiler
                _ -> fail "expected field \"ghc-version\" or \"compiler-version\" not present"

        sdPackages <- Map.mapWithKey (\k v -> unStackagePackageDef v k) <$> o .: "packages"

        return $ StackageSnapshotDef $ \snapName ->
          let sdResolver = ResolverSnapshot snapName
           in SnapshotDef {..}

instance FromJSON StackagePackageDef where
    parseJSON = withObject "StackagePackageDef" $ \o -> do
        version <- o .: "version"
        mcabalFileInfo <- o .:? "cabal-file-info"
        mcabalFileInfo' <- forM mcabalFileInfo $ \o' -> do
            cfiSize <- o' .: "size"
            cfiHashes <- o' .: "hashes"
            cfiGitSHA1 <- fmap (GitSHA1 . encodeUtf8)
                        $ maybe
                            (fail "Could not find GitSHA1")
                            return
                        $ HashMap.lookup ("GitSHA1" :: Text) cfiHashes
            return CabalFileInfo {..}

        Object constraints <- o .: "constraints"
        pdFlags <- constraints .: "flags"
        pdHide <- constraints .:? "hide" .!= False
        let pdGhcOptions = [] -- Stackage snapshots do not allow setting GHC options

        return $ StackagePackageDef $ \name ->
          let pdLocation = PLIndex (PackageIdentifier name version) mcabalFileInfo'
           in PackageDef {..}

-- | Information on the contents of a cabal file
data CabalFileInfo = CabalFileInfo
    { cfiSize :: !Int
    -- ^ File size in bytes
    , cfiGitSHA1 :: !GitSHA1
    -- ^ 'GitSHA1' of the cabal file contents
    }
    deriving (Generic, Show, Eq, Data, Typeable)
instance Store CabalFileInfo
instance NFData CabalFileInfo

-- | Name of an executable.
newtype ExeName = ExeName { unExeName :: Text }
    deriving (Show, Eq, Ord, Hashable, IsString, Generic, Store, NFData, Data, Typeable)

-- | A fully loaded snapshot, including information gleaned from the
-- global database and parsing cabal files.
data LoadedSnapshot = LoadedSnapshot
  { lsCompilerVersion :: !CompilerVersion
  , lsResolver        :: !LoadedResolver
  , lsPackages        :: !(Map PackageName LoadedPackageInfo)
  }
    deriving (Generic, Show, Data, Eq, Typeable)
instance Store LoadedSnapshot
instance NFData LoadedSnapshot

loadedSnapshotVC :: VersionConfig LoadedSnapshot
loadedSnapshotVC = storeVersionConfig "ls-v1" "-jKxkhdmu5EYSA5qaxw-r9ZzX7k="

-- | Information on a single package for the 'LoadedSnapshot' which
-- can be installed.
data LoadedPackageInfo = LoadedPackageInfo
    { lpiVersion :: !Version
    -- ^ This /must/ match the version specified within 'rpiDef'.
    , lpiDef :: !(Maybe PackageDef)
    -- ^ The definition for this package. If the package is in the
    -- global database and not in the snapshot, this will be
    -- @Nothing@.
    , lpiPackageDeps :: !(Set PackageName)
    -- ^ All packages which must be built/copied/registered before
    -- this package.
    , lpiProvidedExes :: !(Set ExeName)
    -- ^ The names of executables provided by this package, for
    -- performing build tool lookups.
    , lpiNeededExes :: !(Map ExeName DepInfo)
    -- ^ Executables needed by this package's various components.
    , lpiExposedModules :: !(Set ModuleName)
    -- ^ Modules exposed by this package's library
    , lpiHide :: !Bool
    -- ^ Should this package be hidden in the database. Affects the
    -- script interpreter's module name import parser.
    }
    deriving (Generic, Show, Eq, Data, Typeable)
instance Store LoadedPackageInfo
instance NFData LoadedPackageInfo

data DepInfo = DepInfo
    { diComponents :: !(Set Component)
    , diRange      :: !VersionIntervals
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

compToText :: Component -> Text
compToText CompLibrary = "library"
compToText CompExecutable = "executable"
compToText CompTestSuite = "test-suite"
compToText CompBenchmark = "benchmark"

-- | A SHA1 hash, but in Git format. This means that the contents are
-- prefixed with @blob@ and the size of the payload before hashing, as
-- Git itself does.
newtype GitSHA1 = GitSHA1 ByteString
    deriving (Generic, Show, Eq, NFData, Store, Data, Typeable, Ord, Hashable)

newtype ModuleName = ModuleName { unModuleName :: ByteString }
  deriving (Show, Eq, Ord, Generic, Store, NFData, Typeable, Data)

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
