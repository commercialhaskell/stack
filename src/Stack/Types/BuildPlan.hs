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
    , Snapshots (..)
    , SnapName (..)
    , ResolvedSnapshot (..)
    , resolvedSnapshotVC
    , ResolvedPackageInfo (..)
    , GitSHA1 (..)
    , renderSnapName
    , parseSnapName
    , SnapshotHash (..)
    , trimmedSnapshotHash
    , ModuleName (..)
    , ModuleInfo (..)
    , moduleInfoVC
    ) where

import           Control.Applicative
import           Control.DeepSeq (NFData)
import           Control.Exception (Exception)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Data.Aeson (ToJSON (..), FromJSON (..), withObject, withText, (.!=), (.:), (.:?), Value (Object), object, (.=))
import           Data.Aeson.Extended (WithJSONWarnings (..), (..:), (..:?), withObjectWarnings, noJSONWarnings)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Data
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable (Hashable)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
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
import           Data.Text.Read (decimal)
import           Data.Time (Day)
import           Data.Traversable (forM)
import qualified Distribution.Text as DT
import qualified Distribution.Version as C
import           GHC.Generics (Generic)
import           Network.HTTP.Client (parseRequest)
import           Prelude -- Fix AMP warning
import           Safe (readMay)
import           Stack.Types.Compiler
import           Stack.Types.FlagName
import           Stack.Types.PackageIdentifier
import           Stack.Types.PackageName
import           Stack.Types.Version
import           Stack.Types.VersionIntervals

-- | The name of an LTS Haskell or Stackage Nightly snapshot.
data SnapName
    = LTS !Int !Int
    | Nightly !Day
    deriving (Show, Eq, Ord)

-- | A definition of a snapshot. This could be a Stackage snapshot or
-- something custom. It does not include information on the global
-- package database, this is added later.
data SnapshotDef = SnapshotDef
    { sdCompilerVersion :: !CompilerVersion
    -- ^ The compiler version used for this snapshot.
    , sdPackages        :: !(Map PackageName PackageDef)
    -- ^ Packages included in this snapshot.
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
newtype StackageSnapshotDef = StackageSnapshotDef SnapshotDef

-- | Newtype wrapper to help parse a 'PackageDef' from the Stackage
-- YAML files.
newtype StackagePackageDef = StackagePackageDef { unStackagePackageDef :: PackageDef }

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

        sdPackages <- Map.map unStackagePackageDef <$> o .: "packages"

        return $ StackageSnapshotDef SnapshotDef {..}

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
        let pdLocation = PLIndex version mcabalFileInfo'

        Object constraints <- o .: "constraints"
        pdFlags <- constraints .: "flags"
        pdHide <- constraints .:? "hide" .!= False
        let pdGhcOptions = [] -- Stackage snapshots do not allow setting GHC options

        return $ StackagePackageDef PackageDef {..}

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

data BuildPlanTypesException
    = ParseSnapNameException Text
    | ParseFailedException TypeRep Text
    deriving Typeable
instance Exception BuildPlanTypesException
instance Show BuildPlanTypesException where
    show (ParseSnapNameException t) = "Invalid snapshot name: " ++ T.unpack t
    show (ParseFailedException rep t) =
        "Unable to parse " ++ show t ++ " as " ++ show rep

-- | Name of an executable.
newtype ExeName = ExeName { unExeName :: Text }
    deriving (Show, Eq, Ord, Hashable, IsString, Generic, Store, NFData, Data, Typeable)

-- | Convert a 'SnapName' into its short representation, e.g. @lts-2.8@,
-- @nightly-2015-03-05@.
renderSnapName :: SnapName -> Text
renderSnapName (LTS x y) = T.pack $ concat ["lts-", show x, ".", show y]
renderSnapName (Nightly d) = T.pack $ "nightly-" ++ show d

-- | Parse the short representation of a 'SnapName'.
parseSnapName :: MonadThrow m => Text -> m SnapName
parseSnapName t0 =
    case lts <|> nightly of
        Nothing -> throwM $ ParseSnapNameException t0
        Just sn -> return sn
  where
    lts = do
        t1 <- T.stripPrefix "lts-" t0
        Right (x, t2) <- Just $ decimal t1
        t3 <- T.stripPrefix "." t2
        Right (y, "") <- Just $ decimal t3
        return $ LTS x y
    nightly = do
        t1 <- T.stripPrefix "nightly-" t0
        Nightly <$> readMay (T.unpack t1)

-- | Most recent Nightly and newest LTS version per major release.
data Snapshots = Snapshots
    { snapshotsNightly :: !Day
    , snapshotsLts     :: !(IntMap Int)
    }
    deriving Show
instance FromJSON Snapshots where
    parseJSON = withObject "Snapshots" $ \o -> Snapshots
        <$> (o .: "nightly" >>= parseNightly)
        <*> fmap IntMap.unions (mapM (parseLTS . snd)
                $ filter (isLTS . fst)
                $ HashMap.toList o)
      where
        parseNightly t =
            case parseSnapName t of
                Left e -> fail $ show e
                Right (LTS _ _) -> fail "Unexpected LTS value"
                Right (Nightly d) -> return d

        isLTS = ("lts-" `T.isPrefixOf`)

        parseLTS = withText "LTS" $ \t ->
            case parseSnapName t of
                Left e -> fail $ show e
                Right (LTS x y) -> return $ IntMap.singleton x y
                Right (Nightly _) -> fail "Unexpected nightly value"

-- | A fully resolved snapshot, including information gleaned from the
-- global database and parsing cabal files.
data ResolvedSnapshot = ResolvedSnapshot
  { rsCompilerVersion :: !CompilerVersion
  , rsPackages        :: !(Map PackageName ResolvedPackageInfo)
  , rsUniqueName      :: !Text
  -- ^ A unique name for this resolved snapshot. Could be based on a
  -- unique upstream name (like a Stackage snapshot), the compiler
  -- name, or a hash of the custom snapshot definition.
  --
  -- This name must not contain any characters which would be
  -- unsuitable for a file path segment (such as forward or back
  -- slashes).
  }
    deriving (Generic, Show, Eq, Data, Typeable)
instance Store ResolvedSnapshot
instance NFData ResolvedSnapshot

resolvedSnapshotVC :: VersionConfig ResolvedSnapshot
resolvedSnapshotVC = storeVersionConfig "rs-v1" "LcNoSPO2J7r0ndDudqJy44QePhE="

-- | Information on a single package for the 'ResolvedSnapshot' which
-- can be installed.
data ResolvedPackageInfo = ResolvedPackageInfo
    { rpiVersion :: !Version
    -- ^ This /must/ match the version specified within 'rpiDef'.
    , rpiDef :: !(Maybe PackageDef)
    -- ^ The definition for this package. If the package is in the
    -- global database and not in the snapshot, this will be
    -- @Nothing@.
    , rpiPackageDeps :: !(Set PackageName)
    -- ^ All packages which must be built/copied/registered before
    -- this package.
    , rpiProvidedExes :: !(Set ExeName)
    -- ^ The names of executables provided by this package, for
    -- performing build tool lookups.
    , rpiNeededExes :: !(Map ExeName DepInfo)
    -- ^ Executables needed by this package's various components.
    , rpiExposedModules :: !(Set ModuleName)
    -- ^ Modules exposed by this package's library
    , rpiHide :: !Bool
    -- ^ Should this package be hidden in the database. Affects the
    -- script interpreter's module name import parser.
    }
    deriving (Generic, Show, Eq, Data, Typeable)
instance Store ResolvedPackageInfo
instance NFData ResolvedPackageInfo

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

newtype SnapshotHash = SnapshotHash { unShapshotHash :: ByteString }
    deriving (Generic, Show, Eq)

trimmedSnapshotHash :: SnapshotHash -> ByteString
trimmedSnapshotHash = BS.take 12 . unShapshotHash

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
