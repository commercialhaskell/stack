{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiWayIf #-}
module Pantry.Types
  ( PantryConfig (..)
  , HackageSecurityConfig (..)
  , Storage (..)
  , HasPantryConfig (..)
  , BlobKey (..)
  , PackageName
  , Version
  , PackageIdentifier (..)
  , Revision (..)
  , CabalFileInfo (..)
  , PrintWarnings (..)
  , PackageNameP (..)
  , VersionP (..)
  , PackageIdentifierRevision (..)
  , FileType (..)
  , BuildFileType (..)
  , FileSize (..)
  , TreeEntry (..)
  , SafeFilePath
  , unSafeFilePath
  , mkSafeFilePath
  , TreeKey (..)
  , Tree (..)
  , renderTree
  , parseTree
  , SHA256
  , Unresolved
  , resolvePaths
  , Package (..)
  , PackageCabal (..)
  , getPCBlobKey
  , getPCTreeEntry
  -- , PackageTarball (..)
  , PackageLocation (..)
  , PackageLocationImmutable (..)
  , Archive (..)
  , Repo (..)
  , RepoType (..)
  , parsePackageIdentifier
  , parsePackageName
  , parsePackageNameThrowing
  , parseFlagName
  , parseVersion
  , parseVersionThrowing
  , packageIdentifierString
  , packageNameString
  , flagNameString
  , versionString
  , moduleNameString
  , OptionalSubdirs (..)
  , ArchiveLocation (..)
  , RelFilePath (..)
  , CabalString (..)
  , toCabalStringMap
  , unCabalStringMap
  , parsePackageIdentifierRevision
  , Mismatch (..)
  , PantryException (..)
  , FuzzyResults (..)
  , ResolvedPath (..)
  , HpackExecutable (..)
  , WantedCompiler (..)
  --, resolveSnapshotLocation
  , ltsSnapshotLocation
  , nightlySnapshotLocation
  , SnapshotLocation (..)
  , parseSnapshotLocation
  , SnapshotLayer (..)
  , Snapshot (..)
  , SnapshotPackage (..)
  , parseWantedCompiler
  , PackageMetadata (..)
  , cabalFileName
  ) where

import RIO
import qualified Data.Conduit.Tar as Tar
import qualified RIO.Text as T
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import RIO.Char (isSpace)
import RIO.List (intersperse)
import RIO.Time (toGregorian, Day)
import qualified RIO.Map as Map
import qualified RIO.HashMap as HM
import qualified Data.Map.Strict as Map (mapKeysMonotonic)
import qualified RIO.Set as Set
import Data.Aeson (ToJSON (..), FromJSON (..), withText, FromJSONKey (..))
import Data.Aeson.Types (ToJSONKey (..) ,toJSONKeyText, Parser)
import Data.Aeson.Extended
import Data.ByteString.Builder (toLazyByteString, byteString, wordDec)
import Database.Persist
import Database.Persist.Sql
import Pantry.SHA256 (SHA256)
import qualified Pantry.SHA256 as SHA256
import qualified Distribution.Compat.ReadP as Parse
import Distribution.CabalSpecVersion (CabalSpecVersion (..), cabalSpecLatest)
import Distribution.Parsec.Common (PError (..), PWarning (..), showPos)
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.VersionRange (VersionRange)
import Distribution.PackageDescription (FlagName, unFlagName, GenericPackageDescription)
import Distribution.Types.PackageId (PackageIdentifier (..))
import qualified Distribution.Text
import Distribution.ModuleName (ModuleName)
import Distribution.Types.Version (Version, mkVersion)
import Data.Store (Size (..), Store (..))
import Network.HTTP.Client (parseRequest)
import Network.HTTP.Types (Status, statusCode)
import Data.Text.Read (decimal)
import Path (Path, Abs, Dir, File, toFilePath, filename)
import Path.IO (resolveFile, resolveDir)
import Data.Pool (Pool)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

-- | Parsed tree with more information on the Haskell package it contains.
--
-- @since 0.1.0.0
data Package = Package
  { packageTreeKey :: !TreeKey
  -- ^ The 'TreeKey' containing this package.
  --
  -- This is a hash of the binary representation of 'packageTree'.
  --
  -- @since 0.1.0.0
  , packageTree :: !Tree
  -- ^ The 'Tree' containing this package.
  --
  -- @since 0.1.0.0
  , packageCabalEntry :: !PackageCabal
  -- ^ Information on the cabal file inside this package.
  --
  -- @since 0.1.0.0
  , packageIdent :: !PackageIdentifier
  -- ^ The package name and version in this package.
  --
  -- @since 0.1.0.0
  }
  deriving (Show, Eq)

data PackageCabal = PCCabalFile !TreeEntry -- ^ TreeEntry of Cabal file
                  | PCHpack !TreeEntry -- ^ TreeEntry of HPack file
                  | PCHpackCabalFile !TreeEntry -- ^ TreeEntry of Cabal file produced via HPack
                    deriving (Show, Eq)

getPCBlobKey :: PackageCabal -> BlobKey
getPCBlobKey (PCCabalFile (TreeEntry cabalBlobKey _)) = cabalBlobKey
getPCBlobKey (PCHpack (TreeEntry hpackBlobKey _)) = hpackBlobKey
getPCBlobKey (PCHpackCabalFile (TreeEntry cabalBlobKey _)) = cabalBlobKey

getPCTreeEntry :: PackageCabal -> TreeEntry
getPCTreeEntry (PCCabalFile tentry) = tentry
getPCTreeEntry (PCHpack tentry) = tentry
getPCTreeEntry (PCHpackCabalFile tentry) = tentry

cabalFileName :: PackageName -> SafeFilePath
cabalFileName name =
  case mkSafeFilePath $ T.pack (packageNameString name) <> ".cabal" of
    Nothing -> error $ "cabalFileName: failed for " ++ show name
    Just sfp -> sfp

-- | The revision number of a package from Hackage, counting upwards
-- from 0 (the original cabal file).
--
-- See caveats on 'CFIRevision'.
--
-- @since 0.1.0.0
newtype Revision = Revision Word
    deriving (Generic, Show, Eq, NFData, Data, Typeable, Ord, Hashable, Store, Display, PersistField, PersistFieldSql)

newtype Storage = Storage (Pool SqlBackend)

-- | Configuration value used by the entire pantry package. Create one
-- using @withPantryConfig@. See also @PantryApp@ for a convenience
-- approach to using pantry.
--
-- @since 0.1.0.0
data PantryConfig = PantryConfig
  { pcHackageSecurity :: !HackageSecurityConfig
  , pcHpackExecutable :: !HpackExecutable
  , pcRootDir :: !(Path Abs Dir)
  , pcStorage :: !Storage
  , pcUpdateRef :: !(MVar Bool)
  -- ^ Want to try updating the index once during a single run for missing
  -- package identifiers. We also want to ensure we only update once at a
  -- time. Start at @True@.
  , pcParsedCabalFilesImmutable :: !(IORef (Map PackageLocationImmutable GenericPackageDescription))
  -- ^ Cache of previously parsed cabal files, to save on slow parsing time.
  , pcParsedCabalFilesMutable ::
      !(IORef
        (Map
         (Path Abs Dir)
         (PrintWarnings -> IO GenericPackageDescription, PackageName, Path Abs File)
        )
       )
  -- ^ Cache for mutable packages. We want to allow for an optimization:
  -- deferring parsing of the 'GenericPackageDescription' until its actually
  -- needed. Therefore, we keep the filepath and the 'PackageName' derived from
  -- that filepath. When the @IO GenericPackageDescription@ is run, it will
  -- ensure that the @PackageName@ matches the value inside the cabal file, and
  -- print out any warnings that still need to be printed.
  , pcConnectionCount :: !Int
  -- ^ concurrently open downloads
  }

-- | Should we print warnings when loading a cabal file?
--
-- @since 0.1.0.0
data PrintWarnings = YesPrintWarnings | NoPrintWarnings

-- | Wraps a value which potentially contains relative paths. Needs to
-- be provided with a base directory to resolve these paths.
--
-- Unwrap this using 'resolvePaths'.
--
-- @since 0.1.0.0
newtype Unresolved a = Unresolved (Maybe (Path Abs Dir) -> IO a)
  deriving Functor
instance Applicative Unresolved where
  pure = Unresolved . const . pure
  Unresolved f <*> Unresolved x = Unresolved $ \mdir -> f mdir <*> x mdir

-- | Resolve all of the file paths in an 'Unresolved' relative to the
-- given directory.
--
-- @since 0.1.0.0
resolvePaths
  :: MonadIO m
  => Maybe (Path Abs Dir) -- ^ directory to use for relative paths
  -> Unresolved a
  -> m a
resolvePaths mdir (Unresolved f) = liftIO (f mdir)

-- | A combination of the relative path provided in a config file,
-- together with the resolved absolute path.
--
-- @since 0.1.0.0
data ResolvedPath t = ResolvedPath
  { resolvedRelative :: !RelFilePath
  -- ^ Original value parsed from a config file.
  , resolvedAbsolute :: !(Path Abs t)
  -- ^ Absolute path resolved against base directory loaded from.
  }
  deriving (Show, Eq, Data, Generic, Ord)
instance NFData (ResolvedPath t)
instance Store (ResolvedPath Dir)
instance Store (ResolvedPath File)

-- | Location to load a package from. Can either be immutable (see
-- 'PackageLocationImmutable') or a local directory which is expected
-- to change over time.
--
-- @since 0.1.0.0
data PackageLocation
  = PLImmutable !PackageLocationImmutable
  | PLMutable !(ResolvedPath Dir)
  deriving (Show, Eq, Data, Generic)
instance NFData PackageLocation
instance Store PackageLocation

instance Display PackageLocation where
  display (PLImmutable loc) = display loc
  display (PLMutable fp) = fromString $ toFilePath $ resolvedAbsolute fp

-- | Location for remote packages or archives assumed to be immutable.
--
-- @since 0.1.0.0
data PackageLocationImmutable
  = PLIHackage !PackageIdentifierRevision !(Maybe TreeKey)
  | PLIArchive !Archive !PackageMetadata
  | PLIRepo    !Repo !PackageMetadata
  deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance NFData PackageLocationImmutable
instance Store PackageLocationImmutable

instance Display PackageLocationImmutable where
  display (PLIHackage pir _tree) = display pir <> " (from Hackage)"
  display (PLIArchive archive _pm) =
    "Archive from " <> display (archiveLocation archive) <>
    (if T.null $ archiveSubdir archive
       then mempty
       else " in subdir " <> display (archiveSubdir archive))
  display (PLIRepo repo _pm) =
    "Repo from " <> display (repoUrl repo) <>
    ", commit " <> display (repoCommit repo) <>
    (if T.null $ repoSubdir repo
       then mempty
       else " in subdir " <> display (repoSubdir repo))

-- | A package archive, could be from a URL or a local file
-- path. Local file path archives are assumed to be unchanging
-- over time, and so are allowed in custom snapshots.
--
-- @since 0.1.0.0
data Archive = Archive
  { archiveLocation :: !ArchiveLocation
  -- ^ Location of the archive
  --
  -- @since 0.1.0.0
  , archiveHash :: !(Maybe SHA256)
  -- ^ Cryptographic hash of the archive file
  --
  -- @since 0.1.0.0
  , archiveSize :: !(Maybe FileSize)
  -- ^ Size of the archive file
  --
  -- @since 0.1.0.0
  , archiveSubdir :: !Text
  -- ^ Subdirectory within the archive to get the package from.
  --
  -- @since 0.1.0.0
  }
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Store Archive
instance NFData Archive

-- | The type of a source control repository.
--
-- @since 0.1.0.0
data RepoType = RepoGit | RepoHg
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Store RepoType
instance NFData RepoType
instance PersistField RepoType where
  toPersistValue RepoGit = toPersistValue (1 :: Int32)
  toPersistValue RepoHg = toPersistValue (2 :: Int32)
  fromPersistValue v = do
    i <- fromPersistValue v
    case i :: Int32 of
      1 -> pure RepoGit
      2 -> pure RepoHg
      _ -> fail $ "Invalid RepoType: " ++ show i
instance PersistFieldSql RepoType where
  sqlType _ = SqlInt32

-- | Information on packages stored in a source control repository.
--
-- @since 0.1.0.0
data Repo = Repo
  { repoUrl :: !Text
    -- ^ Location of the repo
    --
    -- @since 0.1.0.0
  , repoCommit :: !Text
    -- ^ Commit to use from the repo. It's strongly recommended to use
    -- a hash instead of a tag or branch name.
    --
    -- @since 0.1.0.0
  , repoType :: !RepoType
    -- ^ The type of the repo
    --
    -- @since 0.1.0.0
  , repoSubdir :: !Text
  -- ^ Subdirectory within the archive to get the package from.
  --
  -- @since 0.1.0.0
  }
    deriving (Generic, Eq, Ord, Data, Typeable)
instance Store Repo
instance NFData Repo
instance Show Repo where
  show = T.unpack . utf8BuilderToText . display
instance Display Repo where
  display (Repo url commit typ subdir) =
    (case typ of
       RepoGit -> "Git"
       RepoHg -> "Mercurial") <>
    " repo at " <>
    display url <>
    ", commit " <>
    display commit <>
    (if T.null subdir
      then mempty
      else " in subdirectory " <> display subdir)

-- An unexported newtype wrapper to hang a 'FromJSON' instance off of. Contains
-- a GitHub user and repo name separated by a forward slash, e.g. "foo/bar".
newtype GitHubRepo = GitHubRepo Text

instance FromJSON GitHubRepo where
    parseJSON = withText "GitHubRepo" $ \s -> do
        case T.split (== '/') s of
            [x, y] | not (T.null x || T.null y) -> return (GitHubRepo s)
            _ -> fail "expecting \"user/repo\""

-- | Configuration for Hackage Security to securely download package
-- metadata and contents from Hackage. For most purposes, you'll want
-- to use the default Hackage settings via
-- @defaultHackageSecurityConfig@.
--
-- /NOTE/ It's highly recommended to only use the official Hackage
-- server or a mirror. See
-- <https://github.com/commercialhaskell/stack/issues/4137>.
--
-- @since 0.1.0.0
data HackageSecurityConfig = HackageSecurityConfig
  { hscKeyIds :: ![Text]
  , hscKeyThreshold :: !Int
  , hscDownloadPrefix :: !Text
  }
  deriving Show
instance FromJSON (WithJSONWarnings HackageSecurityConfig) where
  parseJSON = withObjectWarnings "HackageSecurityConfig" $ \o' -> do
    hscDownloadPrefix <- o' ..: "download-prefix"
    Object o <- o' ..: "hackage-security"
    hscKeyIds <- o ..: "keyids"
    hscKeyThreshold <- o ..: "key-threshold"
    pure HackageSecurityConfig {..}

-- | An environment which contains a 'PantryConfig'.
--
-- @since 0.1.0.0
class HasPantryConfig env where
  -- | Lens to get or set the 'PantryConfig'
  --
  -- @since 0.1.0.0
  pantryConfigL :: Lens' env PantryConfig

-- | File size in bytes
--
-- @since 0.1.0.0
newtype FileSize = FileSize Word
  deriving (Show, Eq, Ord, Data, Typeable, Generic, Display, Hashable, NFData, Store, PersistField, PersistFieldSql, ToJSON, FromJSON)

-- | A key for looking up a blob, which combines the SHA256 hash of
-- the contents and the file size.
--
-- The file size may seem redundant with the hash. However, it is
-- necessary for safely downloading blobs from an untrusted
-- source. See
-- <https://www.fpcomplete.com/blog/2018/07/pantry-part-2-trees-keys>.
--
-- @since 0.1.0.0
data BlobKey = BlobKey !SHA256 !FileSize
  deriving (Eq, Ord, Data, Typeable, Generic)
instance Store BlobKey
instance NFData BlobKey

instance Show BlobKey where
  show = T.unpack . utf8BuilderToText . display
instance Display BlobKey where
  display (BlobKey sha size') = display sha <> "," <> display size'

blobKeyPairs :: BlobKey -> [(Text, Value)]
blobKeyPairs (BlobKey sha size') =
    [ "sha256" .= sha
    , "size" .= size'
    ]

instance ToJSON BlobKey where
  toJSON = object . blobKeyPairs
instance FromJSON BlobKey where
  parseJSON = withObject "BlobKey" $ \o -> BlobKey
    <$> o .: "sha256"
    <*> o .: "size"

newtype PackageNameP = PackageNameP { unPackageNameP :: PackageName }
  deriving (Show)
instance PersistField PackageNameP where
  toPersistValue (PackageNameP pn) = PersistText $ T.pack $ packageNameString pn
  fromPersistValue v = do
    str <- fromPersistValue v
    case parsePackageName str of
      Nothing -> Left $ "Invalid package name: " <> T.pack str
      Just pn -> Right $ PackageNameP pn
instance PersistFieldSql PackageNameP where
  sqlType _ = SqlString

newtype VersionP = VersionP Version
  deriving (Show)
instance PersistField VersionP where
  toPersistValue (VersionP v) = PersistText $ T.pack $ versionString v
  fromPersistValue v = do
    str <- fromPersistValue v
    case parseVersion str of
      Nothing -> Left $ "Invalid version number: " <> T.pack str
      Just ver -> Right $ VersionP ver
instance PersistFieldSql VersionP where
  sqlType _ = SqlString

-- | How to choose a cabal file for a package from Hackage. This is to
-- work with Hackage cabal file revisions, which makes
-- @PackageIdentifier@ insufficient for specifying a package from
-- Hackage.
--
-- @since 0.1.0.0
data CabalFileInfo
  = CFILatest
  -- ^ Take the latest revision of the cabal file available. This
  -- isn't reproducible at all, but the running assumption (not
  -- necessarily true) is that cabal file revisions do not change
  -- semantics of the build.
  --
  -- @since 0.1.0.0
  | CFIHash !SHA256 !(Maybe FileSize)
  -- ^ Identify by contents of the cabal file itself. Only reason for
  -- @Maybe@ on @FileSize@ is for compatibility with input that
  -- doesn't include the file size.
  --
  -- @since 0.1.0.0
  | CFIRevision !Revision
  -- ^ Identify by revision number, with 0 being the original and
  -- counting upward. This relies on Hackage providing consistent
  -- versioning. @CFIHash@ should be preferred wherever possible for
  -- reproducibility.
  --
  -- @since 0.1.0.0
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Store CabalFileInfo
instance NFData CabalFileInfo
instance Hashable CabalFileInfo

instance Display CabalFileInfo where
  display CFILatest = mempty
  display (CFIHash hash' msize) =
    "@sha256:" <> display hash' <> maybe mempty (\i -> "," <> display i) msize
  display (CFIRevision rev) = "@rev:" <> display rev

-- | A full specification for a package from Hackage, including the
-- package name, version, and how to load up the correct cabal file
-- revision.
--
-- @since 0.1.0.0
data PackageIdentifierRevision = PackageIdentifierRevision !PackageName !Version !CabalFileInfo
  deriving (Generic, Eq, Ord, Data, Typeable)
instance NFData PackageIdentifierRevision

instance Show PackageIdentifierRevision where
  show = T.unpack . utf8BuilderToText . display

instance Display PackageIdentifierRevision where
  display (PackageIdentifierRevision name version cfi) =
    fromString (packageNameString name) <> "-" <> fromString (versionString version) <> display cfi

instance ToJSON PackageIdentifierRevision where
  toJSON = toJSON . utf8BuilderToText . display
instance FromJSON PackageIdentifierRevision where
  parseJSON = withText "PackageIdentifierRevision" $ \t ->
    case parsePackageIdentifierRevision t of
      Left e -> fail $ show e
      Right pir -> pure pir

-- | Parse a 'PackageIdentifierRevision'
--
-- @since 0.1.0.0
parsePackageIdentifierRevision :: Text -> Either PantryException PackageIdentifierRevision
parsePackageIdentifierRevision t = maybe (Left $ PackageIdentifierRevisionParseFail t) Right $ do
  let (identT, cfiT) = T.break (== '@') t
  PackageIdentifier name version <- parsePackageIdentifier $ T.unpack identT
  cfi <-
    case splitColon cfiT of
      Just ("@sha256", shaSizeT) -> do
        let (shaT, sizeT) = T.break (== ',') shaSizeT
        sha <- either (const Nothing) Just $ SHA256.fromHexText shaT
        msize <-
          case T.stripPrefix "," sizeT of
            Nothing -> Just Nothing
            Just sizeT' ->
              case decimal sizeT' of
                Right (size', "") -> Just $ Just $ FileSize size'
                _ -> Nothing
        pure $ CFIHash sha msize
      Just ("@rev", revT) ->
        case decimal revT of
          Right (rev, "") -> pure $ CFIRevision $ Revision rev
          _ -> Nothing
      Nothing -> pure CFILatest
      _ -> Nothing
  pure $ PackageIdentifierRevision name version cfi
  where
    splitColon t' =
      let (x, y) = T.break (== ':') t'
       in (x, ) <$> T.stripPrefix ":" y

data Mismatch a = Mismatch
  { mismatchExpected :: !a
  , mismatchActual :: !a
  }

-- | Things that can go wrong in pantry. Note two things:
--
-- * Many other exception types may be thrown from underlying
--   libraries. Pantry does not attempt to wrap these underlying
--   exceptions.
--
-- * We may add more constructors to this data type in minor version
--   bumps of pantry. This technically breaks the PVP. You should not
--   be writing pattern matches against this type that expect total
--   matching.
--
-- @since 0.1.0.0
data PantryException
  = PackageIdentifierRevisionParseFail !Text
  | InvalidCabalFile
      !(Either PackageLocationImmutable (Path Abs File))
      !(Maybe Version)
      ![PError]
      ![PWarning]
  | TreeWithoutCabalFile !PackageLocationImmutable
  | TreeWithMultipleCabalFiles !PackageLocationImmutable ![SafeFilePath]
  | TreeWithMultipleHPackFiles !PackageLocationImmutable ![SafeFilePath]
  | MismatchedCabalName !(Path Abs File) !PackageName
  | NoCabalFileFound !(Path Abs Dir)
  | MultipleCabalFilesFound !(Path Abs Dir) ![Path Abs File]
  | InvalidWantedCompiler !Text
  | InvalidSnapshotLocation !(Path Abs Dir) !Text
  | InvalidOverrideCompiler !WantedCompiler !WantedCompiler
  | InvalidFilePathSnapshot !Text
  | InvalidSnapshot !SnapshotLocation !SomeException
  | MismatchedPackageMetadata
      !PackageLocationImmutable
      !PackageMetadata
      !(Maybe TreeKey)
      !BlobKey -- cabal file found
      !PackageIdentifier
  | Non200ResponseStatus !Status
  | InvalidBlobKey !(Mismatch BlobKey)
  | Couldn'tParseSnapshot !SnapshotLocation !String
  | WrongCabalFileName !PackageLocationImmutable !SafeFilePath !PackageName
  | DownloadInvalidSHA256 !Text !(Mismatch SHA256)
  | DownloadInvalidSize !Text !(Mismatch FileSize)
  | DownloadTooLarge !Text !(Mismatch FileSize)
  -- ^ Different from 'DownloadInvalidSize' since 'mismatchActual' is
  -- a lower bound on the size from the server.
  | LocalInvalidSHA256 !(Path Abs File) !(Mismatch SHA256)
  | LocalInvalidSize !(Path Abs File) !(Mismatch FileSize)
  | UnknownArchiveType !ArchiveLocation
  | InvalidTarFileType !ArchiveLocation !FilePath !Tar.FileType
  | UnsupportedTarball !ArchiveLocation !Text
  | NoHackageCryptographicHash !PackageIdentifier
  | FailedToCloneRepo !Repo
  | TreeReferencesMissingBlob !PackageLocationImmutable !SafeFilePath !BlobKey
  | CompletePackageMetadataMismatch !PackageLocationImmutable !PackageMetadata
  | CRC32Mismatch !ArchiveLocation !FilePath !(Mismatch Word32)
  | UnknownHackagePackage !PackageIdentifierRevision !FuzzyResults
  | CannotCompleteRepoNonSHA1 !Repo
  | MutablePackageLocationFromUrl !Text
  | MismatchedCabalFileForHackage !PackageIdentifierRevision !(Mismatch PackageIdentifier)
  | PackageNameParseFail !Text
  | PackageVersionParseFail !Text
  | InvalidCabalFilePath !(Path Abs File)
  | DuplicatePackageNames !Utf8Builder ![(PackageName, [PackageLocationImmutable])]

  deriving Typeable
instance Exception PantryException where
instance Show PantryException where
  show = T.unpack . utf8BuilderToText . display
instance Display PantryException where
  display (PackageIdentifierRevisionParseFail text) =
    "Invalid package identifier (with optional revision): " <>
    display text
  display (InvalidCabalFile loc mversion errs warnings) =
    "Unable to parse cabal file from package " <>
    either display (fromString . toFilePath) loc <>
    "\n\n" <>
    foldMap
      (\(PError pos msg) ->
          "- " <>
          fromString (showPos pos) <>
          ": " <>
          fromString msg <>
          "\n")
      errs <>
    foldMap
      (\(PWarning _ pos msg) ->
          "- " <>
          fromString (showPos pos) <>
          ": " <>
          fromString msg <>
          "\n")
      warnings <>

    (case mversion of
       Just version
         | version > cabalSpecLatestVersion ->
             "\n\nThe cabal file uses the cabal specification version " <>
             fromString (versionString version) <>
             ", but we only support up to version " <>
             fromString (versionString cabalSpecLatestVersion) <>
             ".\nRecommended action: upgrade your build tool (e.g., `stack upgrade`)."
       _ -> mempty)
  display (TreeWithoutCabalFile pl) = "No cabal file found for " <> display pl
  display (TreeWithMultipleCabalFiles pl sfps) =
    "Multiple cabal files found for " <> display pl <> ": " <>
    fold (intersperse ", " (map display sfps))
  display (TreeWithMultipleHPackFiles pl sfps) =
    "Multiple package.yaml files found for " <> display pl <> ": " <>
    fold (intersperse ", " (map display sfps))
  display (MismatchedCabalName fp name) =
    "cabal file path " <>
    fromString (toFilePath fp) <>
    " does not match the package name it defines.\n" <>
    "Please rename the file to: " <>
    fromString (packageNameString name) <>
    ".cabal\n" <>
    "For more information, see: https://github.com/commercialhaskell/stack/issues/317"
  display (NoCabalFileFound dir) =
    "Stack looks for packages in the directories configured in\n" <>
    "the 'packages' and 'extra-deps' fields defined in your stack.yaml\n" <>
    "The current entry points to " <>
    fromString (toFilePath dir) <>
    ",\nbut no .cabal or package.yaml file could be found there."
  display (MultipleCabalFilesFound dir files) =
    "Multiple .cabal files found in directory " <>
    fromString (toFilePath dir) <>
    ":\n" <>
    fold (intersperse "\n" (map (\x -> "- " <> fromString (toFilePath (filename x))) files))
  display (InvalidWantedCompiler t) = "Invalid wanted compiler: " <> display t
  display (InvalidSnapshotLocation dir t) =
    "Invalid snapshot location " <>
    displayShow t <>
    " relative to directory " <>
    displayShow (toFilePath dir)
  display (InvalidOverrideCompiler x y) =
    "Specified compiler for a resolver (" <>
    display x <>
    "), but also specified an override compiler (" <>
    display y <>
    ")"
  display (InvalidFilePathSnapshot t) =
    "Specified snapshot as file path with " <>
    displayShow t <>
    ", but not reading from a local file"
  display (InvalidSnapshot loc e) =
    "Exception while reading snapshot from " <>
    display loc <>
    ":\n" <>
    displayShow e
  display (MismatchedPackageMetadata loc pm mtreeKey foundCabal foundIdent) =
    "Mismatched package metadata for " <> display loc <>
    "\nFound: " <> fromString (packageIdentifierString foundIdent) <> " with cabal file " <>
    display foundCabal <>
    (case mtreeKey of
       Nothing -> mempty
       Just treeKey -> " and tree " <> display treeKey) <>
    "\nExpected: " <> display pm
  display (Non200ResponseStatus status) =
    "Unexpected non-200 HTTP status code: " <>
    displayShow (statusCode status)
  display (InvalidBlobKey Mismatch{..}) =
    "Invalid blob key found, expected: " <>
    display mismatchExpected <>
    ", actual: " <>
    display mismatchActual
  display (Couldn'tParseSnapshot sl e) =
    "Couldn't parse snapshot from " <> display sl <> ": " <> fromString e
  display (WrongCabalFileName pl sfp name) =
    "Wrong cabal file name for package " <> display pl <>
    "\nCabal file is named " <> display sfp <>
    ", but package name is " <> fromString (packageNameString name) <>
    "\nFor more information, see:\n  - https://github.com/commercialhaskell/stack/issues/317\n  -https://github.com/commercialhaskell/stack/issues/895"
  display (DownloadInvalidSHA256 url Mismatch {..}) =
    "Mismatched SHA256 hash from " <> display url <>
    "\nExpected: " <> display mismatchExpected <>
    "\nActual:   " <> display mismatchActual
  display (DownloadInvalidSize url Mismatch {..}) =
    "Mismatched download size from " <> display url <>
    "\nExpected: " <> display mismatchExpected <>
    "\nActual:   " <> display mismatchActual
  display (DownloadTooLarge url Mismatch {..}) =
    "Download from " <> display url <> " was too large.\n" <>
    "Expected: " <> display mismatchExpected <> ", stopped after receiving: " <>
    display mismatchActual
  display (LocalInvalidSHA256 path Mismatch {..}) =
    "Mismatched SHA256 hash from " <> fromString (toFilePath path) <>
    "\nExpected: " <> display mismatchExpected <>
    "\nActual:   " <> display mismatchActual
  display (LocalInvalidSize path Mismatch {..}) =
    "Mismatched file size from " <> fromString (toFilePath path) <>
    "\nExpected: " <> display mismatchExpected <>
    "\nActual:   " <> display mismatchActual
  display (UnknownArchiveType loc) = "Unable to determine archive type of: " <> display loc
  display (InvalidTarFileType loc fp x) =
    "Unsupported tar filetype in archive " <> display loc <> " at file " <> fromString fp <> ": " <> displayShow x
  display (UnsupportedTarball loc e) =
    "Unsupported tarball from " <> display loc <> ": " <> display e
  display (NoHackageCryptographicHash ident) =
    "Not cryptographic hash found for Hackage package " <> fromString (packageIdentifierString ident)
  display (FailedToCloneRepo repo) = "Failed to clone repo " <> display repo
  display (TreeReferencesMissingBlob loc sfp key) =
    "The package " <> display loc <>
    " needs blob " <> display key <>
    " for file path " <> display sfp <>
    ", but the blob is not available"
  display (CompletePackageMetadataMismatch loc pm) =
    "When completing package metadata for " <> display loc <>
    ", some values changed in the new package metadata: " <>
    display pm
  display (CRC32Mismatch loc fp Mismatch {..}) =
    "CRC32 mismatch in ZIP file from " <> display loc <>
    " on internal file " <> fromString fp <>
    "\n.Expected: " <> display mismatchExpected <>
    "\n.Actual:   " <> display mismatchActual
  display (UnknownHackagePackage pir fuzzy) =
    "Could not find " <> display pir <> " on Hackage" <>
    displayFuzzy fuzzy
  display (CannotCompleteRepoNonSHA1 repo) =
    "Cannot complete repo information for a non SHA1 commit due to non-reproducibility: " <>
    display repo
  display (MutablePackageLocationFromUrl t) =
    "Cannot refer to a mutable package location from a URL: " <> display t
  display (MismatchedCabalFileForHackage pir Mismatch{..}) =
    "When processing cabal file for Hackage package " <> display pir <>
    ":\nMismatched package identifier." <>
    "\nExpected: " <> fromString (packageIdentifierString mismatchExpected) <>
    "\nActual:   " <> fromString (packageIdentifierString mismatchActual)
  display (PackageNameParseFail t) =
    "Invalid package name: " <> display t
  display (PackageVersionParseFail t) =
    "Invalid version: " <> display t
  display (InvalidCabalFilePath fp) =
    "File path contains a name which is not a valid package name: " <>
    fromString (toFilePath fp)
  display (DuplicatePackageNames source pairs') =
    "Duplicate package names (" <> source <> "):\n" <>
    foldMap
      (\(name, locs) ->
        fromString (packageNameString name) <> ":\n" <>
        foldMap
          (\loc -> "- " <> display loc <> "\n")
          locs
      )
      pairs'

data FuzzyResults
  = FRNameNotFound ![PackageName]
  | FRVersionNotFound !(NonEmpty PackageIdentifierRevision)
  | FRRevisionNotFound !(NonEmpty PackageIdentifierRevision)

displayFuzzy :: FuzzyResults -> Utf8Builder
displayFuzzy (FRNameNotFound names) =
  case NE.nonEmpty names of
    Nothing -> ""
    Just names' ->
      "\nPerhaps you meant " <>
      orSeparated (NE.map (fromString . packageNameString) names') <>
      "?"
displayFuzzy (FRVersionNotFound pirs) =
  "\nPossible candidates: " <>
  commaSeparated (NE.map display pirs) <>
  "."
displayFuzzy (FRRevisionNotFound pirs) =
  "\nThe specified revision was not found.\nPossible candidates: " <>
  commaSeparated (NE.map display pirs) <>
  "."

orSeparated :: NonEmpty Utf8Builder -> Utf8Builder
orSeparated xs
  | NE.length xs == 1 = NE.head xs
  | NE.length xs == 2 = NE.head xs <> " or " <> NE.last xs
  | otherwise = fold (intersperse ", " (NE.init xs)) <> ", or " <> NE.last xs

commaSeparated :: NonEmpty Utf8Builder -> Utf8Builder
commaSeparated = fold . NE.intersperse ", "

-- You'd really think there'd be a better way to do this in Cabal.
cabalSpecLatestVersion :: Version
cabalSpecLatestVersion =
  case cabalSpecLatest of
    CabalSpecOld -> error "this cannot happen"
    CabalSpecV1_22 -> error "this cannot happen"
    CabalSpecV1_24 -> error "this cannot happen"
    CabalSpecV2_0 -> error "this cannot happen"
    CabalSpecV2_2 -> error "this cannot happen"
    CabalSpecV2_4 -> mkVersion [2, 4]

data BuildFileType = CabalFile | HPackFile
  deriving (Show, Eq, Enum, Bounded)

instance PersistField BuildFileType where
  toPersistValue CabalFile = PersistInt64 1
  toPersistValue HPackFile = PersistInt64 2

  fromPersistValue v = do
    i <- fromPersistValue v
    case i :: Int64 of
      1 -> Right CabalFile
      2 -> Right HPackFile
      _ -> Left $ "Invalid BuildFileType: " <> tshow i
instance PersistFieldSql BuildFileType where
  sqlType _ = SqlInt32

data FileType = FTNormal | FTExecutable
  deriving (Show, Eq, Enum, Bounded)
instance PersistField FileType where
  toPersistValue FTNormal = PersistInt64 1
  toPersistValue FTExecutable = PersistInt64 2

  fromPersistValue v = do
    i <- fromPersistValue v
    case i :: Int64 of
      1 -> Right FTNormal
      2 -> Right FTExecutable
      _ -> Left $ "Invalid FileType: " <> tshow i
instance PersistFieldSql FileType where
  sqlType _ = SqlInt32

data TreeEntry = TreeEntry
  { teBlob :: !BlobKey
  , teType :: !FileType
  }
  deriving (Show, Eq)

newtype SafeFilePath = SafeFilePath Text
  deriving (Show, Eq, Ord, Display)

instance PersistField SafeFilePath where
  toPersistValue = toPersistValue . unSafeFilePath
  fromPersistValue v = do
    t <- fromPersistValue v
    maybe (Left $ "Invalid SafeFilePath: " <> t) Right $ mkSafeFilePath t
instance PersistFieldSql SafeFilePath where
  sqlType _ = SqlString

unSafeFilePath :: SafeFilePath -> Text
unSafeFilePath (SafeFilePath t) = t

mkSafeFilePath :: Text -> Maybe SafeFilePath
mkSafeFilePath t = do
  guard $ not $ "\\" `T.isInfixOf` t
  guard $ not $ "//" `T.isInfixOf` t
  guard $ not $ "\n" `T.isInfixOf` t
  guard $ not $ "\0" `T.isInfixOf` t

  (c, _) <- T.uncons t
  guard $ c /= '/'

  guard $ all (not . T.all (== '.')) $ T.split (== '/') t

  Just $ SafeFilePath t

-- | The hash of the binary representation of a 'Tree'.
--
-- @since 0.1.0.0
newtype TreeKey = TreeKey BlobKey
  deriving (Show, Eq, Ord, Generic, Data, Typeable, ToJSON, FromJSON, NFData, Store, Display)

-- | Represents the contents of a tree, which is a mapping from
-- relative file paths to 'TreeEntry's.
--
-- @since 0.1.0.0
newtype Tree
  = TreeMap (Map SafeFilePath TreeEntry)
  -- In the future, consider allowing more lax parsing
  -- See: https://www.fpcomplete.com/blog/2018/07/pantry-part-2-trees-keys
  -- TreeTarball !PackageTarball
  deriving (Show, Eq)

renderTree :: Tree -> ByteString
renderTree = BL.toStrict . toLazyByteString . go
  where
    go :: Tree -> Builder
    go (TreeMap m) = "map:" <> Map.foldMapWithKey goEntry m

    goEntry sfp (TreeEntry (BlobKey sha (FileSize size')) ft) =
      netstring (unSafeFilePath sfp) <>
      byteString (SHA256.toRaw sha) <>
      netword size' <>
      (case ft of
         FTNormal -> "N"
         FTExecutable -> "X")

netstring :: Text -> Builder
netstring t =
  let bs = encodeUtf8 t
   in netword (fromIntegral (B.length bs)) <> byteString bs

netword :: Word -> Builder
netword w = wordDec w <> ":"

parseTree :: ByteString -> Maybe Tree
parseTree bs1 = do
  tree <- parseTree' bs1
  let bs2 = renderTree tree
  guard $ bs1 == bs2
  Just tree

parseTree' :: ByteString -> Maybe Tree
parseTree' bs0 = do
  entriesBS <- B.stripPrefix "map:" bs0
  TreeMap <$> loop Map.empty entriesBS
  where
    loop !m bs1
      | B.null bs1 = pure m
      | otherwise = do
          (sfpBS, bs2) <- takeNetstring bs1
          sfp <-
            case decodeUtf8' sfpBS of
              Left _ -> Nothing
              Right sfpT -> mkSafeFilePath sfpT
          (sha, bs3) <- takeSha bs2
          (size', bs4) <- takeNetword bs3
          (typeW, bs5) <- B.uncons bs4
          ft <-
            case typeW of
              78 -> Just FTNormal -- 'N'
              88 -> Just FTExecutable -- 'X'
              _ -> Nothing
          let entry = TreeEntry (BlobKey sha (FileSize (fromIntegral size'))) ft
          loop (Map.insert sfp entry m) bs5

    takeNetstring bs1 = do
      (size', bs2) <- takeNetword bs1
      guard $ B.length bs2 >= size'
      Just $ B.splitAt size' bs2

    takeSha bs = do
      let (x, y) = B.splitAt 32 bs
      x' <- either (const Nothing) Just (SHA256.fromRaw x)
      Just (x', y)

    takeNetword =
      go 0
      where
        go !accum bs = do
          (next, rest) <- B.uncons bs
          if
            | next == 58 -> pure (accum, rest) -- ':'
            | next >= 48 && next <= 57 ->
                go
                  (accum * 10 + fromIntegral (next - 48))
                  rest
            | otherwise -> Nothing

    {-
data PackageTarball = PackageTarball
  { ptBlob :: !BlobKey
  -- ^ Contains the tarball itself
  , ptCabal :: !BlobKey
  -- ^ Contains the cabal file contents
  , ptSubdir :: !FilePath
  -- ^ Subdir containing the files we want for this package.
  --
  -- There must be precisely one file with a @.cabal@ file extension
  -- located there. Thanks to Hackage revisions, its contents will be
  -- overwritten by the value of @ptCabal@.
  }
  deriving Show
    -}

-- | This is almost a copy of Cabal's parser for package identifiers,
-- the main difference is in the fact that Stack requires version to be
-- present while Cabal uses "null version" as a defaul value
--
-- @since 0.1.0.0
parsePackageIdentifier :: String -> Maybe PackageIdentifier
parsePackageIdentifier str =
    case [p | (p, s) <- Parse.readP_to_S parser str, all isSpace s] of
        [] -> Nothing
        (p:_) -> Just p
  where
    parser = do
        n <- Distribution.Text.parse
        -- version is a required component of a package identifier for Stack
        v <- Parse.char '-' >> Distribution.Text.parse
        return (PackageIdentifier n v)

-- | Parse a package name from a 'String'.
--
-- @since 0.1.0.0
parsePackageName :: String -> Maybe PackageName
parsePackageName = Distribution.Text.simpleParse

-- | Parse a package name from a 'String' throwing on failure
--
-- @since 0.1.0.0
parsePackageNameThrowing :: MonadThrow m => String -> m PackageName
parsePackageNameThrowing str =
  case parsePackageName str of
    Nothing -> throwM $ PackageNameParseFail $ T.pack str
    Just pn -> pure pn

-- | Parse a version from a 'String'.
--
-- @since 0.1.0.0
parseVersion :: String -> Maybe Version
parseVersion = Distribution.Text.simpleParse

-- | Parse a package version from a 'String' throwing on failure
--
-- @since 0.1.0.0
parseVersionThrowing :: MonadThrow m => String -> m Version
parseVersionThrowing str =
  case parseVersion str of
    Nothing -> throwM $ PackageVersionParseFail $ T.pack str
    Just v -> pure v

-- | Parse a version range from a 'String'.
--
-- @since 0.1.0.0
parseVersionRange :: String -> Maybe VersionRange
parseVersionRange = Distribution.Text.simpleParse

-- | Parse a flag name from a 'String'.
--
-- @since 0.1.0.0
parseFlagName :: String -> Maybe FlagName
parseFlagName = Distribution.Text.simpleParse

-- | Render a package name as a 'String'.
--
-- @since 0.1.0.0
packageNameString :: PackageName -> String
packageNameString = unPackageName

-- | Render a package identifier as a 'String'.
--
-- @since 0.1.0.0
packageIdentifierString :: PackageIdentifier -> String
packageIdentifierString = Distribution.Text.display

-- | Render a version as a 'String'.
--
-- @since 0.1.0.0
versionString :: Version -> String
versionString = Distribution.Text.display

-- | Render a flag name as a 'String'.
--
-- @since 0.1.0.0
flagNameString :: FlagName -> String
flagNameString = unFlagName

-- | Render a module name as a 'String'.
--
-- @since 0.1.0.0
moduleNameString :: ModuleName -> String
moduleNameString = Distribution.Text.display

data OptionalSubdirs
  = OSSubdirs !(NonEmpty Text)
  | OSPackageMetadata !Text !PackageMetadata
  -- ^ subdirectory and package metadata
  deriving (Show, Eq, Data, Generic)
instance NFData OptionalSubdirs
instance Store OptionalSubdirs

-- | Metadata provided by a config file for archives and repos. This
-- information can be used for optimized lookups of information like
-- package identifiers, or for validating that the user configuration
-- has the expected information.
--
-- @since 0.1.0.0
data PackageMetadata = PackageMetadata
  { pmName :: !(Maybe PackageName)
    -- ^ Package name in the cabal file
    --
    -- @since 0.1.0.0
  , pmVersion :: !(Maybe Version)
    -- ^ Package version in the cabal file
    --
    -- @since 0.1.0.0
  , pmTreeKey :: !(Maybe TreeKey)
    -- ^ Tree key of the loaded up package
    --
    -- @since 0.1.0.0
  , pmCabal :: !(Maybe BlobKey)
    -- ^ Blob key containing the cabal file
    --
    -- @since 0.1.0.0
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable)
instance Store PackageMetadata
instance NFData PackageMetadata

instance Display PackageMetadata where
  display pm = fold $ intersperse ", " $ catMaybes
    [ (\name -> "name == " <> fromString (packageNameString name)) <$> pmName pm
    , (\version -> "version == " <> fromString (versionString version)) <$> pmVersion pm
    , (\tree -> "tree == " <> display tree) <$> pmTreeKey pm
    , (\cabal -> "cabal file == " <> display cabal) <$> pmCabal pm
    ]

-- | File path relative to the configuration file it was parsed from
--
-- @since 0.1.0.0
newtype RelFilePath = RelFilePath Text
  deriving (Show, ToJSON, FromJSON, Eq, Ord, Generic, Data, Typeable, Store, NFData, Display)

-- | Location that an archive is stored at
--
-- @since 0.1.0.0
data ArchiveLocation
  = ALUrl !Text
    -- ^ Archive stored at an HTTP(S) URL
    --
    -- @since 0.1.0.0
  | ALFilePath !(ResolvedPath File)
    -- ^ Archive stored at a local file path
    --
    -- @since 0.1.0.0
  deriving (Show, Eq, Ord, Generic, Data, Typeable)
instance Store ArchiveLocation
instance NFData ArchiveLocation

instance Display ArchiveLocation where
  display (ALUrl url) = display url
  display (ALFilePath resolved) = fromString $ toFilePath $ resolvedAbsolute resolved

parseArchiveLocationObject :: Object -> WarningParser (Unresolved ArchiveLocation)
parseArchiveLocationObject o =
    ((o ..: "url") >>= validateUrl) <|>
    ((o ..: "filepath") >>= validateFilePath) <|>
    ((o ..: "archive") >>= parseArchiveLocationText) <|>
    ((o ..: "location") >>= parseArchiveLocationText)

-- Forgive me my father, for I have sinned (bad fail, bad!)
parseArchiveLocationText :: (Monad m, Alternative m) => Text -> m (Unresolved ArchiveLocation)
parseArchiveLocationText t = validateUrl t <|> validateFilePath t

validateUrl :: Monad m => Text -> m (Unresolved ArchiveLocation)
validateUrl t =
  case parseRequest $ T.unpack t of
    Left _ -> fail $ "Could not parse URL: " ++ T.unpack t
    Right _ -> pure $ pure $ ALUrl t

validateFilePath :: Monad m => Text -> m (Unresolved ArchiveLocation)
validateFilePath t =
  if any (\ext -> ext `T.isSuffixOf` t) (T.words ".zip .tar .tar.gz")
    then pure $ Unresolved $ \mdir ->
           case mdir of
             Nothing -> throwIO $ InvalidFilePathSnapshot t
             Just dir -> do
               abs' <- resolveFile dir $ T.unpack t
               pure $ ALFilePath $ ResolvedPath (RelFilePath t) abs'
    else fail $ "Does not have an archive file extension: " ++ T.unpack t

instance ToJSON PackageLocation where
  toJSON (PLImmutable pli) = toJSON pli
  toJSON (PLMutable resolved) = toJSON (resolvedRelative resolved)
instance FromJSON (WithJSONWarnings (Unresolved (NonEmpty PackageLocation))) where
  parseJSON v =
    ((fmap.fmap.fmap.fmap) PLImmutable (parseJSON v)) <|>
    ((noJSONWarnings . mkMutable) <$> parseJSON v)
    where
      mkMutable :: Text -> Unresolved (NonEmpty PackageLocation)
      mkMutable t = Unresolved $ \mdir -> do
        case mdir of
          Nothing -> throwIO $ MutablePackageLocationFromUrl t
          Just dir -> do
            abs' <- resolveDir dir $ T.unpack t
            pure $ pure $ PLMutable $ ResolvedPath (RelFilePath t) abs'

instance ToJSON PackageLocationImmutable where
  toJSON (PLIHackage pir mtree) = object $ concat
    [ ["hackage" .= pir]
    , maybe [] (\tree -> ["pantry-tree" .= tree]) mtree
    ]
  toJSON (PLIArchive (Archive loc msha msize subdir) pm) = object $ concat
    [ case loc of
        ALUrl url -> ["url" .= url]
        ALFilePath resolved -> ["filepath" .= resolvedRelative resolved]
    , maybe [] (\sha -> ["sha256" .= sha]) msha
    , maybe [] (\size' -> ["size" .= size']) msize
    , if T.null subdir then [] else ["subdir" .= subdir]
    , pmToPairs pm
    ]
  toJSON (PLIRepo (Repo url commit typ subdir) pm) = object $ concat
    [ [ urlKey .= url
      , "commit" .= commit
      ]
    , if T.null subdir then [] else ["subdir" .= subdir]
    , pmToPairs pm
    ]
    where
      urlKey =
        case typ of
          RepoGit -> "git"
          RepoHg  -> "hg"

pmToPairs :: PackageMetadata -> [(Text, Value)]
pmToPairs (PackageMetadata mname mversion mtree mcabal) = concat
  [ maybe [] (\name -> ["name" .= CabalString name]) mname
  , maybe [] (\version -> ["version" .= CabalString version]) mversion
  , maybe [] (\tree -> ["pantry-tree" .= tree]) mtree
  , maybe [] (\cabal -> ["cabal-file" .= cabal]) mcabal
  ]

instance FromJSON (WithJSONWarnings (Unresolved (NonEmpty PackageLocationImmutable))) where
  parseJSON v
      = http v
    <|> hackageText v
    <|> hackageObject v
    <|> repo v
    <|> archiveObject v
    <|> github v
    <|> fail ("Could not parse a UnresolvedPackageLocationImmutable from: " ++ show v)
    where
      http :: Value -> Parser (WithJSONWarnings (Unresolved (NonEmpty PackageLocationImmutable)))
      http = withText "UnresolvedPackageLocationImmutable.UPLIArchive (Text)" $ \t ->
        case parseArchiveLocationText t of
          Nothing -> fail $ "Invalid archive location: " ++ T.unpack t
          Just (Unresolved mkArchiveLocation) ->
            pure $ noJSONWarnings $ Unresolved $ \mdir -> do
              archiveLocation <- mkArchiveLocation mdir
              let archiveHash = Nothing
                  archiveSize = Nothing
                  archiveSubdir = T.empty
              pure $ pure $ PLIArchive Archive {..} pmEmpty

      hackageText = withText "UnresolvedPackageLocationImmutable.UPLIHackage (Text)" $ \t ->
        case parsePackageIdentifierRevision t of
          Left e -> fail $ show e
          Right pir -> pure $ noJSONWarnings $ pure $ pure $ PLIHackage pir Nothing

      hackageObject = withObjectWarnings "UnresolvedPackageLocationImmutable.UPLIHackage" $ \o -> (pure.pure) <$> (PLIHackage
        <$> o ..: "hackage"
        <*> o ..:? "pantry-tree")

      optionalSubdirs :: Object -> WarningParser OptionalSubdirs
      optionalSubdirs o =
        -- if subdirs exists, it needs to be valid
        case HM.lookup "subdirs" o of
          Just v' -> do
            tellJSONField "subdirs"
            subdirs <- lift $ parseJSON v'
            case NE.nonEmpty subdirs of
              Nothing -> fail "Invalid empty subdirs"
              Just x -> pure $ OSSubdirs x
          Nothing -> OSPackageMetadata
            <$> o ..:? "subdir" ..!= T.empty
            <*> (PackageMetadata
                  <$> (fmap unCabalString <$> (o ..:? "name"))
                  <*> (fmap unCabalString <$> (o ..:? "version"))
                  <*> o ..:? "pantry-tree"
                  <*> o ..:? "cabal-file")

      repo = withObjectWarnings "UnresolvedPackageLocationImmutable.UPLIRepo" $ \o -> do
        (repoType, repoUrl) <-
          ((RepoGit, ) <$> o ..: "git") <|>
          ((RepoHg, ) <$> o ..: "hg")
        repoCommit <- o ..: "commit"
        os <- optionalSubdirs o
        pure $ pure $ NE.map (\(repoSubdir, pm) -> PLIRepo Repo {..} pm) (osToPms os)

      archiveObject = withObjectWarnings "UnresolvedPackageLocationImmutable.UPLIArchive" $ \o -> do
        Unresolved mkArchiveLocation <- parseArchiveLocationObject o
        archiveHash <- o ..:? "sha256"
        archiveSize <- o ..:? "size"
        os <- optionalSubdirs o
        pure $ Unresolved $ \mdir -> do
          archiveLocation <- mkArchiveLocation mdir
          pure $ NE.map (\(archiveSubdir, pm) -> PLIArchive Archive {..} pm) (osToPms os)

      github = withObjectWarnings "PLArchive:github" $ \o -> do
        GitHubRepo ghRepo <- o ..: "github"
        commit <- o ..: "commit"
        let archiveLocation = ALUrl $ T.concat
              [ "https://github.com/"
              , ghRepo
              , "/archive/"
              , commit
              , ".tar.gz"
              ]
        archiveHash <- o ..:? "sha256"
        archiveSize <- o ..:? "size"
        os <- optionalSubdirs o
        pure $ pure $ NE.map (\(archiveSubdir, pm) -> PLIArchive Archive {..} pm) (osToPms os)

-- | Returns pairs of subdirectory and 'PackageMetadata'.
osToPms :: OptionalSubdirs -> NonEmpty (Text, PackageMetadata)
osToPms (OSSubdirs subdirs) = NE.map (, pmEmpty) subdirs
osToPms (OSPackageMetadata subdir pm) = pure (subdir, pm)

pmEmpty :: PackageMetadata
pmEmpty = PackageMetadata Nothing Nothing Nothing Nothing

-- | Newtype wrapper for easier JSON integration with Cabal types.
--
-- @since 0.1.0.0
newtype CabalString a = CabalString { unCabalString :: a }
  deriving (Show, Eq, Ord, Typeable)

-- I'd like to use coerce here, but can't due to roles. unsafeCoerce
-- could work, but let's avoid unsafe code.

-- | Wrap the keys in a 'Map' with a 'CabalString' to get a 'ToJSON'
-- instance.
--
-- @since 0.1.0.0
toCabalStringMap :: Map a v -> Map (CabalString a) v
toCabalStringMap = Map.mapKeysMonotonic CabalString

-- | Unwrap the 'CabalString' from the keys in a 'Map' to use a
-- 'FromJSON' instance.
--
-- @since 0.1.0.0
unCabalStringMap :: Map (CabalString a) v -> Map a v
unCabalStringMap = Map.mapKeysMonotonic unCabalString

instance Distribution.Text.Text a => ToJSON (CabalString a) where
  toJSON = toJSON . Distribution.Text.display . unCabalString
instance Distribution.Text.Text a => ToJSONKey (CabalString a) where
  toJSONKey = toJSONKeyText $ T.pack . Distribution.Text.display . unCabalString

instance forall a. IsCabalString a => FromJSON (CabalString a) where
  parseJSON = withText name $ \t ->
    case cabalStringParser $ T.unpack t of
      Nothing -> fail $ "Invalid " ++ name ++ ": " ++ T.unpack t
      Just x -> pure $ CabalString x
    where
      name = cabalStringName (Nothing :: Maybe a)
instance forall a. IsCabalString a => FromJSONKey (CabalString a) where
  fromJSONKey =
    FromJSONKeyTextParser $ \t ->
    case cabalStringParser $ T.unpack t of
      Nothing -> fail $ "Invalid " ++ name ++ ": " ++ T.unpack t
      Just x -> pure $ CabalString x
    where
      name = cabalStringName (Nothing :: Maybe a)

class IsCabalString a where
  cabalStringName :: proxy a -> String
  cabalStringParser :: String -> Maybe a
instance IsCabalString PackageName where
  cabalStringName _ = "package name"
  cabalStringParser = parsePackageName
instance IsCabalString Version where
  cabalStringName _ = "version"
  cabalStringParser = parseVersion
instance IsCabalString VersionRange where
  cabalStringName _ = "version range"
  cabalStringParser = parseVersionRange
instance IsCabalString PackageIdentifier where
  cabalStringName _ = "package identifier"
  cabalStringParser = parsePackageIdentifier
instance IsCabalString FlagName where
  cabalStringName _ = "flag name"
  cabalStringParser = parseFlagName

-- | What to use for running hpack
--
-- @since 0.1.0.0
data HpackExecutable
    = HpackBundled
    -- ^ Compiled in library
    | HpackCommand !FilePath
    -- ^ Executable at the provided path
    deriving (Show, Read, Eq, Ord)

-- | Which compiler a snapshot wants to use. The build tool may elect
-- to do some fuzzy matching of versions (e.g., allowing different
-- patch versions).
--
-- @since 0.1.0.0
data WantedCompiler
  = WCGhc !Version
  | WCGhcjs
      !Version
      !Version
    -- ^ GHCJS version followed by GHC version
 deriving (Show, Eq, Ord, Data, Generic)
instance NFData WantedCompiler
instance Store WantedCompiler
instance Display WantedCompiler where
  display (WCGhc vghc) = "ghc-" <> fromString (versionString vghc)
  display (WCGhcjs vghcjs vghc) =
    "ghcjs-" <> fromString (versionString vghcjs) <> "_ghc-" <> fromString (versionString vghc)
instance ToJSON WantedCompiler where
  toJSON = toJSON . utf8BuilderToText . display
instance FromJSON WantedCompiler where
  parseJSON = withText "WantedCompiler" $ either (fail . show) pure . parseWantedCompiler
instance FromJSONKey WantedCompiler where
  fromJSONKey =
    FromJSONKeyTextParser $ \t ->
    case parseWantedCompiler t of
      Left e -> fail $ "Invalid WantedComiler " ++ show t ++ ": " ++ show e
      Right x -> pure x

-- | Parse a 'Text' into a 'WantedCompiler' value.
--
-- @since 0.1.0.0
parseWantedCompiler :: Text -> Either PantryException WantedCompiler
parseWantedCompiler t0 = maybe (Left $ InvalidWantedCompiler t0) Right $
  case T.stripPrefix "ghcjs-" t0 of
    Just t1 -> parseGhcjs t1
    Nothing -> T.stripPrefix "ghc-" t0 >>= parseGhc
  where
    parseGhcjs t1 = do
      let (ghcjsVT, t2) = T.break (== '_') t1
      ghcjsV <- parseVersion $ T.unpack ghcjsVT
      ghcVT <- T.stripPrefix "_ghc-" t2
      ghcV <- parseVersion $ T.unpack ghcVT
      pure $ WCGhcjs ghcjsV ghcV
    parseGhc = fmap WCGhc . parseVersion . T.unpack

instance FromJSON (WithJSONWarnings (Unresolved SnapshotLocation)) where
  parseJSON v = text v <|> obj v
    where
      text :: Value -> Parser (WithJSONWarnings (Unresolved SnapshotLocation))
      text = withText "UnresolvedSnapshotLocation (Text)" $ pure . noJSONWarnings . parseSnapshotLocation

      obj :: Value -> Parser (WithJSONWarnings (Unresolved SnapshotLocation))
      obj = withObjectWarnings "UnresolvedSnapshotLocation (Object)" $ \o ->
        ((pure . SLCompiler) <$> o ..: "compiler") <|>
        ((\x y -> pure $ SLUrl x y) <$> o ..: "url" <*> blobKey o) <|>
        (parseSnapshotLocationPath <$> o ..: "filepath")

      blobKey o = do
        msha <- o ..:? "sha256"
        msize <- o ..:? "size"
        case (msha, msize) of
          (Nothing, Nothing) -> pure Nothing
          (Just sha, Just size') -> pure $ Just $ BlobKey sha size'
          (Just _sha, Nothing) -> fail "You must also specify the file size"
          (Nothing, Just _) -> fail "You must also specify the file's SHA256"

instance Display SnapshotLocation where
  display (SLCompiler compiler) = display compiler
  display (SLUrl url Nothing) = display url
  display (SLUrl url (Just blob)) = display url <> " (" <> display blob <> ")"
  display (SLFilePath resolved) = display (resolvedRelative resolved)

-- | Parse a 'Text' into an 'Unresolved' 'SnapshotLocation'.
--
-- @since 0.1.0.0
parseSnapshotLocation :: Text -> Unresolved SnapshotLocation
parseSnapshotLocation t0 = fromMaybe (parseSnapshotLocationPath t0) $
  (either (const Nothing) (Just . pure . SLCompiler) (parseWantedCompiler t0)) <|>
  parseLts <|>
  parseNightly <|>
  parseGithub <|>
  parseUrl
  where
    parseLts = do
      t1 <- T.stripPrefix "lts-" t0
      Right (x, t2) <- Just $ decimal t1
      t3 <- T.stripPrefix "." t2
      Right (y, "") <- Just $ decimal t3
      Just $ pure $ ltsSnapshotLocation x y
    parseNightly = do
      t1 <- T.stripPrefix "nightly-" t0
      date <- readMaybe (T.unpack t1)
      Just $ pure $ nightlySnapshotLocation date

    parseGithub = do
      t1 <- T.stripPrefix "github:" t0
      let (user, t2) = T.break (== '/') t1
      t3 <- T.stripPrefix "/" t2
      let (repo, t4) = T.break (== ':') t3
      path <- T.stripPrefix ":" t4
      Just $ pure $ githubSnapshotLocation user repo path

    parseUrl = parseRequest (T.unpack t0) $> pure (SLUrl t0 Nothing)

parseSnapshotLocationPath :: Text -> Unresolved SnapshotLocation
parseSnapshotLocationPath t =
  Unresolved $ \mdir ->
  case mdir of
    Nothing -> throwIO $ InvalidFilePathSnapshot t
    Just dir -> do
      abs' <- resolveFile dir (T.unpack t) `catchAny` \_ -> throwIO (InvalidSnapshotLocation dir t)
      pure $ SLFilePath $ ResolvedPath (RelFilePath t) abs'

githubSnapshotLocation :: Text -> Text -> Text -> SnapshotLocation
githubSnapshotLocation user repo path =
  let url = T.concat
        [ "https://raw.githubusercontent.com/"
        , user
        , "/"
        , repo
        , "/master/"
        , path
        ]
   in SLUrl url Nothing

defUser :: Text
defUser = "commercialhaskell"

defRepo :: Text
defRepo = "stackage-snapshots"

-- | Location of an LTS snapshot
--
-- @since 0.1.0.0
ltsSnapshotLocation
  :: Int -- ^ major version
  -> Int -- ^ minor version
  -> SnapshotLocation
ltsSnapshotLocation x y =
  githubSnapshotLocation defUser defRepo $
  utf8BuilderToText $
  "lts/" <> display x <> "/" <> display y <> ".yaml"

-- | Location of a Stackage Nightly snapshot
--
-- @since 0.1.0.0
nightlySnapshotLocation :: Day -> SnapshotLocation
nightlySnapshotLocation date =
  githubSnapshotLocation defUser defRepo $
  utf8BuilderToText $
  "nightly/" <> display year <> "/" <> display month <> "/" <> display day <> ".yaml"
  where
    (year, month, day) = toGregorian date

-- | Where to load a snapshot from.
--
-- @since 0.1.0.0
data SnapshotLocation
  = SLCompiler !WantedCompiler
    -- ^ Don't use an actual snapshot, just a version of the compiler
    -- with its shipped packages.
    --
    -- @since 0.1.0.0
  | SLUrl !Text !(Maybe BlobKey)
    -- ^ Download the snapshot from the given URL. The optional
    -- 'BlobKey' is used for reproducibility.
    --
    -- @since 0.1.0.0
  | SLFilePath !(ResolvedPath File)
    -- ^ Snapshot at a local file path.
    --
    -- @since 0.1.0.0
  deriving (Show, Eq, Data, Ord, Generic)
instance Store SnapshotLocation
instance NFData SnapshotLocation

instance ToJSON SnapshotLocation where
  toJSON (SLCompiler compiler) = object ["compiler" .= compiler]
  toJSON (SLUrl url mblob) = object
    $ "url" .= url
    : maybe [] blobKeyPairs mblob
  toJSON (SLFilePath resolved) = object ["filepath" .= resolvedRelative resolved]

-- | A flattened representation of all the layers in a snapshot.
--
-- @since 0.1.0.0
data Snapshot = Snapshot
  { snapshotCompiler :: !WantedCompiler
  -- ^ The compiler wanted for this snapshot.
  , snapshotName :: !Text
  -- ^ The 'slName' from the top 'SnapshotLayer'.
  , snapshotPackages :: !(Map PackageName SnapshotPackage)
  -- ^ Packages available in this snapshot for installation. This will be
  -- applied on top of any globally available packages.
  , snapshotDrop :: !(Set PackageName)
  -- ^ Global packages that should be dropped/ignored.
  }

-- | Settings for a package found in a snapshot.
--
-- @since 0.1.0.0
data SnapshotPackage = SnapshotPackage
  { spLocation :: !PackageLocationImmutable
  -- ^ Where to get the package from
  , spFlags :: !(Map FlagName Bool)
  -- ^ Same as 'slFlags'
  , spHidden :: !Bool
  -- ^ Same as 'slHidden'
  , spGhcOptions :: ![Text]
  -- ^ Same as 'slGhcOptions'
  }
  deriving Show

-- | A single layer of a snapshot, i.e. a specific YAML configuration file.
--
-- @since 0.1.0.0
data SnapshotLayer = SnapshotLayer
  { slParent :: !SnapshotLocation
  -- ^ The sl to extend from. This is either a specific
  -- compiler, or a @SnapshotLocation@ which gives us more information
  -- (like packages). Ultimately, we'll end up with a
  -- @CompilerVersion@.
  --
  -- @since 0.1.0.0
  , slCompiler :: !(Maybe WantedCompiler)
  -- ^ Override the compiler specified in 'slParent'. Must be
  -- 'Nothing' if using 'SLCompiler'.
  --
  -- @since 0.1.0.0
  , slName :: !Text
  -- ^ A user-friendly way of referring to this resolver.
  --
  -- @since 0.1.0.0
  , slLocations :: ![PackageLocationImmutable]
  -- ^ Where to grab all of the packages from.
  --
  -- @since 0.1.0.0
  , slDropPackages :: !(Set PackageName)
  -- ^ Packages present in the parent which should not be included
  -- here.
  --
  -- @since 0.1.0.0
  , slFlags :: !(Map PackageName (Map FlagName Bool))
  -- ^ Flag values to override from the defaults
  --
  -- @since 0.1.0.0
  , slHidden :: !(Map PackageName Bool)
  -- ^ Packages which should be hidden when registering. This will
  -- affect, for example, the import parser in the script
  -- command. We use a 'Map' instead of just a 'Set' to allow
  -- overriding the hidden settings in a parent sl.
  --
  -- @since 0.1.0.0
  , slGhcOptions :: !(Map PackageName [Text])
  -- ^ GHC options per package
  --
  -- @since 0.1.0.0
  }
  deriving (Show, Eq, Data, Generic)
instance Store SnapshotLayer
instance NFData SnapshotLayer
instance ToJSON SnapshotLayer where
  toJSON snap = object $ concat
    [ ["resolver" .= slParent snap]
    , maybe [] (\compiler -> ["compiler" .= compiler]) (slCompiler snap)
    , ["name" .= slName snap]
    , ["packages" .= slLocations snap]
    , if Set.null (slDropPackages snap) then [] else ["drop-packages" .= Set.map CabalString (slDropPackages snap)]
    , if Map.null (slFlags snap) then [] else ["flags" .= fmap toCabalStringMap (toCabalStringMap (slFlags snap))]
    , if Map.null (slHidden snap) then [] else ["hidden" .= toCabalStringMap (slHidden snap)]
    , if Map.null (slGhcOptions snap) then [] else ["ghc-options" .= toCabalStringMap (slGhcOptions snap)]
    ]

instance FromJSON (WithJSONWarnings (Unresolved SnapshotLayer)) where
  parseJSON = withObjectWarnings "Snapshot" $ \o -> do
    mcompiler <- o ..:? "compiler"
    mresolver <- jsonSubWarningsT $ o ...:? ["snapshot", "resolver"]
    unresolvedSnapshotParent <-
      case (mcompiler, mresolver) of
        (Nothing, Nothing) -> fail "Snapshot must have either resolver or compiler"
        (Just compiler, Nothing) -> pure $ pure (SLCompiler compiler, Nothing)
        (_, Just (Unresolved usl)) -> pure $ Unresolved $ \mdir -> do
          sl <- usl mdir
          case (sl, mcompiler) of
            (SLCompiler c1, Just c2) -> throwIO $ InvalidOverrideCompiler c1 c2
            _ -> pure (sl, mcompiler)

    slName <- o ..: "name"
    unresolvedLocs <- jsonSubWarningsT (o ..:? "packages" ..!= [])
    slDropPackages <- Set.map unCabalString <$> (o ..:? "drop-packages" ..!= Set.empty)
    slFlags <- (unCabalStringMap . fmap unCabalStringMap) <$> (o ..:? "flags" ..!= Map.empty)
    slHidden <- unCabalStringMap <$> (o ..:? "hidden" ..!= Map.empty)
    slGhcOptions <- unCabalStringMap <$> (o ..:? "ghc-options" ..!= Map.empty)
    pure $ (\slLocations (slParent, slCompiler) -> SnapshotLayer {..})
      <$> ((concat . map NE.toList) <$> sequenceA unresolvedLocs)
      <*> unresolvedSnapshotParent

instance Store PackageIdentifierRevision where
  size =
    VarSize $ \(PackageIdentifierRevision name version cfi) ->
    (case size of
       ConstSize x -> x
       VarSize f -> f name) +
    (case size of
       ConstSize x -> x
       VarSize f -> f version) +
    (case size of
       ConstSize x -> x
       VarSize f -> f cfi)
  peek = PackageIdentifierRevision <$> peek <*> peek <*> peek
  poke (PackageIdentifierRevision name version cfi) = poke name *> poke version *> poke cfi
