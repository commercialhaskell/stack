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
  , PackageNameP (..)
  , VersionP (..)
  , PackageIdentifierRevision (..)
  , FileType (..)
  , FileSize (..)
  , TreeEntry (..)
  , SafeFilePath
  , unSafeFilePath
  , mkSafeFilePath
  , TreeKey (..)
  , Tree (..)
  , renderTree
  , parseTree
  -- , PackageTarball (..)
  , PackageLocation (..)
  , Archive (..)
  , Repo (..)
  , RepoType (..)
  , parsePackageIdentifier
  , parsePackageName
  , parseFlagName
  , parseVersion
  , displayC
  , RawPackageLocation (..)
  , mkRawPackageLocation
  , unRawPackageLocation
  , OptionalSubdirs (..)
  , ArchiveLocation (..)
  , RawPackageLocationOrPath (..)
  , RelFilePath (..)
  , CabalString (..)
  , toCabalStringMap
  , unCabalStringMap
  , parsePackageIdentifierRevision
  , PantryException (..)
  , PackageLocationOrPath (..)
  , ResolvedPath (..)
  , resolvedAbsolute
  , HpackExecutable (..)
  , WantedCompiler (..)
  , UnresolvedSnapshotLocation
  , resolveSnapshotLocation
  , unresolveSnapshotLocation
  , ltsSnapshotLocation
  , nightlySnapshotLocation
  , SnapshotLocation (..)
  , parseSnapshotLocation
  , parseSnapshot
  , Snapshot (..)
  , parseWantedCompiler
  , PackageMetadata (..)
  ) where

import RIO
import qualified RIO.Text as T
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import RIO.Char (isSpace)
import RIO.List (intersperse)
import RIO.Time (toGregorian, Day)
import qualified RIO.Map as Map
import qualified Data.Map.Strict as Map (mapKeysMonotonic)
import qualified RIO.Set as Set
import Data.Aeson (ToJSON (..), FromJSON (..), withText, FromJSONKey (..))
import Data.Aeson.Types (ToJSONKey (..) ,toJSONKeyText, Parser)
import Data.Aeson.Extended
import Data.ByteString.Builder (toLazyByteString, byteString, wordDec)
import Database.Persist
import Database.Persist.Sql
import Pantry.StaticSHA256
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Parsec.Common (PError (..), PWarning (..), showPos)
import Distribution.Types.PackageName (PackageName)
import Distribution.PackageDescription (FlagName)
import Distribution.Types.PackageId (PackageIdentifier (..))
import qualified Distribution.Text
import Distribution.Types.Version (Version)
import Data.Store (Size (..), Store (..)) -- FIXME remove
import Network.HTTP.Client (parseRequest)
import Data.Text.Read (decimal)
import Path (Abs, Dir, File, parseAbsDir, toFilePath, filename)
import Path.Internal (Path (..)) -- FIXME don't import this
import Path.IO (resolveFile)
import Data.Pool (Pool)

newtype Revision = Revision Word
    deriving (Generic, Show, Eq, NFData, Data, Typeable, Ord, Hashable, Store, Display, PersistField, PersistFieldSql)

newtype Storage = Storage (Pool SqlBackend)

data PantryConfig = PantryConfig
  { pcHackageSecurity :: !HackageSecurityConfig
  , pcHpackExecutable :: !HpackExecutable
  , pcRootDir :: !(Path Abs Dir)
  , pcStorage :: !Storage
  , pcUpdateRef :: !(MVar Bool)
  -- ^ Want to try updating the index once during a single run for missing
  -- package identifiers. We also want to ensure we only update once at a
  -- time. Start at @True@.
  {- FIXME add this shortly
  , pcParsedCabalFiles ::
      !(IORef
          ( Map PackageLocation GenericPackageDescription
          , Map FilePath        GenericPackageDescription
          )
       )
  -- ^ Cache of previously parsed cabal files, to save on slow parsing time.
  -}
  }

-- | A directory which was loaded up relative and has been resolved
-- against the config file it came from.
data ResolvedPath t = ResolvedPath
  { resolvedRelative :: !RelFilePath
  -- ^ Original value parsed from a config file.
  , resolvedAbsoluteHack :: !FilePath -- FIXME when we ditch store, use this !(Path Abs Dir)
  }
  deriving (Show, Eq, Data, Generic, Ord)
instance NFData (ResolvedPath t)
instance Store (ResolvedPath t)

-- FIXME get rid of this ugly hack!
resolvedAbsolute :: ResolvedPath t -> Path Abs t
resolvedAbsolute = Path . resolvedAbsoluteHack

-- | Either a remote package location or a local package directory.
data PackageLocationOrPath
  = PLRemote !PackageLocation
  | PLFilePath !(ResolvedPath Dir)
  deriving (Show, Eq, Data, Generic)
instance NFData PackageLocationOrPath
instance Store PackageLocationOrPath

instance Display PackageLocationOrPath where
  display (PLRemote loc) = display loc

-- | Location for remote packages (i.e., not local file paths).
data PackageLocation
  = PLHackage !PackageIdentifierRevision !(Maybe TreeKey)
  | PLArchive !Archive !PackageMetadata
  | PLRepo    !Repo !PackageMetadata
  deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance NFData PackageLocation
instance Store PackageLocation

instance Display PackageLocation where
  display (PLHackage pir _tree) = display pir <> " (from Hackage)"

-- | A package archive, could be from a URL or a local file
-- path. Local file path archives are assumed to be unchanging
-- over time, and so are allowed in custom snapshots.
data Archive = Archive
  { archiveLocation :: !ArchiveLocation
  , archiveHash :: !(Maybe StaticSHA256)
  , archiveSize :: !(Maybe FileSize)
  }
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Store Archive
instance NFData Archive

-- | A package archive, could be from a URL or a local file
-- path. Local file path archives are assumed to be unchanging
-- over time, and so are allowed in custom snapshots.
data RawArchive = RawArchive
  { raLocation :: !RawArchiveLocation
  , raHash :: !(Maybe StaticSHA256)
  , raSize :: !(Maybe FileSize)
  }
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Store RawArchive
instance NFData RawArchive

-- | The type of a source control repository.
data RepoType = RepoGit | RepoHg
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Store RepoType
instance NFData RepoType

-- | Information on packages stored in a source control repository.
data Repo = Repo
    { repoUrl :: !Text
    , repoCommit :: !Text
    , repoType :: !RepoType
    }
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Store Repo
instance NFData Repo

-- An unexported newtype wrapper to hang a 'FromJSON' instance off of. Contains
-- a GitHub user and repo name separated by a forward slash, e.g. "foo/bar".
newtype GitHubRepo = GitHubRepo Text

instance FromJSON GitHubRepo where
    parseJSON = withText "GitHubRepo" $ \s -> do
        case T.split (== '/') s of
            [x, y] | not (T.null x || T.null y) -> return (GitHubRepo s)
            _ -> fail "expecting \"user/repo\""

data HackageSecurityConfig = HackageSecurityConfig
  { hscKeyIds :: ![Text]
  , hscKeyThreshold :: !Int
  , hscDownloadPrefix :: !Text
  }

class HasPantryConfig env where
  pantryConfigL :: Lens' env PantryConfig

-- | File size in bytes
newtype FileSize = FileSize Word
  deriving (Show, Eq, Ord, Data, Typeable, Generic, Display, Hashable, NFData, Store, PersistField, PersistFieldSql, ToJSON, FromJSON)

data BlobKey = BlobKey !StaticSHA256 !FileSize
  deriving (Eq, Ord, Data, Typeable, Generic)
instance Store BlobKey
instance NFData BlobKey

instance Show BlobKey where
  show = T.unpack . utf8BuilderToText . display
instance Display BlobKey where
  display (BlobKey sha size) = display sha <> "," <> display size

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

newtype PackageNameP = PackageNameP PackageName
instance PersistField PackageNameP where
  toPersistValue (PackageNameP pn) = PersistText $ displayC pn
  fromPersistValue v = do
    str <- fromPersistValue v
    case parsePackageName str of
      Nothing -> Left $ "Invalid package name: " <> T.pack str
      Just pn -> Right $ PackageNameP pn
instance PersistFieldSql PackageNameP where
  sqlType _ = SqlString

newtype VersionP = VersionP Version
instance PersistField VersionP where
  toPersistValue (VersionP v) = PersistText $ displayC v
  fromPersistValue v = do
    str <- fromPersistValue v
    case parseVersion str of
      Nothing -> Left $ "Invalid version number: " <> T.pack str
      Just ver -> Right $ VersionP ver
instance PersistFieldSql VersionP where
  sqlType _ = SqlString

-- | Information on the contents of a cabal file
data CabalFileInfo
  = CFILatest
  -- ^ Take the latest revision of the cabal file available. This
  -- isn't reproducible at all, but the running assumption (not
  -- necessarily true) is that cabal file revisions do not change
  -- semantics of the build.
  | CFIHash !StaticSHA256 !(Maybe FileSize)
  -- ^ Identify by contents of the cabal file itself. Only reason for
  -- @Maybe@ on @FileSize@ is for compatibility with input that
  -- doesn't include the file size.
  | CFIRevision !Revision
  -- ^ Identify by revision number, with 0 being the original and
  -- counting upward. This relies on Hackage providing consistent
  -- versioning. @CFIHash@ should be preferred wherever possible for
  -- reproducibility.
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Store CabalFileInfo
instance NFData CabalFileInfo
instance Hashable CabalFileInfo

instance Display CabalFileInfo where
  display CFILatest = mempty
  display (CFIHash hash' msize) =
    "@sha256:" <> display hash' <> maybe mempty (\i -> "," <> display i) msize
  display (CFIRevision rev) = "@rev:" <> display rev

data PackageIdentifierRevision = PackageIdentifierRevision !PackageName !Version !CabalFileInfo
  deriving (Generic, Eq, Ord, Data, Typeable)
instance NFData PackageIdentifierRevision

instance Show PackageIdentifierRevision where
  show = T.unpack . utf8BuilderToText . display

instance Display PackageIdentifierRevision where
  display (PackageIdentifierRevision name version cfi) =
    displayC name <> "-" <> displayC version <> display cfi

instance ToJSON PackageIdentifierRevision where
  toJSON = toJSON . utf8BuilderToText . display
instance FromJSON PackageIdentifierRevision where
  parseJSON = withText "PackageIdentifierRevision" $ \t ->
    case parsePackageIdentifierRevision t of
      Left e -> fail $ show e
      Right pir -> pure pir

-- | Parse a 'PackageIdentifierRevision'
parsePackageIdentifierRevision :: MonadThrow m => Text -> m PackageIdentifierRevision
parsePackageIdentifierRevision t = maybe (throwM $ PackageIdentifierRevisionParseFail t) pure $ do
  let (identT, cfiT) = T.break (== '@') t
  PackageIdentifier name version <- parsePackageIdentifier $ T.unpack identT
  cfi <-
    case splitColon cfiT of
      Just ("@sha256", shaSizeT) -> do
        let (shaT, sizeT) = T.break (== ',') shaSizeT
        sha <- either (const Nothing) Just $ mkStaticSHA256FromText shaT
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

data PantryException
  = PackageIdentifierRevisionParseFail !Text
  | InvalidCabalFile
      !(Either PackageLocation (Path Abs File))
      !(Maybe Version)
      ![PError]
      ![PWarning]
  | TreeWithoutCabalFile !PackageLocation
  | TreeWithMultipleCabalFiles !PackageLocation ![SafeFilePath]
  | MismatchedCabalName !(Path Abs File) !PackageName
  | NoCabalFileFound !(Path Abs Dir)
  | MultipleCabalFilesFound !(Path Abs Dir) ![Path Abs File]
  | InvalidWantedCompiler !Text
  | InvalidSnapshotLocation !(Path Abs Dir) !Text
  | InvalidOverrideCompiler !WantedCompiler !WantedCompiler
  | InvalidFilePathSnapshot !Text
  | InvalidSnapshot !SnapshotLocation !SomeException
  | TreeKeyMismatch
      !PackageLocation
      !TreeKey -- expected
      !TreeKey -- actual
  | MismatchedPackageMetadata
      !PackageLocation
      !PackageMetadata
      !BlobKey -- cabal file found
      !PackageIdentifier

  deriving Typeable
instance Exception PantryException where
instance Show PantryException where
  show = T.unpack . utf8BuilderToText . display
instance Display PantryException where
  display (PackageIdentifierRevisionParseFail text) =
    "Invalid package identifier (with optional revision): " <>
    display text
  display (InvalidCabalFile loc _mversion errs warnings) =
    "Unable to parse cabal file from package " <>
    either display (fromString . toFilePath) loc <>

    {-

     Not actually needed, the errors will indicate if a newer version exists.
     Also, it seems that this is set to Just the version even if we support it.

    , case mversion of
        Nothing -> ""
        Just version -> "\nRequires newer Cabal file parser version: " ++
                        versionString version
    -}

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
      warnings
  display (MismatchedCabalName fp name) =
    "cabal file path " <>
    fromString (toFilePath fp) <>
    " does not match the package name it defines.\n" <>
    "Please rename the file to: " <>
    displayC name <>
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
  display (TreeKeyMismatch loc expected actual) =
    "Tree key mismatch when getting " <> display loc <>
    "\nExpected: " <> display expected <>
    "\nActual:   " <> display actual
  display (MismatchedPackageMetadata loc pm foundCabal foundIdent) =
    "Mismatched package metadata for " <> display loc <>
    "\nFound: " <> displayC foundIdent <> " with cabal file " <>
    display foundCabal <> "\nExpected: " <> display pm

data FileType = FTNormal | FTExecutable
  deriving Show
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

data TreeEntry = TreeEntry !BlobKey !FileType
  deriving Show

newtype SafeFilePath = SafeFilePath Text
  deriving (Show, Eq, Ord)

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

newtype TreeKey = TreeKey BlobKey
  deriving (Show, Eq, Ord, Generic, Data, Typeable, ToJSON, FromJSON, NFData, Store, Display)

newtype Tree
  = TreeMap (Map SafeFilePath TreeEntry)
  -- FIXME in the future, consider allowing more lax parsing
  -- See: https://www.fpcomplete.com/blog/2018/07/pantry-part-2-trees-keys
  -- | TreeTarball !PackageTarball
  deriving Show

renderTree :: Tree -> ByteString
renderTree = BL.toStrict . toLazyByteString . go
  where
    go :: Tree -> Builder
    go (TreeMap m) = "map:" <> Map.foldMapWithKey goEntry m

    goEntry sfp (TreeEntry (BlobKey sha (FileSize size')) ft) =
      netstring (unSafeFilePath sfp) <>
      netstring (staticSHA256ToText sha) <>
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
parseTree' = undefined

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

parsePackageName :: String -> Maybe PackageName
parsePackageName = Distribution.Text.simpleParse

parseVersion :: String -> Maybe Version
parseVersion = Distribution.Text.simpleParse

parseFlagName :: String -> Maybe FlagName
parseFlagName = Distribution.Text.simpleParse

-- | Display Cabal types using 'Distribution.Text.Text'.
displayC :: (IsString str, Distribution.Text.Text a) => a -> str
displayC = fromString . Distribution.Text.display

data OptionalSubdirs
  = OSSubdirs ![Text]
  | OSPackageMetadata !PackageMetadata
  deriving (Show, Eq, Data, Generic)
instance NFData OptionalSubdirs
instance Store OptionalSubdirs

data PackageMetadata = PackageMetadata
  { pmName :: !(Maybe PackageName)
  , pmVersion :: !(Maybe Version)
  , pmTree :: !(Maybe TreeKey)
  , pmCabal :: !(Maybe BlobKey)
  , pmSubdir :: !(Maybe Text) -- subdir
  }
  deriving (Show, Eq, Ord, Generic, Data, Typeable)
instance Store PackageMetadata
instance NFData PackageMetadata

instance Display PackageMetadata where
  display pm = fold $ intersperse ", " $ catMaybes
    [ (\name -> "name == " <> displayC name) <$> pmName pm
    , (\version -> "version == " <> displayC version) <$> pmVersion pm
    , (\tree -> "tree == " <> display tree) <$> pmTree pm
    , (\cabal -> "cabal file == " <> display cabal) <$> pmCabal pm
    , (\subdir -> "subdir == " <> display subdir) <$> pmSubdir pm
    ]

osNoInfo :: OptionalSubdirs
osNoInfo = OSPackageMetadata $ PackageMetadata Nothing Nothing Nothing Nothing Nothing

-- | File path relative to the configuration file it was parsed from
newtype RelFilePath = RelFilePath Text
  deriving (Show, ToJSON, FromJSON, Eq, Ord, Generic, Data, Typeable, Store, NFData)

data ArchiveLocation
  = ALUrl !Text
  | ALFilePath !(ResolvedPath File)
  deriving (Show, Eq, Ord, Generic, Data, Typeable)
instance Store ArchiveLocation
instance NFData ArchiveLocation

instance Display ArchiveLocation where
  display (ALUrl url) = display url
  display (ALFilePath resolved) = fromString $ toFilePath $ resolvedAbsolute resolved

data RawArchiveLocation
  = RALUrl !Text
  | RALFilePath !RelFilePath
  -- ^ relative to the configuration file it came from
  deriving (Show, Eq, Ord, Generic, Data, Typeable)
instance Store RawArchiveLocation
instance NFData RawArchiveLocation
instance ToJSON RawArchiveLocation where
  toJSON (RALUrl url) = object ["url" .= url]
  toJSON (RALFilePath (RelFilePath fp)) = object ["filepath" .= fp]
instance FromJSON RawArchiveLocation where
  parseJSON v = asObjectUrl v <|> asObjectFilePath v <|> asText v
    where
      asObjectUrl = withObject "ArchiveLocation (URL object)" $ \o ->
        RALUrl <$> ((o .: "url") >>= validateUrl)
      asObjectFilePath = withObject "ArchiveLocation (FilePath object)" $ \o ->
        RALFilePath <$> ((o .: "url") >>= validateFilePath)

      asText = withText "ArchiveLocation (Text)" $ \t ->
        (RALUrl <$> validateUrl t) <|> (RALFilePath <$> validateFilePath t)

      validateUrl t =
        case parseRequest $ T.unpack t of
          Left _ -> fail $ "Could not parse URL: " ++ T.unpack t
          Right _ -> pure t

      validateFilePath t =
        if any (\ext -> ext `T.isSuffixOf` t) (T.words ".zip .tar .tar.gz")
          then pure (RelFilePath t)
          else fail $ "Does not have an archive file extension: " ++ T.unpack t

-- | A raw package location /or/ a file path to a directory containing a package.
data RawPackageLocationOrPath
  = RPLRemote !RawPackageLocation
  | RPLFilePath !RelFilePath
  deriving Show
instance ToJSON RawPackageLocationOrPath where
  toJSON (RPLRemote rpl) = toJSON rpl
  toJSON (RPLFilePath (RelFilePath fp)) = toJSON fp
instance FromJSON (WithJSONWarnings RawPackageLocationOrPath) where
  parseJSON v =
    (fmap RPLRemote <$> parseJSON v) <|>
    ((noJSONWarnings . RPLFilePath . RelFilePath) <$> parseJSON v)

-- | The raw representation of packages allowed in a snapshot
-- specification. Does /not/ allow local filepaths.
data RawPackageLocation
  = RPLHackage !PackageIdentifierRevision !(Maybe TreeKey)
  | RPLArchive !RawArchive !OptionalSubdirs
  | RPLRepo !Repo !OptionalSubdirs
  deriving (Show, Eq, Data, Generic)
instance Store RawPackageLocation
instance NFData RawPackageLocation
instance ToJSON RawPackageLocation where
  toJSON (RPLHackage pir mtree) = object $ concat
    [ ["hackage" .= pir]
    , maybe [] (\tree -> ["pantry-tree" .= tree]) mtree
    ]
  toJSON (RPLArchive (RawArchive loc msha msize) os) = object $ concat
    [ ["location" .= loc]
    , maybe [] (\sha -> ["sha256" .= sha]) msha
    , maybe [] (\size' -> ["size " .= size']) msize
    , osToPairs os
    ]
  toJSON (RPLRepo (Repo url commit typ) os) = object $ concat
    [ [ urlKey .= url
      , "commit" .= commit
      ]
    , osToPairs os
    ]
    where
      urlKey =
        case typ of
          RepoGit -> "git"
          RepoHg  -> "hg"

osToPairs :: OptionalSubdirs -> [(Text, Value)]
osToPairs (OSSubdirs subdirs) = [("subdirs" .= subdirs)]
osToPairs (OSPackageMetadata (PackageMetadata mname mversion mtree mcabal msubdir)) = concat
  [ maybe [] (\name -> ["name" .= CabalString name]) mname
  , maybe [] (\version -> ["version" .= CabalString version]) mversion
  , maybe [] (\tree -> ["pantry-tree" .= tree]) mtree
  , maybe [] (\cabal -> ["cabal-file" .= cabal]) mcabal
  , maybe [] (\subdir -> ["subdir" .= subdir]) msubdir
  ]

instance FromJSON (WithJSONWarnings RawPackageLocation) where
  parseJSON v
      = http v
    <|> hackageText v
    <|> hackageObject v
    <|> repo v
    <|> archiveObject v
    <|> github v
    <|> fail ("Could not parse a RawPackageLocation from: " ++ show v)
    where
      http = withText "RawPackageLocation.RPLArchive (Text)" $ \t -> do
        loc <- parseJSON $ String t
        pure $ noJSONWarnings $ RPLArchive
          RawArchive
            { raLocation = loc
            , raHash = Nothing
            , raSize = Nothing
            }
          osNoInfo

      hackageText = withText "RawPackageLocation.RPLHackage (Text)" $ \t ->
        case parsePackageIdentifierRevision t of
          Left e -> fail $ show e
          Right pir -> pure $ noJSONWarnings $ RPLHackage pir Nothing

      hackageObject = withObjectWarnings "RawPackageLocation.RPLHackage" $ \o -> RPLHackage
        <$> o ..: "hackage"
        <*> o ..:? "pantry-tree"

      optionalSubdirs o =
        (OSSubdirs <$> o ..: "subdirs") <|>
        (OSPackageMetadata <$> (PackageMetadata
            <$> (fmap unCabalString <$> (o ..:? "name"))
            <*> (fmap unCabalString <$> (o ..:? "version"))
            <*> o ..:? "pantry-tree"
            <*> o ..:? "cabal-file"
            <*> o ..:? "subdir"))

      repo = withObjectWarnings "RawPackageLocation.RPLRepo" $ \o -> do
        (repoType, repoUrl) <-
          ((RepoGit, ) <$> o ..: "git") <|>
          ((RepoHg, ) <$> o ..: "hg")
        repoCommit <- o ..: "commit"
        RPLRepo Repo {..} <$> optionalSubdirs o

      archiveObject = withObjectWarnings "RawPackageLocation.RPLArchive" $ \o -> do
        raLocation <- o ..: "archive" <|> o ..: "location" <|> o ..: "url"
        raHash <- o ..:? "sha256"
        raSize <- o ..:? "size"
        RPLArchive RawArchive {..} <$> optionalSubdirs o

      github = withObjectWarnings "PLArchive:github" $ \o -> do
        GitHubRepo ghRepo <- o ..: "github"
        commit <- o ..: "commit"
        let raLocation = RALUrl $ T.concat
              [ "https://github.com/"
              , ghRepo
              , "/archive/"
              , commit
              , ".tar.gz"
              ]
        raHash <- o ..:? "sha256"
        raSize <- o ..:? "size"
        RPLArchive RawArchive {..} <$> optionalSubdirs o

-- | Convert a 'RawPackageLocation' into a list of 'PackageLocation's.
unRawPackageLocation
  :: MonadIO m
  => Maybe (Path Abs Dir) -- ^ directory to resolve relative paths from, if local
  -> RawPackageLocation
  -> m [PackageLocation]
unRawPackageLocation _dir (RPLHackage pir mtree) = pure [PLHackage pir mtree]

-- | Convert a 'PackageLocation' into a 'RawPackageLocation'.
mkRawPackageLocation :: PackageLocation -> RawPackageLocation
mkRawPackageLocation (PLHackage pir mtree) = RPLHackage pir mtree
mkRawPackageLocation (PLArchive archive pm) =
  RPLArchive
    RawArchive
      { raLocation =
          case archiveLocation archive of
            ALUrl url -> RALUrl url
            ALFilePath resolved -> RALFilePath $ resolvedRelative resolved
      , raHash = archiveHash archive
      , raSize = archiveSize archive
      }
    (OSPackageMetadata pm)
mkRawPackageLocation (PLRepo repo pm) = RPLRepo repo (OSPackageMetadata pm)

-- | Newtype wrapper for easier JSON integration with Cabal types.
newtype CabalString a = CabalString { unCabalString :: a }
  deriving (Show, Eq, Ord, Typeable)

toCabalStringMap :: Map a v -> Map (CabalString a) v
toCabalStringMap = Map.mapKeysMonotonic CabalString -- FIXME why doesn't coerce work?

unCabalStringMap :: Map (CabalString a) v -> Map a v
unCabalStringMap = Map.mapKeysMonotonic unCabalString -- FIXME why doesn't coerce work?

instance Distribution.Text.Text a => ToJSON (CabalString a) where
  toJSON = toJSON . Distribution.Text.display . unCabalString
instance Distribution.Text.Text a => ToJSONKey (CabalString a) where
  toJSONKey = toJSONKeyText $ displayC . unCabalString

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
instance IsCabalString PackageIdentifier where
  cabalStringName _ = "package identifier"
  cabalStringParser = parsePackageIdentifier
instance IsCabalString FlagName where
  cabalStringName _ = "flag name"
  cabalStringParser = parseFlagName

data HpackExecutable
    = HpackBundled
    | HpackCommand String
    deriving (Show, Read, Eq, Ord)

data WantedCompiler
  = WCGhc !Version
  | WCGhcjs
      !Version -- GHCJS version
      !Version -- GHC version
 deriving (Show, Eq, Ord, Data, Generic)
instance NFData WantedCompiler
instance Store WantedCompiler
instance Display WantedCompiler where
  display (WCGhc vghc) = "ghc-" <> displayC vghc
  display (WCGhcjs vghcjs vghc) = "ghcjs-" <> displayC vghcjs <> "_ghc-" <> displayC vghc
instance ToJSON WantedCompiler where
  toJSON = toJSON . utf8BuilderToText . display
instance FromJSON WantedCompiler where
  parseJSON = withText "WantedCompiler" $ either (fail . show) pure . parseWantedCompiler

parseWantedCompiler :: Text -> Either PantryException WantedCompiler
parseWantedCompiler t0 = maybe (Left $ InvalidWantedCompiler t0) Right $
  case T.stripPrefix "ghcjs-" t0 of
    Just t1 -> parseGhcjs t1
    Nothing -> T.stripPrefix "ghc-" t0 >>= parseGhc
  where
    parseGhcjs = undefined
    parseGhc = fmap WCGhc . parseVersion . T.unpack

data UnresolvedSnapshotLocation
  = USLCompiler !WantedCompiler
  | USLUrl !Text !(Maybe BlobKey)
  | USLFilePath !RelFilePath
  deriving (Show, Eq, Ord, Data, Typeable, Generic)
instance ToJSON UnresolvedSnapshotLocation where
  toJSON (USLCompiler c) = object ["compiler" .= c]
  toJSON (USLUrl t mblob) = object $ concat
    [ ["url" .= t]
    , maybe [] (\blob -> ["blob" .= blob]) mblob
    ]
  toJSON (USLFilePath fp) = object ["filepath" .= fp]
instance FromJSON (WithJSONWarnings UnresolvedSnapshotLocation) where
  parseJSON v = text v <|> obj v
    where
      text = withText "UnresolvedSnapshotLocation (Text)" $ pure . noJSONWarnings . parseSnapshotLocation

      obj = withObjectWarnings "UnresolvedSnapshotLocation (Object)" $ \o ->
        (USLCompiler <$> o ..: "compiler") <|>
        (USLUrl <$> o ..: "url" <*> o ..:? "blob") <|>
        (USLFilePath <$> o ..: "filepath")

resolveSnapshotLocation
  :: UnresolvedSnapshotLocation
  -> Maybe (Path Abs Dir)
  -> Maybe WantedCompiler
  -> IO SnapshotLocation
resolveSnapshotLocation (USLCompiler compiler) _ Nothing = pure $ SLCompiler compiler
resolveSnapshotLocation (USLCompiler compiler1) _ (Just compiler2) = throwIO $ InvalidOverrideCompiler compiler1 compiler2
resolveSnapshotLocation (USLUrl url mblob) _ mcompiler = pure $ SLUrl url mblob mcompiler
resolveSnapshotLocation (USLFilePath (RelFilePath t)) Nothing _mcompiler = throwIO $ InvalidFilePathSnapshot t
resolveSnapshotLocation (USLFilePath rfp@(RelFilePath t)) (Just dir) mcompiler = do
  abs' <- resolveFile dir (T.unpack t) `catchAny` \_ -> throwIO (InvalidSnapshotLocation dir t)
  pure $ SLFilePath
            ResolvedPath
              { resolvedRelative = rfp
              , resolvedAbsoluteHack = toFilePath abs'
              }
            mcompiler

unresolveSnapshotLocation
  :: SnapshotLocation
  -> (UnresolvedSnapshotLocation, Maybe WantedCompiler)
unresolveSnapshotLocation (SLCompiler compiler) = (USLCompiler compiler, Nothing)
unresolveSnapshotLocation (SLUrl url mblob mcompiler) = (USLUrl url mblob, mcompiler)
unresolveSnapshotLocation (SLFilePath fp mcompiler) = (USLFilePath $ resolvedRelative fp, mcompiler)

instance Display UnresolvedSnapshotLocation where
  display (USLCompiler compiler) = display compiler
  display (USLUrl url Nothing) = display url
  display (USLUrl url (Just blob)) = display url <> " (" <> display blob <> ")"
  display (USLFilePath (RelFilePath t)) = display t

instance Display SnapshotLocation where
  display sl =
    let (usl, mcompiler) = unresolveSnapshotLocation sl
     in display usl <>
        (case mcompiler of
           Nothing -> mempty
           Just compiler -> ", override compiler: " <> display compiler)

parseSnapshotLocation :: Text -> UnresolvedSnapshotLocation
parseSnapshotLocation t0 = fromMaybe parsePath $
  (either (const Nothing) (Just . USLCompiler) (parseWantedCompiler t0)) <|>
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
      Just $ fst $ ltsSnapshotLocation x y
    parseNightly = do
      t1 <- T.stripPrefix "nightly-" t0
      date <- readMaybe (T.unpack t1)
      Just $ fst $ nightlySnapshotLocation date

    parseGithub = do
      t1 <- T.stripPrefix "github:" t0
      let (user, t2) = T.break (== '/') t1
      t3 <- T.stripPrefix "/" t2
      let (repo, t4) = T.break (== ':') t3
      path <- T.stripPrefix ":" t4
      Just $ fst $ githubSnapshotLocation user repo path

    parseUrl = parseRequest (T.unpack t0) $> USLUrl t0 Nothing

    parsePath = USLFilePath $ RelFilePath t0

githubSnapshotLocation :: Text -> Text -> Text -> (UnresolvedSnapshotLocation, SnapshotLocation)
githubSnapshotLocation user repo path =
  let url = T.concat
        [ "https://raw.githubusercontent.com/"
        , user
        , "/"
        , repo
        , "/master/"
        , path
        ]
   in (USLUrl url Nothing, SLUrl url Nothing Nothing)

defUser :: Text
defUser = "commercialhaskell"

defRepo :: Text
defRepo = "stack-templates"

ltsSnapshotLocation :: Int -> Int -> (UnresolvedSnapshotLocation, SnapshotLocation)
ltsSnapshotLocation x y =
  githubSnapshotLocation defUser defRepo $
  utf8BuilderToText $
  "lts/" <> display x <> "/" <> display y <> ".yaml"

nightlySnapshotLocation :: Day -> (UnresolvedSnapshotLocation, SnapshotLocation)
nightlySnapshotLocation date =
  githubSnapshotLocation defUser defRepo $
  utf8BuilderToText $
  "nightly/" <> display year <> "/" <> display month <> "/" <> display day <> ".yaml"
  where
    (year, month, day) = toGregorian date

data SnapshotLocation
  = SLCompiler !WantedCompiler
  | SLUrl !Text !(Maybe BlobKey) !(Maybe WantedCompiler)
  | SLFilePath !(ResolvedPath File) !(Maybe WantedCompiler)
  deriving (Show, Eq, Data, Ord, Generic)
instance Store SnapshotLocation
instance NFData SnapshotLocation

data Snapshot = Snapshot
  { snapshotParent :: !SnapshotLocation
  -- ^ The snapshot to extend from. This is either a specific
  -- compiler, or a @SnapshotLocation@ which gives us more information
  -- (like packages). Ultimately, we'll end up with a
  -- @CompilerVersion@.
  , snapshotName :: !Text
  -- ^ A user-friendly way of referring to this resolver.
  , snapshotLocations :: ![PackageLocation]
  -- ^ Where to grab all of the packages from.
  , snapshotDropPackages :: !(Set PackageName)
  -- ^ Packages present in the parent which should not be included
  -- here.
  , snapshotFlags :: !(Map PackageName (Map FlagName Bool))
  -- ^ Flag values to override from the defaults
  , snapshotHidden :: !(Map PackageName Bool)
  -- ^ Packages which should be hidden when registering. This will
  -- affect, for example, the import parser in the script
  -- command. We use a 'Map' instead of just a 'Set' to allow
  -- overriding the hidden settings in a parent snapshot.
  , snapshotGhcOptions :: !(Map PackageName [Text])
  -- ^ GHC options per package
  , snapshotGlobalHints :: !(Map PackageName (Maybe Version))
  -- ^ Hints about which packages are available globally. When
  -- actually building code, we trust the package database provided
  -- by GHC itself, since it may be different based on platform or
  -- GHC install. However, when we want to check the compatibility
  -- of a snapshot with some codebase without installing GHC (e.g.,
  -- during stack init), we would use this field.
  }
  deriving (Show, Eq, Data, Generic)
instance Store Snapshot
instance NFData Snapshot
instance ToJSON Snapshot where
  toJSON snap = object $ concat
    [ case snapshotParent snap of
        SLCompiler compiler -> ["compiler" .= compiler]
        SLUrl url mblob mcompiler -> concat
          [ pure $ "resolver" .= concat
              [ ["url" .= url]
              , maybe [] blobKeyPairs mblob
              ]
          , case mcompiler of
              Nothing -> []
              Just compiler -> ["compiler" .= compiler]
          ]
    , ["name" .= snapshotName snap]
    , ["packages" .= map mkRawPackageLocation (snapshotLocations snap)]
    , if Set.null (snapshotDropPackages snap) then [] else ["drop-packages" .= Set.map CabalString (snapshotDropPackages snap)]
    , if Map.null (snapshotFlags snap) then [] else ["flags" .= fmap toCabalStringMap (toCabalStringMap (snapshotFlags snap))]
    , if Map.null (snapshotHidden snap) then [] else ["hidden" .= toCabalStringMap (snapshotHidden snap)]
    , if Map.null (snapshotGhcOptions snap) then [] else ["ghc-options" .= toCabalStringMap (snapshotGhcOptions snap)]
    , if Map.null (snapshotGlobalHints snap) then [] else ["global-hints" .= fmap (fmap CabalString) (toCabalStringMap (snapshotGlobalHints snap))]
    ]

parseSnapshot :: Maybe (Path Abs Dir) -> Value -> Parser (WithJSONWarnings (IO Snapshot))
parseSnapshot mdir = withObjectWarnings "Snapshot" $ \o -> do
  mcompiler <- o ..:? "compiler"
  mresolver <- jsonSubWarningsT $ o ..:? "resolver"
  iosnapshotParent <-
    case (mcompiler, mresolver) of
      (Nothing, Nothing) -> fail "Snapshot must have either resolver or compiler"
      (Just compiler, Nothing) -> pure $ pure $ SLCompiler compiler
      (_, Just usl) -> pure $ resolveSnapshotLocation usl mdir mcompiler

  snapshotName <- o ..: "name"
  rawLocs <- jsonSubWarningsT (o ..:? "packages" ..!= [])
  snapshotDropPackages <- Set.map unCabalString <$> (o ..:? "drop-packages" ..!= Set.empty)
  snapshotFlags <- (unCabalStringMap . fmap unCabalStringMap) <$> (o ..:? "flags" ..!= Map.empty)
  snapshotHidden <- unCabalStringMap <$> (o ..:? "hidden" ..!= Map.empty)
  snapshotGhcOptions <- unCabalStringMap <$> (o ..:? "ghc-options" ..!= Map.empty)
  snapshotGlobalHints <- unCabalStringMap . (fmap.fmap) unCabalString <$> (o ..:? "global-hints" ..!= Map.empty)
  pure $ do
    snapshotLocations <- fmap concat $ mapM (unRawPackageLocation mdir) rawLocs
    snapshotParent <- iosnapshotParent
    pure Snapshot {..}

-- FIXME ORPHANS remove

instance Store PackageIdentifier where
  size =
    VarSize $ \(PackageIdentifier name version) ->
    (case size of
       ConstSize x -> x
       VarSize f -> f name) +
    (case size of
       ConstSize x -> x
       VarSize f -> f version)
  peek = PackageIdentifier <$> peek <*> peek
  poke (PackageIdentifier name version) = poke name *> poke version
instance Store PackageName where
  size =
    VarSize $ \name ->
    case size of
      ConstSize x -> x
      VarSize f -> f (displayC name :: String)
  peek = peek >>= maybe (fail "Invalid package name") pure . parsePackageName
  poke name = poke (displayC name :: String)
instance Store Version where
  size =
    VarSize $ \version ->
    case size of
      ConstSize x -> x
      VarSize f -> f (displayC version :: String)
  peek = peek >>= maybe (fail "Invalid version") pure . parseVersion
  poke version = poke (displayC version :: String)
instance Store FlagName where
  size =
    VarSize $ \fname ->
    case size of
      ConstSize x -> x
      VarSize f -> f (displayC fname :: String)
  peek = peek >>= maybe (fail "Invalid flag name") pure . parseFlagName
  poke fname = poke (displayC fname :: String)
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

deriving instance Data Dir
deriving instance Data File
