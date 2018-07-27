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
  , OptionalSubdirs (..)
  , ArchiveLocation (..)
  , RawPackageLocationOrPath (..)
  , RelFilePath (..)
  , CabalString (..)
  , parsePackageIdentifierRevision
  , PantryException (..)
  , PackageLocationOrPath (..)
  , ResolvedDir (..)
  , resolvedAbsolute
  , HpackExecutable (..)
  ) where

import RIO
import qualified RIO.Text as T
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import RIO.Char (isSpace)
import RIO.List (intersperse)
import qualified RIO.Map as Map
import Data.Aeson (ToJSON (..), FromJSON (..), withText, FromJSONKey (..))
import Data.Aeson.Types (ToJSONKey (..) ,toJSONKeyText)
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
import qualified Data.Text.Read
import Path (Path, Abs, Dir, File, parseAbsDir, toFilePath, filename)

newtype Revision = Revision Word
    deriving (Generic, Show, Eq, NFData, Data, Typeable, Ord, Hashable, Store, Display, PersistField, PersistFieldSql)

newtype Storage = Storage SqlBackend

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
data ResolvedDir = ResolvedDir
  { resolvedRelative :: !RelFilePath
  -- ^ Original value parsed from a config file.
  , resolvedAbsoluteHack :: !FilePath -- FIXME when we ditch store, use this !(Path Abs Dir)
  }
  deriving (Show, Eq, Data, Generic)
instance NFData ResolvedDir
instance Store ResolvedDir

-- FIXME get rid of this ugly hack!
resolvedAbsolute :: ResolvedDir -> Path Abs Dir
resolvedAbsolute = either impureThrow id . parseAbsDir . resolvedAbsoluteHack

-- | Either a remote package location or a local package directory.
data PackageLocationOrPath
  = PLRemote !PackageLocation
  | PLFilePath !ResolvedDir
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

instance ToJSON BlobKey where
  toJSON (BlobKey sha size') = object
    [ "sha256" .= sha
    , "size" .= size'
    ]
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
    displayC name <> displayC version <> display cfi

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
              case Data.Text.Read.decimal sizeT' of
                Right (size', "") -> Just $ Just $ FileSize size'
                _ -> Nothing
        pure $ CFIHash sha msize
      Just ("@rev", revT) ->
        case Data.Text.Read.decimal revT of
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

osNoInfo :: OptionalSubdirs
osNoInfo = OSPackageMetadata $ PackageMetadata Nothing Nothing Nothing Nothing Nothing

-- | File path relative to the configuration file it was parsed from
newtype RelFilePath = RelFilePath Text
  deriving (Show, ToJSON, FromJSON, Eq, Ord, Generic, Data, Typeable, Store, NFData)

data ArchiveLocation
  = ALUrl !Text
  | ALFilePath !RelFilePath
  -- ^ relative to the configuration file it came from
  deriving (Show, Eq, Ord, Generic, Data, Typeable)
instance Store ArchiveLocation
instance NFData ArchiveLocation
instance ToJSON ArchiveLocation where
  toJSON (ALUrl url) = object ["url" .= url]
  toJSON (ALFilePath (RelFilePath fp)) = object ["filepath" .= fp]
instance FromJSON ArchiveLocation where
  parseJSON v = asObjectUrl v <|> asObjectFilePath v <|> asText v
    where
      asObjectUrl = withObject "ArchiveLocation (URL object)" $ \o ->
        ALUrl <$> ((o .: "url") >>= validateUrl)
      asObjectFilePath = withObject "ArchiveLocation (FilePath object)" $ \o ->
        ALFilePath <$> ((o .: "url") >>= validateFilePath)

      asText = withText "ArchiveLocation (Text)" $ \t ->
        (ALUrl <$> validateUrl t) <|> (ALFilePath <$> validateFilePath t)

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
  | RPLArchive !Archive !OptionalSubdirs
  | RPLRepo !Repo !OptionalSubdirs
  deriving (Show, Eq, Data, Generic)
instance Store RawPackageLocation
instance NFData RawPackageLocation
instance ToJSON RawPackageLocation where
  toJSON (RPLHackage pir mtree) = object $ concat
    [ ["hackage" .= pir]
    , maybe [] (\tree -> ["pantry-tree" .= tree]) mtree
    ]
  toJSON (RPLArchive (Archive loc msha msize) os) = object $ concat
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
    where
      http = withText "RawPackageLocation.RPLArchive (Text)" $ \t -> do
        loc <- parseJSON $ String t
        pure $ noJSONWarnings $ RPLArchive
          Archive
            { archiveLocation = loc
            , archiveHash = Nothing
            , archiveSize = Nothing
            }
          osNoInfo

      hackageText = withText "RawPackageLocation.RPLHackage (Text)" $ \t ->
        case parsePackageIdentifierRevision t of
          Left e -> fail $ show e
          Right pir -> pure $ noJSONWarnings $ RPLHackage pir Nothing

      hackageObject = withObjectWarnings "RawPackageLocation.RPLHackage" $ \o -> RPLHackage
        <$> o ..: "hackage"
        <*> o ..:? "pantry-key"

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
        archiveLocation <- o ..: "archive" <|> o ..: "location" <|> o ..: "url"
        archiveHash <- o ..:? "sha256"
        archiveSize <- o ..:? "size"
        RPLArchive Archive {..} <$> optionalSubdirs o

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
        RPLArchive Archive {..} <$> optionalSubdirs o

-- | Newtype wrapper for easier JSON integration with Cabal types.
newtype CabalString a = CabalString { unCabalString :: a }
  deriving (Show, Eq, Ord, Typeable)

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
