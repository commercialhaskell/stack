{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
  , CabalHash (..)
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
  , PackageTarball (..)
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
  , RawArchive (..)
  , RawRepo (..)
  , OptionalSubdirs (..)
  , CabalString (..)
  , parsePackageIdentifierRevision
  , PantryException (..)
  ) where

import RIO
import qualified RIO.Text as T
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import RIO.Char (isSpace)
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

newtype Revision = Revision Word
    deriving (Generic, Show, Eq, NFData, Data, Typeable, Ord, Hashable, Store, Display, PersistField, PersistFieldSql)

newtype Storage = Storage SqlBackend

-- | A cryptographic hash of a Cabal file and its size, if known.
--
-- We only keep the size as a @Maybe@ for compatibility with cases
-- where users may not provide the file size. However, for security,
-- they should be provided in all cases.
data CabalHash = CabalHash
  { chHash :: !StaticSHA256
  , chSize :: !(Maybe FileSize)
  }
    deriving (Generic, Show, Eq, Data, Typeable, Ord)
instance Store CabalHash
instance NFData CabalHash
instance Hashable CabalHash

data PantryConfig = PantryConfig
  { pcHackageSecurity :: !HackageSecurityConfig
  , pcRootDir :: !FilePath
  , pcStorage :: !Storage
  , pcUpdateRef :: !(MVar Bool)
    {- FIXME add this shortly
  -- ^ Want to try updating the index once during a single run for missing
  -- package identifiers. We also want to ensure we only update once at a
  -- time. Start at @True@.
  --
  -- TODO: probably makes sense to move this concern into getPackageCaches
  , pcParsedCabalFiles ::
      !(IORef
          ( Map PackageLocation GenericPackageDescription
          , Map FilePath        GenericPackageDescription
          )
       )
  -- ^ Cache of previously parsed cabal files, to save on slow parsing time.
    -}
  }

-- | Location for remote packages (i.e., not local file paths).
data PackageLocation
  = PLHackage !PackageIdentifierRevision
  | PLArchive !Archive
  | PLRepo    !Repo
  deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance NFData PackageLocation
instance Store PackageLocation

instance Display PackageLocation where
  display (PLHackage pir) = display pir <> " (from Hackage)"

-- | A package archive, could be from a URL or a local file
-- path. Local file path archives are assumed to be unchanging
-- over time, and so are allowed in custom snapshots.
data Archive = Archive
  { archiveUrl :: !Text
  , archiveSubdir :: !Text
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
    , repoSubdir :: !Text
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
  deriving (Show, Eq)

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
  toPersistValue (PackageNameP pn) = PersistText $ T.pack $ Distribution.Text.display pn
  fromPersistValue v = do
    str <- fromPersistValue v
    case Distribution.Text.simpleParse str of
      Nothing -> Left $ "Invalid package name: " <> T.pack str
      Just pn -> Right $ PackageNameP pn
instance PersistFieldSql PackageNameP where
  sqlType _ = SqlString

newtype VersionP = VersionP Version
instance PersistField VersionP where
  toPersistValue (VersionP v) = PersistText $ T.pack $ Distribution.Text.display v
  fromPersistValue v = do
    str <- fromPersistValue v
    case Distribution.Text.simpleParse str of
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
  | CFIHash !CabalHash
  -- ^ Identify by contents of the cabal file itself
  | CFIRevision !Revision
  -- ^ Identify by revision number, with 0 being the original and
  -- counting upward.
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Store CabalFileInfo
instance NFData CabalFileInfo
instance Hashable CabalFileInfo

instance Display CabalFileInfo where
  display CFILatest = mempty
  display (CFIHash (CabalHash hash' msize)) =
    "@sha256:" <> display hash' <> maybe mempty (\i -> "," <> display i) msize
  display (CFIRevision rev) = "@rev:" <> display rev

data PackageIdentifierRevision = PackageIdentifierRevision !PackageName !Version !CabalFileInfo
  deriving (Generic, Eq, Ord, Data, Typeable)
instance NFData PackageIdentifierRevision

instance Show PackageIdentifierRevision where
  show = T.unpack . utf8BuilderToText . display

instance Display PackageIdentifierRevision where
  display (PackageIdentifierRevision name version cfi) =
    fromString (Distribution.Text.display name) <> "-" <>
    fromString (Distribution.Text.display version) <>
    display cfi

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
  PackageIdentifier name version <- Distribution.Text.simpleParse $ T.unpack identT
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
        pure $ CFIHash $ CabalHash sha msize
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
      !PackageLocation
      !(Maybe Version)
      ![PError]
      ![PWarning]
  | TreeWithoutCabalFile !PackageLocation
  | TreeWithMultipleCabalFiles !PackageLocation ![SafeFilePath]

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
    display loc <>

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
  deriving (Show, Eq, ToJSON, FromJSON)

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

-- | Modified version of Cabal's parser - we don't need null version
-- in package indentifiers
parsePackageIdentifier :: String -> Maybe PackageIdentifier
parsePackageIdentifier str =
    case [p | (p, s) <- Parse.readP_to_S parser str, all isSpace s] of
        [] -> Nothing
        (p:_) -> Just p
  where
    parser = do
        n <- Distribution.Text.parse
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
  | OSPackageMetadata
      !(Maybe PackageName)
      !(Maybe Version)
      !(Maybe TreeKey)
      !(Maybe BlobKey)
      !(Maybe Text) -- subdir
  deriving Show

osNoInfo :: OptionalSubdirs
osNoInfo = OSPackageMetadata Nothing Nothing Nothing Nothing Nothing

data RawArchive = RawArchive
  { raUrl :: !Text
  , raHash :: !(Maybe StaticSHA256)
  , raSize :: !(Maybe FileSize)
  }
  deriving Show

data RawRepo = RawRepo
  { rrUrl :: !Text
  , rrCommit :: !Text
  , rrType :: !RepoType
  }
  deriving Show

-- | The raw representation of packages allowed in a snapshot
-- specification. Does /not/ allow local filepaths.
data RawPackageLocation
  = RPLHackage !PackageIdentifierRevision !(Maybe TreeKey) !(Maybe BlobKey)
  | RPLArchive !RawArchive !OptionalSubdirs
  | RPLRepo !RawRepo !OptionalSubdirs
  deriving Show
instance ToJSON RawPackageLocation where
  toJSON (RPLHackage pir mtree mcabal) = object $ concat
    [ ["hackage" .= pir]
    , maybe [] (\tree -> ["pantry-tree" .= tree]) mtree
    , maybe [] (\cabal -> ["cabal-file" .= cabal]) mcabal
    ]
  toJSON (RPLArchive (RawArchive url msha msize) os) = object $ concat
    [ ["url" .= url]
    , maybe [] (\sha -> ["sha256" .= sha]) msha
    , maybe [] (\size' -> ["size " .= size']) msize
    , osToPairs os
    ]
  toJSON (RPLRepo (RawRepo url commit typ) os) = object $ concat
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
osToPairs (OSPackageMetadata mname mversion mtree mcabal msubdir) = concat
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
      http = withText "RawPackageLocation.RPLArchive (Text)" $ \t ->
        case parseRequest $ T.unpack t of
          Left  _ -> fail $ "Could not parse URL: " ++ T.unpack t
          Right _ -> pure $ noJSONWarnings $ RPLArchive
                                RawArchive
                                  { raUrl = t
                                  , raHash = Nothing
                                  , raSize = Nothing
                                  }
                                osNoInfo

      hackageText = withText "RawPackageLocation.RPLHackage (Text)" $ \t ->
        case parsePackageIdentifierRevision t of
          Left e -> fail $ show e
          Right pir -> pure $ noJSONWarnings $ RPLHackage pir Nothing Nothing

      hackageObject = withObjectWarnings "RawPackageLocation.RPLHackage" $ \o -> RPLHackage
        <$> o ..: "hackage"
        <*> o ..:? "pantry-key"
        <*> o ..:? "cabal-file"

      optionalSubdirs o =
        (OSSubdirs <$> o ..: "subdirs") <|>
        (OSPackageMetadata
            <$> (fmap unCabalString <$> (o ..:? "name"))
            <*> (fmap unCabalString <$> (o ..:? "version"))
            <*> o ..:? "pantry-tree"
            <*> o ..:? "cabal-file"
            <*> o ..:? "subdir")

      repo = withObjectWarnings "RawPackageLocation.RPLRepo" $ \o -> do
        (rrType, rrUrl) <-
          ((RepoGit, ) <$> o ..: "git") <|>
          ((RepoHg, ) <$> o ..: "hg")
        rrCommit <- o ..: "commit"
        RPLRepo RawRepo {..} <$> optionalSubdirs o

      archiveObject = withObjectWarnings "RawPackageLocation.RPLArchive" $ \o -> do
        raUrl <- o ..: "archive" <|> o ..: "location" <|> o ..: "url"
        raHash <- o ..:? "sha256"
        raSize <- o ..:? "size"
        RPLArchive RawArchive {..} <$> optionalSubdirs o

      github = withObjectWarnings "PLArchive:github" $ \o -> do
        GitHubRepo ghRepo <- o ..: "github"
        commit <- o ..: "commit"
        let raUrl = T.concat
              [ "https://github.com/"
              , ghRepo
              , "/archive/"
              , commit
              , ".tar.gz"
              ]
        raHash <- o ..:? "sha256"
        raSize <- o ..:? "size"
        RPLArchive RawArchive {..} <$> optionalSubdirs o

-- | Newtype wrapper for easier JSON integration with Cabal types.
newtype CabalString a = CabalString { unCabalString :: a }
  deriving (Show, Eq, Ord, Typeable)

instance Distribution.Text.Text a => ToJSON (CabalString a) where
  toJSON = toJSON . Distribution.Text.display . unCabalString
instance Distribution.Text.Text a => ToJSONKey (CabalString a) where
  toJSONKey = toJSONKeyText $ displayC . unCabalString

instance forall a. IsCabalString a => FromJSON (CabalString a) where
  parseJSON = withText name $ \t ->
    case Distribution.Text.simpleParse $ T.unpack t of
      Nothing -> fail $ "Invalid " ++ name ++ ": " ++ T.unpack t
      Just x -> pure $ CabalString x
    where
      name = cabalStringName (Nothing :: Maybe a)
instance forall a. IsCabalString a => FromJSONKey (CabalString a) where
  fromJSONKey =
    FromJSONKeyTextParser $ \t ->
    case Distribution.Text.simpleParse $ T.unpack t of
      Nothing -> fail $ "Invalid " ++ name ++ ": " ++ T.unpack t
      Just x -> pure $ CabalString x
    where
      name = cabalStringName (Nothing :: Maybe a)

class Distribution.Text.Text a => IsCabalString a where
  cabalStringName :: proxy a -> String
instance IsCabalString PackageName where
  cabalStringName _ = "package name"
instance IsCabalString Version where
  cabalStringName _ = "version"
instance IsCabalString PackageIdentifier where
  cabalStringName _ = "package identifier"
instance IsCabalString FlagName where
  cabalStringName _ = "flag name"

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
