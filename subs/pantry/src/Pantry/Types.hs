{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.Types
  ( PantryConfig (..)
  , HackageSecurityConfig (..)
  , Storage (..)
  , HasPantryConfig (..)
  , BlobKey (..)
  , PackageName
  , Version
  , Revision (..)
  , CabalHash (..)
  , CabalFileInfo (..)
  , PackageNameP (..)
  , VersionP (..)
  , displayPackageIdentifierRevision
  , FileType (..)
  , TreeEntry (..)
  , SafeFilePath
  , unSafeFilePath
  , mkSafeFilePath
  , TreeKey (..)
  , Tree (..)
  , renderTree
  , parseTree
  , PackageTarball (..)
  ) where

import RIO
import qualified RIO.Text as T
import qualified RIO.ByteString.Lazy as BL
import Data.ByteString.Builder (toLazyByteString)
import Data.Pool (Pool)
import Database.Persist
import Database.Persist.Sql
import Pantry.StaticSHA256
import Distribution.Types.PackageName (PackageName)
import qualified Distribution.Text
import Distribution.Types.Version (Version)
import Data.Store (Store) -- FIXME remove

newtype Revision = Revision Word
    deriving (Generic, Show, Eq, NFData, Data, Typeable, Ord, Hashable, Store, Display, PersistField, PersistFieldSql)

newtype Storage = Storage (Pool SqlBackend)

-- | A cryptographic hash of a Cabal file and its size, if known.
data CabalHash = CabalHash
  { chHash :: !StaticSHA256
  , chSize :: !(Maybe Word)
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
  -- ^ Want to try updating the index once during a single run for missing
  -- package identifiers. We also want to ensure we only update once at a
  -- time. Start at @True@.
  --
  -- TODO: probably makes sense to move this concern into getPackageCaches
  }

data HackageSecurityConfig = HackageSecurityConfig
  { hscKeyIds :: ![Text]
  , hscKeyThreshold :: !Int
  , hscDownloadPrefix :: !Text
  }

class HasPantryConfig env where
  pantryConfigL :: Lens' env PantryConfig

newtype BlobKey = BlobKey StaticSHA256
  deriving (Show, PersistField, PersistFieldSql)

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

displayPackageIdentifierRevision
  :: PackageName
  -> Version
  -> CabalFileInfo
  -> Utf8Builder
displayPackageIdentifierRevision name version cfi =
  fromString (Distribution.Text.display name) <> "-" <>
  fromString (Distribution.Text.display version) <>
  display cfi

data FileType = FTNormal | FTExecutable
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
mkSafeFilePath = undefined

newtype TreeKey = TreeKey StaticSHA256
  deriving (Show, Eq, Ord, PersistField, PersistFieldSql)

data Tree
  = TreeMap !(Map SafeFilePath TreeEntry)
  | TreeTarball !PackageTarball

renderTree :: Tree -> ByteString
renderTree _tree = BL.toStrict $ toLazyByteString undefined

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
