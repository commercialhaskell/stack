{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Package identifier (name-version).

module Stack.Types.PackageIdentifier
  ( PackageIdentifier(..)
  , PackageIdentifierRevision(..)
  , CabalHash
  , mkCabalHashFromSHA256
  , computeCabalHash
  , showCabalHash
  , CabalFileInfo(..)
  , toTuple
  , fromTuple
  , parsePackageIdentifier
  , parsePackageIdentifierFromString
  , parsePackageIdentifierRevision
  , packageIdentifierParser
  , packageIdentifierString
  , packageIdentifierRevisionString
  , packageIdentifierText
  , toCabalPackageIdentifier
  , fromCabalPackageIdentifier
  , StaticSHA256
  , mkStaticSHA256FromText
  , mkStaticSHA256FromFile
  , mkStaticSHA256FromDigest
  , staticSHA256ToText
  , staticSHA256ToBase16
  , staticSHA256ToRaw
  )
  where

import           Stack.Prelude
import           Crypto.Hash.Conduit (hashFile)
import           Crypto.Hash as Hash (hashlazy, Digest, SHA256)
import           Data.Aeson.Extended
import           Data.Attoparsec.Text as A
import qualified Data.ByteArray
import qualified Data.ByteArray.Encoding as Mem
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Distribution.Package as C
import           Stack.StaticBytes
import           Stack.Types.PackageName
import           Stack.Types.Version

-- | A parse fail.
data PackageIdentifierParseFail
  = PackageIdentifierParseFail Text
  | PackageIdentifierRevisionParseFail Text
  deriving (Typeable)
instance Show PackageIdentifierParseFail where
    show (PackageIdentifierParseFail bs) = "Invalid package identifier: " ++ show bs
    show (PackageIdentifierRevisionParseFail bs) = "Invalid package identifier (with optional revision): " ++ show bs
instance Exception PackageIdentifierParseFail

-- | A pkg-ver combination.
data PackageIdentifier = PackageIdentifier
  { -- | Get the name part of the identifier.
    packageIdentifierName    :: !PackageName
    -- | Get the version part of the identifier.
  , packageIdentifierVersion :: !Version
  } deriving (Eq,Ord,Generic,Data,Typeable)

instance NFData PackageIdentifier where
  rnf (PackageIdentifier !p !v) =
      seq (rnf p) (rnf v)

instance Hashable PackageIdentifier
instance Store PackageIdentifier

instance Show PackageIdentifier where
  show = show . packageIdentifierString
instance Display PackageIdentifier where
  display = fromString . packageIdentifierString

instance ToJSON PackageIdentifier where
  toJSON = toJSON . packageIdentifierString
instance FromJSON PackageIdentifier where
  parseJSON = withText "PackageIdentifier" $ \t ->
    case parsePackageIdentifier t of
      Left e -> fail $ show (e, t)
      Right x -> return x

-- | A 'PackageIdentifier' combined with optionally specified Hackage
-- cabal file revision.
data PackageIdentifierRevision = PackageIdentifierRevision
  { pirIdent :: !PackageIdentifier
  , pirRevision :: !CabalFileInfo
  } deriving (Eq,Ord,Generic,Data,Typeable)

instance NFData PackageIdentifierRevision where
  rnf (PackageIdentifierRevision !i !c) =
      seq (rnf i) (rnf c)

instance Hashable PackageIdentifierRevision
instance Store PackageIdentifierRevision

instance Show PackageIdentifierRevision where
  show = show . packageIdentifierRevisionString

instance ToJSON PackageIdentifierRevision where
  toJSON = toJSON . packageIdentifierRevisionString
instance FromJSON PackageIdentifierRevision where
  parseJSON = withText "PackageIdentifierRevision" $ \t ->
    case parsePackageIdentifierRevision t of
      Left e -> fail $ show (e, t)
      Right x -> return x

-- | A cryptographic hash of a Cabal file.
newtype CabalHash = CabalHash { unCabalHash :: StaticSHA256 }
    deriving (Generic, Show, Eq, NFData, Data, Typeable, Ord, Store, Hashable)

-- | A SHA256 hash, stored in a static size for more efficient
-- serialization with store.
newtype StaticSHA256 = StaticSHA256 Bytes32
    deriving (Generic, Show, Eq, NFData, Data, Typeable, Ord, Hashable, Store)

-- | Generate a 'StaticSHA256' value from a base16-encoded SHA256 hash.
mkStaticSHA256FromText :: Text -> Either SomeException StaticSHA256
mkStaticSHA256FromText t =
  mapLeft (toException . stringException) (Mem.convertFromBase Mem.Base16 (encodeUtf8 t))
  >>= either (Left . toE) (Right . StaticSHA256)
    . toStaticExact
    . (id :: ByteString -> ByteString)
  where
    toE e = toException $ stringException $ concat
      [ "Unable to convert "
      , show t
      , " into SHA256: "
      , show e
      ]

-- | Generate a 'StaticSHA256' value from the contents of a file.
mkStaticSHA256FromFile :: MonadIO m => Path Abs File -> m StaticSHA256
mkStaticSHA256FromFile fp = liftIO $ mkStaticSHA256FromDigest <$> hashFile (toFilePath fp)

mkStaticSHA256FromDigest :: Hash.Digest Hash.SHA256 -> StaticSHA256
mkStaticSHA256FromDigest digest
  = StaticSHA256
  $ either impureThrow id
  $ toStaticExact
    (Data.ByteArray.convert digest :: ByteString)

-- | Convert a 'StaticSHA256' into a base16-encoded SHA256 hash.
staticSHA256ToText :: StaticSHA256 -> Text
staticSHA256ToText = decodeUtf8 . staticSHA256ToBase16

-- | Convert a 'StaticSHA256' into a base16-encoded SHA256 hash.
staticSHA256ToBase16 :: StaticSHA256 -> ByteString
staticSHA256ToBase16 (StaticSHA256 x) = Mem.convertToBase Mem.Base16 x

staticSHA256ToRaw :: StaticSHA256 -> ByteString
staticSHA256ToRaw (StaticSHA256 x) = Data.ByteArray.convert x

-- | Generate a 'CabalHash' value from a base16-encoded SHA256 hash.
mkCabalHashFromSHA256 :: Text -> Either SomeException CabalHash
mkCabalHashFromSHA256 = fmap CabalHash . mkStaticSHA256FromText

-- | Convert a 'CabalHash' into a base16-encoded SHA256 hash.
cabalHashToText :: CabalHash -> Text
cabalHashToText = staticSHA256ToText . unCabalHash

-- | Compute a 'CabalHash' value from a cabal file's contents.
computeCabalHash :: L.ByteString -> CabalHash
computeCabalHash = CabalHash . mkStaticSHA256FromDigest . Hash.hashlazy

showCabalHash :: CabalHash -> Text
showCabalHash = T.append (T.pack "sha256:") . cabalHashToText

-- | Information on the contents of a cabal file
data CabalFileInfo
  = CFILatest
  -- ^ Take the latest revision of the cabal file available. This
  -- isn't reproducible at all, but the running assumption (not
  -- necessarily true) is that cabal file revisions do not change
  -- semantics of the build.
  | CFIHash
      !(Maybe Int) -- file size in bytes
      !CabalHash
  -- ^ Identify by contents of the cabal file itself
  | CFIRevision !Word
  -- ^ Identify by revision number, with 0 being the original and
  -- counting upward.
    deriving (Generic, Show, Eq, Ord, Data, Typeable)
instance Store CabalFileInfo
instance NFData CabalFileInfo
instance Hashable CabalFileInfo

-- | Convert from a package identifier to a tuple.
toTuple :: PackageIdentifier -> (PackageName,Version)
toTuple (PackageIdentifier n v) = (n,v)

-- | Convert from a tuple to a package identifier.
fromTuple :: (PackageName,Version) -> PackageIdentifier
fromTuple (n,v) = PackageIdentifier n v

-- | A parser for a package-version pair.
packageIdentifierParser :: Parser PackageIdentifier
packageIdentifierParser =
  do name <- packageNameParser
     char '-'
     version <- versionParser
     return (PackageIdentifier name version)

-- | Convenient way to parse a package identifier from a 'Text'.
parsePackageIdentifier :: MonadThrow m => Text -> m PackageIdentifier
parsePackageIdentifier x = go x
  where go =
          either (const (throwM (PackageIdentifierParseFail x))) return .
          parseOnly (packageIdentifierParser <* endOfInput)

-- | Convenience function for parsing from a 'String'.
parsePackageIdentifierFromString :: MonadThrow m => String -> m PackageIdentifier
parsePackageIdentifierFromString =
  parsePackageIdentifier . T.pack

-- | Parse a 'PackageIdentifierRevision'
parsePackageIdentifierRevision :: MonadThrow m => Text -> m PackageIdentifierRevision
parsePackageIdentifierRevision x = go x
  where
    go =
      either (const (throwM (PackageIdentifierRevisionParseFail x))) return .
      parseOnly (parser <* endOfInput)

    parser = PackageIdentifierRevision
        <$> packageIdentifierParser
        <*> (cfiHash <|> cfiRevision <|> pure CFILatest)

    cfiHash = do
      _ <- string $ T.pack "@sha256:"
      hash' <- A.takeWhile (/= ',')
      hash'' <- either (\e -> fail $ "Invalid SHA256: " ++ show e) return
              $ mkCabalHashFromSHA256 hash'
      msize <- optional $ do
        _ <- A.char ','
        A.decimal
      A.endOfInput
      return $ CFIHash msize hash''

    cfiRevision = do
      _ <- string $ T.pack "@rev:"
      y <- A.decimal
      A.endOfInput
      return $ CFIRevision y
-- | Get a string representation of the package identifier; name-ver.
packageIdentifierString :: PackageIdentifier -> String
packageIdentifierString (PackageIdentifier n v) = show n ++ "-" ++ show v

-- | Get a string representation of the package identifier with revision; name-ver[@hashtype:hash[,size]].
packageIdentifierRevisionString :: PackageIdentifierRevision -> String
packageIdentifierRevisionString (PackageIdentifierRevision ident cfi) =
  concat $ packageIdentifierString ident : rest
  where
    rest =
      case cfi of
        CFILatest -> []
        CFIHash msize hash' ->
            "@sha256:"
          : T.unpack (cabalHashToText hash')
          : showSize msize
        CFIRevision rev -> ["@rev:", show rev]

    showSize Nothing = []
    showSize (Just int) = [',' : show int]

-- | Get a Text representation of the package identifier; name-ver.
packageIdentifierText :: PackageIdentifier -> Text
packageIdentifierText = T.pack .  packageIdentifierString

toCabalPackageIdentifier :: PackageIdentifier -> C.PackageIdentifier
toCabalPackageIdentifier x =
    C.PackageIdentifier
        (toCabalPackageName (packageIdentifierName x))
        (toCabalVersion (packageIdentifierVersion x))

fromCabalPackageIdentifier :: C.PackageIdentifier -> PackageIdentifier
fromCabalPackageIdentifier (C.PackageIdentifier name version) =
    PackageIdentifier
        (fromCabalPackageName name)
        (fromCabalVersion version)
