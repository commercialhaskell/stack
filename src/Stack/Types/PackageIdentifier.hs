{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , staticSHA256ToText
  , staticSHA256ToBase16
  )
  where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad.IO.Unlift
import           Crypto.Hash as Hash (hashlazy, Digest, SHA256)
import           Data.Aeson.Extended
import           Data.Attoparsec.Text as A
import qualified Data.ByteArray.Encoding as Mem (convertToBase, Base(Base16))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Data
import           Data.Hashable
import           Data.Store (Store)
import           Data.Store.Internal (Size (..), StaticSize (..), size,
                                      toStaticSize, toStaticSizeEx, unStaticSize)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Distribution.Package as C
import           GHC.Generics
import           Prelude hiding (FilePath)
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
  , pirRevision :: !(Maybe CabalFileInfo)
  } deriving (Eq,Generic,Data,Typeable)

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
newtype StaticSHA256 = StaticSHA256 (StaticSize 64 ByteString)
    deriving (Generic, Show, Eq, NFData, Data, Typeable, Ord)

instance Store StaticSHA256 where
    size = ConstSize 64
    -- poke (GitSHA1 x) = do
    --   let (sourceFp, sourceOffset, sourceLength) = BSI.toForeignPtr (unStaticSize x)
    --   pokeFromForeignPtr sourceFp sourceOffset sourceLength
    -- peek = do
    --     let len = 20
    --     fp <- peekToPlainForeignPtr ("StaticSize " ++ show len ++ " Data.ByteString.ByteString") len
    --     return (GitSHA1 $ StaticSize (BSI.PS fp 0 len))
    -- {-# INLINE size #-}
    -- {-# INLINE peek #-}
    -- {-# INLINE poke #-}

instance Hashable StaticSHA256 where
  hashWithSalt s (StaticSHA256 x) = hashWithSalt s (unStaticSize x)

-- | Generate a 'StaticSHA256' value from a base16-encoded SHA256 hash.
mkStaticSHA256FromText :: Text -> Maybe StaticSHA256
mkStaticSHA256FromText = fmap StaticSHA256 . toStaticSize . encodeUtf8

-- | Convert a 'StaticSHA256' into a base16-encoded SHA256 hash.
staticSHA256ToText :: StaticSHA256 -> Text
staticSHA256ToText = decodeUtf8 . staticSHA256ToBase16

-- | Convert a 'StaticSHA256' into a base16-encoded SHA256 hash.
staticSHA256ToBase16 :: StaticSHA256 -> ByteString
staticSHA256ToBase16 (StaticSHA256 x) = unStaticSize x

-- | Generate a 'CabalHash' value from a base16-encoded SHA256 hash.
mkCabalHashFromSHA256 :: Text -> Maybe CabalHash
mkCabalHashFromSHA256 = fmap CabalHash . mkStaticSHA256FromText

-- | Convert a 'CabalHash' into a base16-encoded SHA256 hash.
cabalHashToText :: CabalHash -> Text
cabalHashToText = staticSHA256ToText . unCabalHash

-- | Compute a 'CabalHash' value from a cabal file's contents.
computeCabalHash :: L.ByteString -> CabalHash
computeCabalHash = CabalHash . StaticSHA256 . toStaticSizeEx . Mem.convertToBase Mem.Base16 . hashSHA256

hashSHA256 :: L.ByteString -> Hash.Digest Hash.SHA256
hashSHA256 = Hash.hashlazy

showCabalHash :: CabalHash -> Text
showCabalHash = T.append (T.pack "sha256:") . cabalHashToText

-- | Information on the contents of a cabal file
data CabalFileInfo = CabalFileInfo
    { cfiSize :: !(Maybe Int)
    -- ^ File size in bytes
    , cfiHash :: !CabalHash
    -- ^ Hash of the cabal file contents
    }
    deriving (Generic, Show, Eq, Data, Typeable)
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
        <*> optional cabalFileInfo

    cabalFileInfo = do
      _ <- string $ T.pack "@sha256:"
      hash' <- A.takeWhile (/= ',')
      hash'' <- maybe (fail "Invalid SHA256") return
              $ mkCabalHashFromSHA256 hash'
      msize <- optional $ do
        _ <- A.char ','
        A.decimal
      return CabalFileInfo
        { cfiSize = msize
        , cfiHash = hash''
        }

-- | Get a string representation of the package identifier; name-ver.
packageIdentifierString :: PackageIdentifier -> String
packageIdentifierString (PackageIdentifier n v) = show n ++ "-" ++ show v

-- | Get a string representation of the package identifier with revision; name-ver[@hashtype:hash[,size]].
packageIdentifierRevisionString :: PackageIdentifierRevision -> String
packageIdentifierRevisionString (PackageIdentifierRevision ident mcfi) =
  concat $ packageIdentifierString ident : rest
  where
    rest =
      case mcfi of
        Nothing -> []
        Just cfi ->
            "@sha256:"
          : T.unpack (cabalHashToText (cfiHash cfi))
          : showSize (cfiSize cfi)

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
