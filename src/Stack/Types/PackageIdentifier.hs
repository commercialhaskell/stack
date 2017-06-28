{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Package identifier (name-version).

module Stack.Types.PackageIdentifier
  ( PackageIdentifier(..)
  , PackageIdentifierRevision(..)
  , GitSHA1(..) -- FIXME don't expose constructor, replace with a better hash/name
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
  , toCabalPackageIdentifier )
  where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception (Exception)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Data.Aeson.Extended
import           Data.Attoparsec.Text as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Data
import           Data.Hashable
import           Data.Store (Store)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
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

-- | A SHA1 hash, but in Git format. This means that the contents are
-- prefixed with @blob@ and the size of the payload before hashing, as
-- Git itself does.
newtype GitSHA1 = GitSHA1 { unGitSHA1 :: ByteString } -- FIXME replace with Text? Or a digest value?
    deriving (Generic, Show, Eq, NFData, Store, Data, Typeable, Ord, Hashable)

-- | Information on the contents of a cabal file
data CabalFileInfo = CabalFileInfo
    { cfiSize :: !(Maybe Int)
    -- ^ File size in bytes
    , cfiGitSHA1 :: !GitSHA1
    -- ^ 'GitSHA1' of the cabal file contents
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
      _ <- string $ T.pack "@gitsha1:"
      hash <- A.takeWhile (/= ',')
      msize <- optional $ do
        _ <- A.char ','
        A.decimal
      return CabalFileInfo
        { cfiSize = msize
        , cfiGitSHA1 = GitSHA1 $ encodeUtf8 hash
        }

-- | Get a string representation of the package identifier; name-ver.
packageIdentifierString :: PackageIdentifier -> String
packageIdentifierString (PackageIdentifier n v) = show n ++ "-" ++ show v

-- | Get a string representation of the package identifier with revision; name-ver[@hashtype:hash[,size]].
packageIdentifierRevisionString :: PackageIdentifierRevision -> String
packageIdentifierRevisionString (PackageIdentifierRevision ident mcfi) =
  concat $ show ident : rest
  where
    rest =
      case mcfi of
        Nothing -> []
        Just cfi ->
            "@gitsha1:"
          : S8.unpack (unGitSHA1 $ cfiGitSHA1 cfi)
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
