{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ) where

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
import           Pantry
import           Pantry.StaticSHA256
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
  display (PackageIdentifier p v) = display p <> "-" <> display v

instance ToJSON PackageIdentifier where
  toJSON = toJSON . packageIdentifierString
instance FromJSON PackageIdentifier where
  parseJSON = withText "PackageIdentifier" $ \t ->
    case parsePackageIdentifier t of
      Left e -> fail $ show (e, t)
      Right x -> return x

{- FIXME
instance ToJSON PackageIdentifierRevision where
  toJSON = toJSON . packageIdentifierRevisionString
instance FromJSON PackageIdentifierRevision where
  parseJSON = withText "PackageIdentifierRevision" $ \t ->
    case parsePackageIdentifierRevision t of
      Left e -> fail $ show (e, t)
      Right x -> return x
-}

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
     PackageIdentifier name <$> versionParser

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

    parser = do
      PackageIdentifier name version <- packageIdentifierParser
      cfi <- cfiHash <|> cfiRevision <|> pure CFILatest
      pure $ PackageIdentifierRevision (toCabalPackageName name) (toCabalVersion version) cfi

    cfiHash = do
      _ <- string $ T.pack "@sha256:"
      hash' <- A.takeWhile (/= ',')
      hash'' <- either (\e -> fail $ "Invalid SHA256: " ++ show e) return
              $ mkStaticSHA256FromText hash'
      msize <- optional $ do
        _ <- A.char ','
        FileSize <$> A.decimal
      A.endOfInput
      return $ CFIHash $ CabalHash hash'' msize

    cfiRevision = do
      _ <- string $ T.pack "@rev:"
      y <- A.decimal
      A.endOfInput
      return $ CFIRevision $ Revision y
-- | Get a string representation of the package identifier; name-ver.
packageIdentifierString :: PackageIdentifier -> String
packageIdentifierString = T.unpack . packageIdentifierText

-- | Get a string representation of the package identifier with revision; name-ver[@hashtype:hash[,size]].
packageIdentifierRevisionString :: PackageIdentifierRevision -> String
packageIdentifierRevisionString = show

-- | Get a Text representation of the package identifier; name-ver.
packageIdentifierText :: PackageIdentifier -> Text
packageIdentifierText = utf8BuilderToText . display

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
