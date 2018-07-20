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
  ( parsePackageIdentifier
  , parsePackageIdentifierFromString
  , parsePackageIdentifierRevision
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

{- FIXME
instance ToJSON PackageIdentifier where
  toJSON = toJSON . packageIdentifierString
instance FromJSON PackageIdentifier where
  parseJSON = withText "PackageIdentifier" $ \t ->
    case parsePackageIdentifier t of
      Left e -> fail $ show (e, t)
      Right x -> return x

instance ToJSON PackageIdentifierRevision where
  toJSON = toJSON . packageIdentifierRevisionString
instance FromJSON PackageIdentifierRevision where
  parseJSON = withText "PackageIdentifierRevision" $ \t ->
    case parsePackageIdentifierRevision t of
      Left e -> fail $ show (e, t)
      Right x -> return x
-}

-- | Convenient way to parse a package identifier from a 'Text'.
parsePackageIdentifier :: MonadThrow m => Text -> m PackageIdentifier
parsePackageIdentifier = parsePackageIdentifierFromString . T.unpack

-- | Convenience function for parsing from a 'String'.
parsePackageIdentifierFromString :: MonadThrow m => String -> m PackageIdentifier
parsePackageIdentifierFromString str =
  case parseC str of
    Nothing -> throwM $ PackageIdentifierParseFail $ T.pack str
    Just ident -> pure ident

-- | Parse a 'PackageIdentifierRevision'
parsePackageIdentifierRevision :: MonadThrow m => Text -> m PackageIdentifierRevision
parsePackageIdentifierRevision t = maybe (throwM $ PackageIdentifierRevisionParseFail t) pure $ do
  let (identT, cfiT) = T.break (== '@') t
  PackageIdentifier name version <- parsePackageIdentifier identT
  cfi <- either (const Nothing) Just $ parseOnly (parser <* endOfInput) cfiT
  pure $ PackageIdentifierRevision name version cfi
  where
    parser = cfiHash <|> cfiRevision <|> pure CFILatest

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
