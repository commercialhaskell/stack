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
  , parsePackageIdentifierThrowing
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
  deriving (Typeable)
instance Show PackageIdentifierParseFail where
    show (PackageIdentifierParseFail bs) = "Invalid package identifier: " ++ show bs
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

-- | Convenience function for parsing from a 'String'.
parsePackageIdentifierThrowing :: MonadThrow m => String -> m PackageIdentifier
parsePackageIdentifierThrowing str =
  case parsePackageIdentifier str of
    Nothing -> throwM $ PackageIdentifierParseFail $ T.pack str
    Just ident -> pure ident
