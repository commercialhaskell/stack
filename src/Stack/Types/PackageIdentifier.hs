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
import qualified Data.Text as T

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
