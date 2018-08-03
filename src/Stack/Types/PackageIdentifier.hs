{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
newtype PackageIdentifierParseFail
  = PackageIdentifierParseFail Text
  deriving (Typeable)
instance Show PackageIdentifierParseFail where
    show (PackageIdentifierParseFail bs) = "Invalid package identifier: " ++ show bs
instance Exception PackageIdentifierParseFail

-- | Convenience function for parsing from a 'String'.
parsePackageIdentifierThrowing :: MonadThrow m => String -> m PackageIdentifier
parsePackageIdentifierThrowing str =
  case parsePackageIdentifier str of
    Nothing -> throwM $ PackageIdentifierParseFail $ T.pack str
    Just ident -> pure ident
