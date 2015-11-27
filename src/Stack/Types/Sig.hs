{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-|
Module      : Stack.Types.Sig
Description : Signature Types
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Types.Sig
       (Signature(..), Fingerprint(..), SigException(..))
       where

import           Control.Exception (Exception)
import           Data.Aeson (Value(..), ToJSON(..), FromJSON(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import           Data.Char (isDigit, isAlpha, isSpace)
import           Data.Monoid ((<>))
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable (Typeable)
import           Stack.Types.PackageName

-- | A GPG signature.
newtype Signature =
    Signature ByteString
    deriving (Ord,Eq)

instance Show Signature where
    show (Signature s) = "Signature " ++
        (if SB.length s > 140
             then show (SB.take 140 s) ++
                  "..."
             else show (SB.take 140 s))

-- | The GPG fingerprint.
newtype Fingerprint = Fingerprint
    { fingerprintSample :: Text
    } deriving (Eq,Ord,Show)

instance FromJSON Fingerprint where
    parseJSON j = do
        s <- parseJSON j
        let withoutSpaces = T.filter (not . isSpace) s
        if T.null withoutSpaces ||
           T.all
               (\c ->
                     isAlpha c || isDigit c || isSpace c)
               withoutSpaces
            then return (Fingerprint withoutSpaces)
            else fail ("Expected fingerprint, but got: " ++ T.unpack s)

instance ToJSON Fingerprint where
    toJSON (Fingerprint txt) = String txt

instance IsString Fingerprint where
    fromString = Fingerprint . T.pack

instance FromJSON (Aeson PackageName) where
    parseJSON j = do
        s <- parseJSON j
        case (parsePackageName . T.encodeUtf8) s of
            Just name -> return (Aeson name)
            Nothing -> fail ("Invalid package name: " <> T.unpack s)

-- | Handy wrapper for orphan instances.
newtype Aeson a = Aeson
    { _unAeson :: a
    } deriving (Ord,Eq)

-- | Exceptions
data SigException
    = GPGFingerprintException String
    | GPGSignException String
    | GPGVerifyException String
    | SigInvalidSDistTarBall
    | SigNoProjectRootException
    | SigServiceException String
    deriving (Typeable)

instance Exception SigException

instance Show SigException where
    show (GPGFingerprintException e) =
        "Error extracting a GPG fingerprint " <> e
    show (GPGSignException e) = "Error signing with GPG " <> e
    show (GPGVerifyException e) = "Error verifying with GPG " <> e
    show SigNoProjectRootException = "Missing Project Root"
    show SigInvalidSDistTarBall = "Invalid sdist tarball"
    show (SigServiceException e) = "Error with the Signature Service " <> e
