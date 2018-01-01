{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

{-|
Module      : Stack.Types.Sig
Description : Signature Types
Copyright   : (c) 2015-2018, Stack contributors
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Types.Sig
       (Signature(..), Fingerprint, mkFingerprint, SigException(..)) where

import           Stack.Prelude
import           Data.Aeson (Value(..), ToJSON(..), FromJSON(..))
import qualified Data.ByteString as SB
import           Data.Char (isHexDigit)
import qualified Data.Text as T
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
newtype Fingerprint =
    Fingerprint Text
    deriving (Eq,Ord)

mkFingerprint :: Text -> Fingerprint
mkFingerprint = Fingerprint . hexText

hexText :: Text -> Text
hexText = T.toUpper . T.dropWhile (not . isHexDigit)

instance Show Fingerprint where
    show (Fingerprint hex) = T.unpack (hexText hex)

instance FromJSON Fingerprint where
    parseJSON j = Fingerprint . hexText <$> parseJSON j

instance ToJSON Fingerprint where
    toJSON (Fingerprint hex) = String (hexText hex)

instance IsString Fingerprint where
    fromString = Fingerprint . hexText . T.pack

instance FromJSON (Aeson PackageName) where
    parseJSON j = do
        s <- parseJSON j
        case parsePackageName s of
            Just name -> return (Aeson name)
            Nothing -> fail ("Invalid package name: " <> T.unpack s)

-- | Handy wrapper for orphan instances.
newtype Aeson a = Aeson
    { _unAeson :: a
    } deriving (Ord,Eq)

-- | Exceptions
data SigException
    = GPGFingerprintException String
    | GPGNotFoundException
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
    show GPGNotFoundException = "Unable to find gpg2 or gpg executable"
    show (GPGSignException e) = "Error signing with GPG " <> e
    show (GPGVerifyException e) = "Error verifying with GPG " <> e
    show SigNoProjectRootException = "Missing Project Root"
    show SigInvalidSDistTarBall = "Invalid sdist tarball"
    show (SigServiceException e) = "Error with the Signature Service " <> e
