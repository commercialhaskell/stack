{-# LANGUAGE CPP             #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Stack.Sig.GPG
Description : GPG Functions
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.GPG (fullFingerprint, signPackage, verifyFile)
       where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>))
#endif

import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as C
import           Data.Char (isSpace)
import           Data.List (find)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Path
import           Stack.Types
import           System.Exit (ExitCode(..))
import           System.Process (readProcessWithExitCode)

-- | Extract the full long @fingerprint@ given a short (or long)
-- @fingerprint@
fullFingerprint
    :: (Monad m, MonadIO m, MonadThrow m)
    => Fingerprint -> m Fingerprint
fullFingerprint (Fingerprint fp) = do
    (code,out,err) <-
        liftIO
            (readProcessWithExitCode "gpg" ["--fingerprint", T.unpack fp] [])
    if code /= ExitSuccess
        then throwM (GPGFingerprintException (out ++ "\n" ++ err))
        else maybe
                 (throwM
                      (GPGFingerprintException
                           ("unable to extract full fingerprint from output:\n " <>
                            out)))
                 return
                 (let hasFingerprint =
                          (==) ["Key", "fingerprint", "="] . take 3
                      fingerprint =
                          T.filter (not . isSpace) . T.pack . unwords . drop 3
                  in Fingerprint . fingerprint <$>
                     find hasFingerprint (map words (lines out)))

-- | Sign a file path with GPG, returning the @Signature@.
signPackage
    :: (Monad m, MonadIO m, MonadThrow m)
    => Path Abs File -> m Signature
signPackage path = do
    (code,out,err) <-
        liftIO
            (readProcessWithExitCode
                 "gpg"
                 [ "--output"
                 , "-"
                 , "--use-agent"
                 , "--detach-sig"
                 , "--armor"
                 , toFilePath path]
                 [])
    if code /= ExitSuccess
        then throwM (GPGSignException (out ++ "\n" ++ err))
        else return (Signature (C.pack out))

-- | Verify the @Signature@ of a file path returning the
-- @Fingerprint@.
verifyFile
    :: (Monad m, MonadIO m, MonadThrow m)
    => Signature -> Path Abs File -> m Fingerprint
verifyFile (Signature signature) path = do
    let process =
            readProcessWithExitCode
                "gpg"
                ["--verify", "-", toFilePath path]
                (C.unpack signature)
    (code,out,err) <- liftIO process
    if code /= ExitSuccess
        then throwM (GPGVerifyException (out ++ "\n" ++ err))
        else maybe
                 (throwM
                      (GPGFingerprintException
                           ("unable to extract short fingerprint from output\n: " <>
                            out)))
                 return
                 (let hasFingerprint =
                          (==) ["gpg:", "Signature", "made"] . take 3
                      fingerprint = T.pack . last
                  in Fingerprint . fingerprint <$>
                     find hasFingerprint (map words (lines err)))
