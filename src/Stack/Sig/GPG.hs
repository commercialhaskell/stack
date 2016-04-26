{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Stack.Sig.GPG
Description : GPG Functions
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.GPG (gpgSign, gpgVerify) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>), (<*>))
#endif

import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as C
import           Data.List (find)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Path
import           Stack.Types
import           System.Directory (findExecutable)
import           System.Exit (ExitCode(..))
import           System.Process (ProcessHandle, runInteractiveProcess,
                                 waitForProcess)
import           System.IO (Handle, hGetContents, hPutStrLn)

-- | Sign a file path with GPG, returning the @Signature@.
gpgSign
    :: (MonadIO m, MonadThrow m)
    => Path Abs File -> m Signature
gpgSign path = do
    (_hIn,hOut,hErr,process) <-
        gpg
            [ "--output"
            , "-"
            , "--use-agent"
            , "--detach-sig"
            , "--armor"
            , toFilePath path]
    (out,err,code) <-
        liftIO
            ((,,) <$>
             hGetContents hOut <*>
             hGetContents hErr <*>
             waitForProcess process)
    if code /= ExitSuccess
        then throwM (GPGSignException $ out <> "\n" <> err)
        else return (Signature $ C.pack out)

-- | Verify the @Signature@ of a file path returning the
-- @Fingerprint@.
gpgVerify
    :: (MonadIO m, MonadThrow m)
    => Signature -> Path Abs File -> m Fingerprint
gpgVerify (Signature signature) path = do
    (hIn,hOut,hErr,process) <- gpg ["--verify", "-", toFilePath path]
    (_in,out,err,code) <-
        liftIO
            ((,,,) <$>
             hPutStrLn hIn (C.unpack signature) <*>
             hGetContents hOut <*>
             hGetContents hErr <*>
             waitForProcess process)
    if code /= ExitSuccess
        then throwM (GPGVerifyException (out ++ "\n" ++ err))
        else maybe
                 (throwM
                      (GPGFingerprintException
                           ("unable to extract fingerprint from output\n: " <>
                            out)))
                 return
                 (mkFingerprint . T.pack . concat . drop 3 <$>
                  find
                      ((==) ["Primary", "key", "fingerprint:"] . take 3)
                      (map words (lines err)))

-- | Try to execute `gpg2` but fallback to `gpg` (as a backup)
gpg
    :: (MonadIO m, MonadThrow m)
    => [String] -> m (Handle, Handle, Handle, ProcessHandle)
gpg args = do
    mGpg2Path <- liftIO (findExecutable "gpg2")
    case mGpg2Path of
        Just _ -> liftIO (runInteractiveProcess "gpg2" args Nothing Nothing)
        Nothing -> do
            mGpgPath <- liftIO (findExecutable "gpg")
            case mGpgPath of
                Just _ -> liftIO (runInteractiveProcess "gpg" args Nothing Nothing)
                Nothing -> throwM GPGNotFoundException
