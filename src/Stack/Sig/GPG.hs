{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Stack.Sig.GPG
Description : GPG Functions
Copyright   : (c) 2015-2019, Stack contributors
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Stack.Sig.GPG (gpgSign, gpgVerify) where

import           Stack.Prelude
import qualified Data.ByteString.Char8 as C
import           Data.List (find, isPrefixOf)
import qualified Data.Text as T
import           Stack.Types.Sig
import           System.Directory (findExecutable)
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode(..))
import           System.IO (hGetContents, hPutStrLn)
import           System.Info (os)
import           System.Process (ProcessHandle, runInteractiveProcess,
                                 waitForProcess)

-- | Sign a file path with GPG, returning the @Signature@.
gpgSign
    :: HasLogFunc env
    => Path Abs File -> RIO env Signature
gpgSign path = do
    gpgWarnTTY
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
    (hIn,hOut,hErr,process) <-
        gpg ["--verify", "--with-fingerprint", "-", toFilePath path]
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
                Just _ ->
                    liftIO (runInteractiveProcess "gpg" args Nothing Nothing)
                Nothing -> throwM GPGNotFoundException

-- | `man gpg-agent` shows that you need GPG_TTY environment variable set to
-- properly deal with interactions with gpg-agent. (Doesn't apply to Windows
-- though)
gpgWarnTTY :: HasLogFunc env => RIO env ()
gpgWarnTTY =
    unless
        ("ming" `isPrefixOf` os)
        (do mTTY <- liftIO (lookupEnv "GPG_TTY")
            when
                (null mTTY)
                (logWarn
                     "Environment variable GPG_TTY is not set (see `man gpg-agent`)"))
