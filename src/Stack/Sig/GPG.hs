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

module Stack.Sig.GPG (signPackage, verifyFile) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>))
#endif

import           Control.Exception (catch, SomeException)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as C
import           Data.List (find)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Path
import           Stack.Types
import           System.Exit (ExitCode(..))
import           System.Process (readProcessWithExitCode)

-- | Sign a file path with GPG, returning the @Signature@.
signPackage
    :: (Monad m, MonadIO m, MonadThrow m)
    => Path Abs File -> m Signature
signPackage path = do
    (code,out,err) <-
        gpg
            [ "--output"
            , "-"
            , "--use-agent"
            , "--detach-sig"
            , "--armor"
            , toFilePath path]
            []
    if code /= ExitSuccess
        then throwM (GPGSignException (out ++ "\n" ++ err))
        else return (Signature (C.pack out))

-- | Verify the @Signature@ of a file path returning the
-- @Fingerprint@.
verifyFile
    :: (Monad m, MonadIO m, MonadThrow m)
    => Signature -> Path Abs File -> m Fingerprint
verifyFile (Signature signature) path = do
    (code,out,err) <-
        gpg ["--verify", "-", toFilePath path] (C.unpack signature)
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
    :: (Monad m, MonadIO m)
    => [String] -> String -> m (ExitCode, String, String)
gpg args stdin =
    liftIO
        (catch
             (readProcessWithExitCode "gpg2" args stdin)
             (oops
                  (catch
                       (readProcessWithExitCode "gpg" args stdin)
                       (return . (, [], [])))))
  where
    oops :: IO a -> SomeException -> IO a
    oops = const
