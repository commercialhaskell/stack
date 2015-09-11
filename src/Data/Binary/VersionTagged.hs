{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Tag a Binary instance with the stack version number to ensure we're
-- reading a compatible format.
module Data.Binary.VersionTagged
    ( taggedDecodeOrLoad
    , taggedEncodeFile
    , Binary (..)
    , BinarySchema
    , HasStructuralInfo
    , HasSemanticVersion
    , decodeFileOrFailDeep
    , NFData (..)
    ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Data.Binary (Binary (..))
import Data.Binary.Get (ByteOffset)
import Data.Binary.Tagged (HasStructuralInfo, HasSemanticVersion)
import qualified Data.Binary.Tagged as BinaryTagged
import Data.Typeable (Typeable)
import Control.Exception.Enclosed (tryAnyDeep)
import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Text as T

type BinarySchema a = (Binary a, NFData a, HasStructuralInfo a, HasSemanticVersion a)

-- | Write to the given file, with a binary-tagged tag.
taggedEncodeFile :: (BinarySchema a, MonadIO m)
                 => FilePath
                 -> a
                 -> m ()
taggedEncodeFile fp x = liftIO $ do
    createDirectoryIfMissing True $ takeDirectory fp
    BinaryTagged.taggedEncodeFile fp x

-- | Read from the given file. If the read fails, run the given action and
-- write that back to the file. Always starts the file off with the version
-- tag.
taggedDecodeOrLoad :: (BinarySchema a, MonadIO m, MonadLogger m)
                   => FilePath
                   -> m a
                   -> m a
taggedDecodeOrLoad fp mx = do
    $logDebug $ T.pack $ "Trying to decode " ++ fp
    eres <- decodeFileOrFailDeep fp
    case eres of
        Left _ -> do
            $logDebug $ T.pack $ "Failure decoding " ++ fp
            x <- mx
            taggedEncodeFile fp x
            return x
        Right x -> do
            $logDebug $ T.pack $ "Success decoding " ++ fp
            return x

-- | Ensure that there are no lurking exceptions deep inside the parsed
-- value... because that happens unfortunately. See
-- https://github.com/commercialhaskell/stack/issues/554
decodeFileOrFailDeep :: (BinarySchema a, MonadIO m, MonadThrow n)
                     => FilePath
                     -> m (n a)
decodeFileOrFailDeep fp = liftIO $ fmap (either throwM return) $ tryAnyDeep $ do
    eres <- BinaryTagged.taggedDecodeFileOrFail fp
    case eres of
        Left (offset, str) -> throwM $ DecodeFileFailure fp offset str
        Right x -> return x

data DecodeFileFailure = DecodeFileFailure FilePath ByteOffset String
    deriving Typeable
instance Show DecodeFileFailure where
    show (DecodeFileFailure fp offset str) = concat
        [ "Decoding of "
        , fp
        , " failed at offset "
        , show offset
        , ": "
        , str
        ]
instance Exception DecodeFileFailure
