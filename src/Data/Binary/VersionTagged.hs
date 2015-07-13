{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Tag a Binary instance with the stack version number to ensure we're
-- reading a compatible format.
module Data.Binary.VersionTagged
    ( taggedDecodeOrLoad
    , taggedEncodeFile
    , Binary (..)
    , BinarySchema (..)
    , decodeFileOrFailDeep
    , encodeFile
    , NFData (..)
    , genericRnf
    ) where

import Control.DeepSeq.Generics (NFData (..), genericRnf)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Binary (Binary (..), encodeFile, decodeFileOrFail, putWord8, getWord8)
import Data.Binary.Get (ByteOffset)
import Data.Typeable (Typeable)
import Control.Exception.Enclosed (tryAnyDeep)
import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString as S
import Data.ByteString (ByteString)
import Control.Monad (forM_, when)
import Data.Proxy

magic :: ByteString
magic = "stack"

-- | A @Binary@ instance that also has a schema version
class (Binary a, NFData a) => BinarySchema a where
    binarySchema :: Proxy a -> Int

newtype WithTag a = WithTag a
    deriving NFData
instance forall a. BinarySchema a => Binary (WithTag a) where
    get = do
        forM_ (S.unpack magic) $ \w -> do
            w' <- getWord8
            when (w /= w')
                $ fail "Mismatched magic string, forcing a recompute"
        tag' <- get
        if binarySchema (Proxy :: Proxy a) == tag'
            then fmap WithTag get
            else fail "Mismatched tags, forcing a recompute"
    put (WithTag x) = do
        mapM_ putWord8 $ S.unpack magic
        put (binarySchema (Proxy :: Proxy a))
        put x

-- | Write to the given file, with a version tag.
taggedEncodeFile :: (BinarySchema a, MonadIO m)
                 => FilePath
                 -> a
                 -> m ()
taggedEncodeFile fp x = liftIO $ do
    createDirectoryIfMissing True $ takeDirectory fp
    encodeFile fp $ WithTag x

-- | Read from the given file. If the read fails, run the given action and
-- write that back to the file. Always starts the file off with the version
-- tag.
taggedDecodeOrLoad :: (BinarySchema a, MonadIO m)
                   => FilePath
                   -> m a
                   -> m a
taggedDecodeOrLoad fp mx = do
    eres <- decodeFileOrFailDeep fp
    case eres of
        Left _ -> do
            x <- mx
            taggedEncodeFile fp x
            return x
        Right (WithTag x) -> return x

-- | Ensure that there are no lurking exceptions deep inside the parsed
-- value... because that happens unfortunately. See
-- https://github.com/commercialhaskell/stack/issues/554
decodeFileOrFailDeep :: (Binary a, NFData a, MonadIO m, MonadThrow n)
                     => FilePath
                     -> m (n a)
decodeFileOrFailDeep fp = liftIO $ fmap (either throwM return) $ tryAnyDeep $ do
    eres <- decodeFileOrFail fp
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
