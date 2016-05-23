{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Tag a Store instance with structural version info to ensure we're
-- reading a compatible format.
module Data.Store.VersionTagged
    ( taggedDecodeOrLoad
    , taggedEncodeFile
    , decodeFileMaybe
    ) where

import Control.Exception.Lifted (catch, IOException, assert)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import Data.Store
import Data.Store.TypeHash
import qualified Data.Text as T
import Path
import Path.IO (ensureDir)

-- | Write to the given file, with a binary-tagged tag.
taggedEncodeFile :: (Store a, HasTypeHash a, MonadIO m, MonadLogger m, Eq a)
                 => Path Abs File
                 -> a
                 -> m ()
taggedEncodeFile fp x = do
    let fpt = T.pack (toFilePath fp)
    $logDebug $ "Encoding " <> fpt
    ensureDir (parent fp)
    let encoded = encode (Tagged x)
    assert (decodeEx encoded == Tagged x) $ liftIO $ BS.writeFile (toFilePath fp) encoded
    $logDebug $ "Finished writing " <> fpt

-- | Read from the given file. If the read fails, run the given action and
-- write that back to the file. Always starts the file off with the
-- version tag.
taggedDecodeOrLoad :: (Store a, HasTypeHash a, Eq a, MonadIO m, MonadLogger m, MonadBaseControl IO m)
                   => Path Abs File
                   -> m a
                   -> m a
taggedDecodeOrLoad fp mx = do
    let fpt = T.pack (toFilePath fp)
    $logDebug $ "Trying to decode " <> fpt
    mres <- decodeFileMaybe fp
    case mres of
        Nothing -> do
            $logDebug $ "Failure decoding " <> fpt
            x <- mx
            taggedEncodeFile fp x
            return x
        Just x -> do
            $logDebug $ "Success decoding " <> fpt
            return x

decodeFileMaybe :: (Store a, HasTypeHash a, MonadIO m, MonadLogger m, MonadBaseControl IO m)
                => Path loc File
                -> m (Maybe a)
decodeFileMaybe fp = do
    mbs <- liftIO (Just <$> BS.readFile (toFilePath fp)) `catch` \(err :: IOException) -> do
        $logDebug ("Exception ignored when attempting to load " <> T.pack (toFilePath fp) <> ": " <> T.pack (show err))
        return Nothing
    case mbs of
        Nothing -> return Nothing
        Just bs ->
            liftIO (do (Tagged res) <- decodeIO bs
                       return (Just res)) `catch` \(err :: PeekException) -> do
                 let fpt = T.pack (toFilePath fp)
                 $logDebug ("Error while decoding " <> fpt <> ": " <> T.pack (show err) <> " (this might not be an error, when switching between stack versions)")
                 return Nothing
