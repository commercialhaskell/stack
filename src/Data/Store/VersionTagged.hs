{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
-- | Tag a Store instance with structural version info to ensure we're
-- reading a compatible format.
module Data.Store.VersionTagged
    ( versionedEncodeFile
    , versionedDecodeOrLoad
    , versionedDecodeFile
    , storeVersionConfig
    ) where

import Control.Applicative
import Control.Exception.Lifted (catch, IOException, assert)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString as BS
import Data.Data (Data)
import qualified Data.Map as M
import Data.Monoid ((<>))
import qualified Data.Set as S
import Data.Store
import Data.Store.Core (unsafeEncodeWith)
import Data.Store.Version
import qualified Data.Text as T
import Language.Haskell.TH
import Path
import Path.IO (ensureDir)
import Prelude

versionedEncodeFile :: Data a => VersionConfig a -> Q Exp
versionedEncodeFile vc = [e| storeEncodeFile $(encodeWithVersionQ vc) $(decodeWithVersionQ vc) |]

versionedDecodeOrLoad :: Data a => VersionConfig a -> Q Exp
versionedDecodeOrLoad vc = [| versionedDecodeOrLoadImpl $(encodeWithVersionQ vc) $(decodeWithVersionQ vc) |]

versionedDecodeFile :: Data a => VersionConfig a -> Q Exp
versionedDecodeFile vc = [e| versionedDecodeFileImpl $(decodeWithVersionQ vc) |]

-- | Write to the given file.
storeEncodeFile :: (Store a, MonadIO m, MonadLogger m, Eq a)
                => (a -> (Int, Poke ()))
                -> Peek a
                -> Path Abs File
                -> a
                -> m ()
storeEncodeFile pokeFunc peekFunc fp x = do
    let fpt = T.pack (toFilePath fp)
    $logDebug $ "Encoding " <> fpt
    ensureDir (parent fp)
    let (sz, poker) = pokeFunc x
        encoded = unsafeEncodeWith poker sz
    assert (decodeExWith peekFunc encoded == x) $ liftIO $ BS.writeFile (toFilePath fp) encoded
    $logDebug $ "Finished writing " <> fpt

-- | Read from the given file. If the read fails, run the given action and
-- write that back to the file. Always starts the file off with the
-- version tag.
versionedDecodeOrLoadImpl :: (Store a, Eq a, MonadIO m, MonadLogger m, MonadBaseControl IO m)
                          => (a -> (Int, Poke ()))
                          -> Peek a
                          -> Path Abs File
                          -> m a
                          -> m a
versionedDecodeOrLoadImpl pokeFunc peekFunc fp mx = do
    let fpt = T.pack (toFilePath fp)
    $logDebug $ "Trying to decode " <> fpt
    mres <- versionedDecodeFileImpl peekFunc fp
    case mres of
        Just x -> do
            $logDebug $ "Success decoding " <> fpt
            return x
        _ -> do
            $logDebug $ "Failure decoding " <> fpt
            x <- mx
            storeEncodeFile pokeFunc peekFunc fp x
            return x

versionedDecodeFileImpl :: (Store a, MonadIO m, MonadLogger m, MonadBaseControl IO m)
                        => Peek a
                        -> Path loc File
                        -> m (Maybe a)
versionedDecodeFileImpl peekFunc fp = do
    mbs <- liftIO (Just <$> BS.readFile (toFilePath fp)) `catch` \(err :: IOException) -> do
        $logDebug ("Exception ignored when attempting to load " <> T.pack (toFilePath fp) <> ": " <> T.pack (show err))
        return Nothing
    case mbs of
        Nothing -> return Nothing
        Just bs ->
            liftIO (Just <$> decodeIOWith peekFunc bs) `catch` \(err :: PeekException) -> do
                 let fpt = T.pack (toFilePath fp)
                 $logDebug ("Error while decoding " <> fpt <> ": " <> T.pack (show err) <> " (this might not be an error, when switching between stack versions)")
                 return Nothing

storeVersionConfig :: String -> String -> VersionConfig a
storeVersionConfig name hash = (namedVersionConfig name hash)
    { vcIgnore = S.fromList
        [ "Data.Vector.Unboxed.Base.Vector GHC.Types.Word"
        , "Data.ByteString.Internal.ByteString"
        ]
    , vcRenames = M.fromList
        [ ( "Data.Maybe.Maybe", "GHC.Base.Maybe") ]
    }
