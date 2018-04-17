{-# LANGUAGE NoImplicitPrelude #-}
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

import Stack.Prelude
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Store
import Data.Store.Core (unsafeEncodeWith)
import Data.Store.Version
import Language.Haskell.TH
import Path
import Path.IO (ensureDir)

versionedEncodeFile :: Data a => VersionConfig a -> Q Exp
versionedEncodeFile vc = [e| storeEncodeFile $(encodeWithVersionQ vc) $(decodeWithVersionQ vc) |]

versionedDecodeOrLoad :: Data a => VersionConfig a -> Q Exp
versionedDecodeOrLoad vc = [| versionedDecodeOrLoadImpl $(encodeWithVersionQ vc) $(decodeWithVersionQ vc) |]

versionedDecodeFile :: Data a => VersionConfig a -> Q Exp
versionedDecodeFile vc = [e| versionedDecodeFileImpl $(decodeWithVersionQ vc) |]

-- | Write to the given file.
storeEncodeFile :: (Store a, MonadIO m, MonadReader env m, HasCallStack, HasLogFunc env, Eq a)
                => (a -> (Int, Poke ()))
                -> Peek a
                -> Path Abs File
                -> a
                -> m ()
storeEncodeFile pokeFunc peekFunc fp x = do
    let fpt = fromString (toFilePath fp)
    logDebug $ "Encoding " <> fpt
    ensureDir (parent fp)
    let (sz, poker) = pokeFunc x
        encoded = unsafeEncodeWith poker sz
    assert (decodeExWith peekFunc encoded == x) $ liftIO $ BS.writeFile (toFilePath fp) encoded
    logDebug $ "Finished writing " <> fpt

-- | Read from the given file. If the read fails, run the given action and
-- write that back to the file. Always starts the file off with the
-- version tag.
versionedDecodeOrLoadImpl :: (Store a, Eq a, MonadUnliftIO m, MonadReader env m, HasCallStack, HasLogFunc env)
                          => (a -> (Int, Poke ()))
                          -> Peek a
                          -> Path Abs File
                          -> m a
                          -> m a
versionedDecodeOrLoadImpl pokeFunc peekFunc fp mx = do
    let fpt = fromString (toFilePath fp)
    logDebug $ "Trying to decode " <> fpt
    mres <- versionedDecodeFileImpl peekFunc fp
    case mres of
        Just x -> do
            logDebug $ "Success decoding " <> fpt
            return x
        _ -> do
            logDebug $ "Failure decoding " <> fpt
            x <- mx
            storeEncodeFile pokeFunc peekFunc fp x
            return x

versionedDecodeFileImpl :: (Store a, MonadUnliftIO m, MonadReader env m, HasCallStack, HasLogFunc env)
                        => Peek a
                        -> Path loc File
                        -> m (Maybe a)
versionedDecodeFileImpl peekFunc fp = do
    mbs <- liftIO (Just <$> BS.readFile (toFilePath fp)) `catch` \(err :: IOException) -> do
        logDebug ("Exception ignored when attempting to load " <> fromString (toFilePath fp) <> ": " <> displayShow err)
        return Nothing
    case mbs of
        Nothing -> return Nothing
        Just bs ->
            liftIO (Just <$> decodeIOWith peekFunc bs) `catch` \(err :: PeekException) -> do
                 let fpt = fromString (toFilePath fp)
                 logDebug ("Error while decoding " <> fpt <> ": " <> displayShow err <> " (this might not be an error, when switching between stack versions)")
                 return Nothing

storeVersionConfig :: String -> String -> VersionConfig a
storeVersionConfig name hash = (namedVersionConfig name hash)
    { vcIgnore = S.fromList
        [ "Data.Vector.Unboxed.Base.Vector GHC.Types.Word"
        , "Data.ByteString.Internal.ByteString"
        ]
    , vcRenames = M.fromList
        [ ( "Data.Maybe.Maybe", "GHC.Base.Maybe")
        , ( "Stack.Types.Compiler.CVActual"
          , "Stack.Types.Compiler.'CVActual"
          )
        , ( "Stack.Types.Compiler.CVWanted"
          , "Stack.Types.Compiler.'CVWanted"
          )
        -- moved in containers 0.5.9.1
        , ( "Data.Map.Internal.Map", "Data.Map.Base.Map")
        , ( "Data.Set.Internal.Set", "Data.Set.Base.Set")
        ]
    }
