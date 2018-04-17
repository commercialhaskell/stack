{-# LANGUAGE OverloadedStrings #-}

-- |
-- Wrapper functions of 'Network.HTTP.Simple' and 'Network.HTTP.Client' to
-- add the 'User-Agent' HTTP request header to each request.

module Network.HTTP.StackClient
  ( httpJSON
  , httpLbs
  , httpLBS
  , httpNoBody
  , httpSink
  , setUserAgent
  , withResponse
  , withResponseByManager
  ) where

import           Data.Aeson (FromJSON)
import qualified Data.ByteString as Strict
import           Data.ByteString.Lazy (ByteString)
import           Data.Conduit (ConduitM, transPipe)
import           Data.Void (Void)
import qualified Network.HTTP.Client
import           Network.HTTP.Client (BodyReader, Manager, Request, Response)
import           Network.HTTP.Simple (setRequestHeader)
import qualified Network.HTTP.Simple
import           UnliftIO (MonadIO, MonadUnliftIO, withRunInIO, withUnliftIO, unliftIO)


setUserAgent :: Request -> Request
setUserAgent = setRequestHeader "User-Agent" ["The Haskell Stack"]


httpJSON :: (MonadIO m, FromJSON a) => Request -> m (Response a)
httpJSON = Network.HTTP.Simple.httpJSON . setUserAgent


httpLbs :: MonadIO m => Request -> m (Response ByteString)
httpLbs = Network.HTTP.Simple.httpLbs . setUserAgent


httpLBS :: MonadIO m => Request -> m (Response ByteString)
httpLBS = httpLbs


httpNoBody :: MonadIO m => Request -> m (Response ())
httpNoBody = Network.HTTP.Simple.httpNoBody . setUserAgent


httpSink
  :: MonadUnliftIO m
  => Request
  -> (Response () -> ConduitM Strict.ByteString Void m a)
  -> m a
httpSink req inner = withUnliftIO $ \u ->
  Network.HTTP.Simple.httpSink (setUserAgent req) (transPipe (unliftIO u) . inner)


withResponse
  :: (MonadUnliftIO m, MonadIO n)
  => Request -> (Response (ConduitM i Strict.ByteString n ()) -> m a) -> m a
withResponse req inner = withRunInIO $ \run ->
  Network.HTTP.Simple.withResponse (setUserAgent req) (run . inner)


withResponseByManager :: MonadUnliftIO m => Request -> Manager -> (Response BodyReader -> m a) -> m a
withResponseByManager req man inner = withRunInIO $ \run ->
  Network.HTTP.Client.withResponse (setUserAgent req) man (run . inner)
