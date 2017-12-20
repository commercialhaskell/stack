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

import           Control.Monad.Catch (MonadMask)
import           Data.Aeson (FromJSON)
import qualified Data.ByteString as Strict
import           Data.ByteString.Lazy (ByteString)
import           Data.Conduit (ConduitM, Sink)
import qualified Network.HTTP.Client
import           Network.HTTP.Client (BodyReader, Manager, Request, Response)
import           Network.HTTP.Simple (setRequestHeader)
import qualified Network.HTTP.Simple
import           UnliftIO (MonadIO)


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


httpSink :: (MonadIO m, MonadMask m) => Request -> (Response () -> Sink Strict.ByteString m a) -> m a
httpSink = Network.HTTP.Simple.httpSink . setUserAgent


withResponse
  :: (MonadIO m, MonadMask m, MonadIO n)
  => Request -> (Response (ConduitM i Strict.ByteString n ()) -> m a) -> m a
withResponse = Network.HTTP.Simple.withResponse . setUserAgent


withResponseByManager :: Request -> Manager -> (Response BodyReader -> IO a) -> IO a
withResponseByManager = Network.HTTP.Client.withResponse . setUserAgent
