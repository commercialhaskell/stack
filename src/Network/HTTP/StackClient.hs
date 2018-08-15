{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Wrapper functions of 'Network.HTTP.Simple' and 'Network.HTTP.Client' to
-- add the 'User-Agent' HTTP request header to each request.

module Network.HTTP.StackClient
  ( httpJSON
  , httpLbs
  , httpNoBody
  , httpSink
  , withResponse
  , setRequestMethod
  , setRequestHeader
  , addRequestHeader
  , setRequestBody
  , getResponseHeaders
  , getResponseBody
  , getResponseStatusCode
  , parseRequest
  , getUri
  , path
  , checkResponse
  , parseUrlThrow
  , requestHeaders
  , getGlobalManager
  , applyDigestAuth
  , displayDigestAuthException
  , Request
  , RequestBody(RequestBodyBS, RequestBodyLBS)
  , Response
  , HttpException
  , hAccept
  , hContentLength
  , hContentMD5
  , methodPut
  , formDataBody
  , partFileRequestBody
  , partBS
  , partLBS
  ) where

import           Data.Aeson (FromJSON)
import qualified Data.ByteString as Strict
import           Data.Conduit (ConduitM)
import           Data.Void (Void)
import           Network.HTTP.Client (Request, RequestBody(..), Response, parseRequest, getUri, path, checkResponse, parseUrlThrow)
import           Network.HTTP.Simple (setRequestMethod, setRequestBody, setRequestHeader, addRequestHeader, HttpException(..), getResponseBody, getResponseStatusCode, getResponseHeaders)
import           Network.HTTP.Types (hAccept, hContentLength, hContentMD5, methodPut)
import           Network.HTTP.Conduit (requestHeaders)
import           Network.HTTP.Client.TLS (getGlobalManager, applyDigestAuth, displayDigestAuthException)
import qualified Network.HTTP.Simple
import           Network.HTTP.Client.MultipartFormData (formDataBody, partFileRequestBody, partBS, partLBS)
import           RIO


setUserAgent :: Request -> Request
setUserAgent = setRequestHeader "User-Agent" ["The Haskell Stack"]


httpJSON :: (MonadIO m, FromJSON a) => Request -> m (Response a)
httpJSON = Network.HTTP.Simple.httpJSON . setUserAgent


httpLbs :: MonadIO m => Request -> m (Response LByteString)
httpLbs = Network.HTTP.Simple.httpLbs . setUserAgent


httpNoBody :: MonadIO m => Request -> m (Response ())
httpNoBody = Network.HTTP.Simple.httpNoBody . setUserAgent


httpSink
  :: MonadUnliftIO m
  => Request
  -> (Response () -> ConduitM Strict.ByteString Void m a)
  -> m a
httpSink = Network.HTTP.Simple.httpSink . setUserAgent


withResponse
  :: (MonadUnliftIO m, MonadIO n)
  => Request -> (Response (ConduitM i Strict.ByteString n ()) -> m a) -> m a
withResponse = Network.HTTP.Simple.withResponse . setUserAgent
