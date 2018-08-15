-- Explicitly disabling due to external code {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
-- Taken from
-- https://github.com/well-typed/hackage-security/tree/master/hackage-security-http-client
-- to avoid extra dependencies
module Hackage.Security.Client.Repository.HttpLib.HttpClient (
    httpLib
  ) where

import Control.Exception
import Control.Monad (void)
import Data.ByteString (ByteString)
import Network.URI
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BS.C8
import qualified Pantry.HTTP as HTTP

import Hackage.Security.Client hiding (Header)
import Hackage.Security.Client.Repository.HttpLib
import Hackage.Security.Util.Checked
import qualified Hackage.Security.Util.Lens as Lens

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

-- | An 'HttpLib' value using the default global manager
httpLib :: HttpLib
httpLib = HttpLib
    { httpGet      = get
    , httpGetRange = getRange
    }

{-------------------------------------------------------------------------------
  Individual methods
-------------------------------------------------------------------------------}

get :: Throws SomeRemoteError
    => [HttpRequestHeader] -> URI
    -> ([HttpResponseHeader] -> BodyReader -> IO a)
    -> IO a
get reqHeaders uri callback = wrapCustomEx $ do
    -- TODO: setUri fails under certain circumstances; in particular, when
    -- the URI contains URL auth. Not sure if this is a concern.
    request' <- HTTP.setUri HTTP.defaultRequest uri
    let request = setRequestHeaders reqHeaders request'
    checkHttpException $ HTTP.withResponse request $ \response -> do
      let br = wrapCustomEx $ HTTP.getResponseBody response
      callback (getResponseHeaders response) br

getRange :: Throws SomeRemoteError
         => [HttpRequestHeader] -> URI -> (Int, Int)
         -> (HttpStatus -> [HttpResponseHeader] -> BodyReader -> IO a)
         -> IO a
getRange reqHeaders uri (from, to) callback = wrapCustomEx $ do
    request' <- HTTP.setUri HTTP.defaultRequest uri
    let request = setRange from to
                $ setRequestHeaders reqHeaders request'
    checkHttpException $ HTTP.withResponse request $ \response -> do
      let br = wrapCustomEx $ HTTP.getResponseBody response
      case () of
         () | HTTP.getResponseStatus response == HTTP.partialContent206 ->
           callback HttpStatus206PartialContent (getResponseHeaders response) br
         () | HTTP.getResponseStatus response == HTTP.ok200 ->
           callback HttpStatus200OK (getResponseHeaders response) br
         _otherwise ->
           throwChecked $ HTTP.HttpExceptionRequest request
                        $ HTTP.StatusCodeException (void response) ""

-- | Wrap custom exceptions
--
-- NOTE: The only other exception defined in @http-client@ is @TimeoutTriggered@
-- but it is currently disabled <https://github.com/snoyberg/http-client/issues/116>
wrapCustomEx :: (Throws HTTP.HttpException => IO a)
             -> (Throws SomeRemoteError => IO a)
wrapCustomEx act = handleChecked (\(ex :: HTTP.HttpException) -> go ex) act
  where
    go ex = throwChecked (SomeRemoteError ex)

checkHttpException :: Throws HTTP.HttpException => IO a -> IO a
checkHttpException = handle $ \(ex :: HTTP.HttpException) ->
                       throwChecked ex

{-------------------------------------------------------------------------------
  http-client auxiliary
-------------------------------------------------------------------------------}

hAcceptRanges :: HTTP.HeaderName
hAcceptRanges = "Accept-Ranges"

hAcceptEncoding :: HTTP.HeaderName
hAcceptEncoding = "Accept-Encoding"

setRange :: Int -> Int
         -> HTTP.Request -> HTTP.Request
setRange from to =
    HTTP.addRequestHeader HTTP.hRange rangeHeader
  where
    -- Content-Range header uses inclusive rather than exclusive bounds
    -- See <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html>
    rangeHeader = BS.C8.pack $ "bytes=" ++ show from ++ "-" ++ show (to - 1)

-- | Set request headers
setRequestHeaders :: [HttpRequestHeader]
                  -> HTTP.Request -> HTTP.Request
setRequestHeaders opts =
    HTTP.setRequestHeaders (trOpt disallowCompressionByDefault opts)
  where
    trOpt :: [(HTTP.HeaderName, [ByteString])]
          -> [HttpRequestHeader]
          -> [HTTP.Header]
    trOpt acc [] =
      concatMap finalizeHeader acc
    trOpt acc (HttpRequestMaxAge0:os) =
      trOpt (insert HTTP.hCacheControl ["max-age=0"] acc) os
    trOpt acc (HttpRequestNoTransform:os) =
      trOpt (insert HTTP.hCacheControl ["no-transform"] acc) os

    -- disable content compression (potential security issue)
    disallowCompressionByDefault :: [(HTTP.HeaderName, [ByteString])]
    disallowCompressionByDefault = [(hAcceptEncoding, [])]

    -- Some headers are comma-separated, others need multiple headers for
    -- multiple options.
    --
    -- TODO: Right we we just comma-separate all of them.
    finalizeHeader :: (HTTP.HeaderName, [ByteString])
                   -> [HTTP.Header]
    finalizeHeader (name, strs) = [(name, BS.intercalate ", " (reverse strs))]

    insert :: Eq a => a -> [b] -> [(a, [b])] -> [(a, [b])]
    insert x y = Lens.modify (Lens.lookupM x) (++ y)

-- | Extract the response headers
getResponseHeaders :: HTTP.Response a -> [HttpResponseHeader]
getResponseHeaders response = concat [
      [ HttpResponseAcceptRangesBytes
      | (hAcceptRanges, "bytes") `elem` headers
      ]
    ]
  where
    headers = HTTP.getResponseHeaders response
