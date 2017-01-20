{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Taken from
-- https://github.com/well-typed/hackage-security/tree/master/hackage-security-http-client
-- to avoid extra dependencies
module Hackage.Security.Client.Repository.HttpLib.HttpClient (
    withClient
  , makeHttpLib
    -- ** Re-exports
  , Manager -- opaque
  ) where

import Control.Exception
import Control.Monad (void)
import Data.ByteString (ByteString)
import Network.URI
import Network.HTTP.Client (Manager)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BS.C8
import qualified Network.HTTP.Client          as HttpClient
import qualified Network.HTTP.Client.Internal as HttpClient
import qualified Network.HTTP.Types           as HttpClient

import Hackage.Security.Client hiding (Header)
import Hackage.Security.Client.Repository.HttpLib
import Hackage.Security.Util.Checked
import qualified Hackage.Security.Util.Lens as Lens

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

-- | Initialization
--
-- The proxy must be specified at initialization because @http-client@ does not
-- allow to change the proxy once the 'Manager' is created.
withClient :: ProxyConfig HttpClient.Proxy -> (Manager -> HttpLib -> IO a) -> IO a
withClient proxyConfig callback = do
    manager <- HttpClient.newManager (setProxy HttpClient.defaultManagerSettings)
    callback manager $ makeHttpLib manager
  where
    setProxy = HttpClient.managerSetProxy $
      case proxyConfig of
        ProxyConfigNone  -> HttpClient.noProxy
        ProxyConfigUse p -> HttpClient.useProxy p
        ProxyConfigAuto  -> HttpClient.proxyEnvironment Nothing

-- | Create an 'HttpLib' value from a preexisting 'Manager'.
makeHttpLib :: Manager -> HttpLib
makeHttpLib manager = HttpLib
    { httpGet      = get      manager
    , httpGetRange = getRange manager
    }

{-------------------------------------------------------------------------------
  Individual methods
-------------------------------------------------------------------------------}

get :: Throws SomeRemoteError
    => Manager
    -> [HttpRequestHeader] -> URI
    -> ([HttpResponseHeader] -> BodyReader -> IO a)
    -> IO a
get manager reqHeaders uri callback = wrapCustomEx $ do
    -- TODO: setUri fails under certain circumstances; in particular, when
    -- the URI contains URL auth. Not sure if this is a concern.
    request' <- HttpClient.setUri HttpClient.defaultRequest uri
    let request = setRequestHeaders reqHeaders request'
    checkHttpException $ HttpClient.withResponse request manager $ \response -> do
      let br = wrapCustomEx $ HttpClient.responseBody response
      callback (getResponseHeaders response) br

getRange :: Throws SomeRemoteError
         => Manager
         -> [HttpRequestHeader] -> URI -> (Int, Int)
         -> (HttpStatus -> [HttpResponseHeader] -> BodyReader -> IO a)
         -> IO a
getRange manager reqHeaders uri (from, to) callback = wrapCustomEx $ do
    request' <- HttpClient.setUri HttpClient.defaultRequest uri
    let request = setRange from to
                $ setRequestHeaders reqHeaders request'
    checkHttpException $ HttpClient.withResponse request manager $ \response -> do
      let br = wrapCustomEx $ HttpClient.responseBody response
      case () of
         () | HttpClient.responseStatus response == HttpClient.partialContent206 ->
           callback HttpStatus206PartialContent (getResponseHeaders response) br
         () | HttpClient.responseStatus response == HttpClient.ok200 ->
           callback HttpStatus200OK (getResponseHeaders response) br
         _otherwise ->
           throwChecked $ HttpClient.HttpExceptionRequest request
                        $ HttpClient.StatusCodeException (void response) ""

-- | Wrap custom exceptions
--
-- NOTE: The only other exception defined in @http-client@ is @TimeoutTriggered@
-- but it is currently disabled <https://github.com/snoyberg/http-client/issues/116>
wrapCustomEx :: (Throws HttpClient.HttpException => IO a)
             -> (Throws SomeRemoteError => IO a)
wrapCustomEx act = handleChecked (\(ex :: HttpClient.HttpException) -> go ex) act
  where
    go ex = throwChecked (SomeRemoteError ex)

checkHttpException :: Throws HttpClient.HttpException => IO a -> IO a
checkHttpException = handle $ \(ex :: HttpClient.HttpException) ->
                       throwChecked ex

{-------------------------------------------------------------------------------
  http-client auxiliary
-------------------------------------------------------------------------------}

hAcceptRanges :: HttpClient.HeaderName
hAcceptRanges = "Accept-Ranges"

hAcceptEncoding :: HttpClient.HeaderName
hAcceptEncoding = "Accept-Encoding"

setRange :: Int -> Int
         -> HttpClient.Request -> HttpClient.Request
setRange from to req = req {
      HttpClient.requestHeaders = (HttpClient.hRange, rangeHeader)
                                : HttpClient.requestHeaders req
    }
  where
    -- Content-Range header uses inclusive rather than exclusive bounds
    -- See <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html>
    rangeHeader = BS.C8.pack $ "bytes=" ++ show from ++ "-" ++ show (to - 1)

-- | Set request headers
setRequestHeaders :: [HttpRequestHeader]
                  -> HttpClient.Request -> HttpClient.Request
setRequestHeaders opts req = req {
      HttpClient.requestHeaders = trOpt disallowCompressionByDefault opts
    }
  where
    trOpt :: [(HttpClient.HeaderName, [ByteString])]
          -> [HttpRequestHeader]
          -> [HttpClient.Header]
    trOpt acc [] =
      concatMap finalizeHeader acc
    trOpt acc (HttpRequestMaxAge0:os) =
      trOpt (insert HttpClient.hCacheControl ["max-age=0"] acc) os
    trOpt acc (HttpRequestNoTransform:os) =
      trOpt (insert HttpClient.hCacheControl ["no-transform"] acc) os

    -- disable content compression (potential security issue)
    disallowCompressionByDefault :: [(HttpClient.HeaderName, [ByteString])]
    disallowCompressionByDefault = [(hAcceptEncoding, [])]

    -- Some headers are comma-separated, others need multiple headers for
    -- multiple options.
    --
    -- TODO: Right we we just comma-separate all of them.
    finalizeHeader :: (HttpClient.HeaderName, [ByteString])
                   -> [HttpClient.Header]
    finalizeHeader (name, strs) = [(name, BS.intercalate ", " (reverse strs))]

    insert :: Eq a => a -> [b] -> [(a, [b])] -> [(a, [b])]
    insert x y = Lens.modify (Lens.lookupM x) (++ y)

-- | Extract the response headers
getResponseHeaders :: HttpClient.Response a -> [HttpResponseHeader]
getResponseHeaders response = concat [
      [ HttpResponseAcceptRangesBytes
      | (hAcceptRanges, "bytes") `elem` headers
      ]
    ]
  where
    headers = HttpClient.responseHeaders response
