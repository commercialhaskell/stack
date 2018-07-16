-- Explicitly disabling due to external code {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Taken from
-- https://github.com/well-typed/hackage-security/tree/master/hackage-security-http-client
-- to avoid extra dependencies
module Hackage.Security.Client.Repository.HttpLib.HttpClient (
    makeHttpLib
    -- ** Re-exports
  , Manager -- opaque
  ) where

import Control.Exception
import Control.Monad (void)
import Data.ByteString (ByteString)
import Network.URI
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BS.C8
import           Network.HTTP.StackClient (Manager)
import qualified Network.HTTP.StackClient     as StackClient

import Hackage.Security.Client hiding (Header)
import Hackage.Security.Client.Repository.HttpLib
import Hackage.Security.Util.Checked
import qualified Hackage.Security.Util.Lens as Lens

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

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
    request' <- StackClient.setUri StackClient.defaultRequest uri
    let request = setRequestHeaders reqHeaders request'
    checkHttpException $ StackClient.withResponseByManager request manager $ \response -> do
      let br = wrapCustomEx $ StackClient.responseBody response
      callback (getResponseHeaders response) br

getRange :: Throws SomeRemoteError
         => Manager
         -> [HttpRequestHeader] -> URI -> (Int, Int)
         -> (HttpStatus -> [HttpResponseHeader] -> BodyReader -> IO a)
         -> IO a
getRange manager reqHeaders uri (from, to) callback = wrapCustomEx $ do
    request' <- StackClient.setUri StackClient.defaultRequest uri
    let request = setRange from to
                $ setRequestHeaders reqHeaders request'
    checkHttpException $ StackClient.withResponseByManager request manager $ \response -> do
      let br = wrapCustomEx $ StackClient.responseBody response
      case () of
         () | StackClient.responseStatus response == StackClient.partialContent206 ->
           callback HttpStatus206PartialContent (getResponseHeaders response) br
         () | StackClient.responseStatus response == StackClient.ok200 ->
           callback HttpStatus200OK (getResponseHeaders response) br
         _otherwise ->
           throwChecked $ StackClient.HttpExceptionRequest request
                        $ StackClient.StatusCodeException (void response) ""

-- | Wrap custom exceptions
--
-- NOTE: The only other exception defined in @http-client@ is @TimeoutTriggered@
-- but it is currently disabled <https://github.com/snoyberg/http-client/issues/116>
wrapCustomEx :: (Throws StackClient.HttpException => IO a)
             -> (Throws SomeRemoteError => IO a)
wrapCustomEx act = handleChecked (\(ex :: StackClient.HttpException) -> go ex) act
  where
    go ex = throwChecked (SomeRemoteError ex)

checkHttpException :: Throws StackClient.HttpException => IO a -> IO a
checkHttpException = handle $ \(ex :: StackClient.HttpException) ->
                       throwChecked ex

{-------------------------------------------------------------------------------
  http-client auxiliary
-------------------------------------------------------------------------------}

hAcceptRanges :: StackClient.HeaderName
hAcceptRanges = "Accept-Ranges"

hAcceptEncoding :: StackClient.HeaderName
hAcceptEncoding = "Accept-Encoding"

setRange :: Int -> Int
         -> StackClient.Request -> StackClient.Request
setRange from to req = req {
      StackClient.requestHeaders = (StackClient.hRange, rangeHeader)
                                : StackClient.requestHeaders req
    }
  where
    -- Content-Range header uses inclusive rather than exclusive bounds
    -- See <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html>
    rangeHeader = BS.C8.pack $ "bytes=" ++ show from ++ "-" ++ show (to - 1)

-- | Set request headers
setRequestHeaders :: [HttpRequestHeader]
                  -> StackClient.Request -> StackClient.Request
setRequestHeaders opts req = req {
      StackClient.requestHeaders = trOpt disallowCompressionByDefault opts
    }
  where
    trOpt :: [(StackClient.HeaderName, [ByteString])]
          -> [HttpRequestHeader]
          -> [StackClient.Header]
    trOpt acc [] =
      concatMap finalizeHeader acc
    trOpt acc (HttpRequestMaxAge0:os) =
      trOpt (insert StackClient.hCacheControl ["max-age=0"] acc) os
    trOpt acc (HttpRequestNoTransform:os) =
      trOpt (insert StackClient.hCacheControl ["no-transform"] acc) os

    -- disable content compression (potential security issue)
    disallowCompressionByDefault :: [(StackClient.HeaderName, [ByteString])]
    disallowCompressionByDefault = [(hAcceptEncoding, [])]

    -- Some headers are comma-separated, others need multiple headers for
    -- multiple options.
    --
    -- TODO: Right we we just comma-separate all of them.
    finalizeHeader :: (StackClient.HeaderName, [ByteString])
                   -> [StackClient.Header]
    finalizeHeader (name, strs) = [(name, BS.intercalate ", " (reverse strs))]

    insert :: Eq a => a -> [b] -> [(a, [b])] -> [(a, [b])]
    insert x y = Lens.modify (Lens.lookupM x) (++ y)

-- | Extract the response headers
getResponseHeaders :: StackClient.Response a -> [HttpResponseHeader]
getResponseHeaders response = concat [
      [ HttpResponseAcceptRangesBytes
      | (hAcceptRanges, "bytes") `elem` headers
      ]
    ]
  where
    headers = StackClient.responseHeaders response
